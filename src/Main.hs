{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Main where

import Options
import System
import Aws
import Aws.S3
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Exception
import Control.Concurrent.MVar
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.FilePath
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.Posix.Types
import System.Posix.Files
import System.Posix.IO
import System.Fuse
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    args <- getArgs
    case getOpt' Permute options args of
        ([], _, _, []) -> error $ usageInfo header options
        (opts, as, as', []) -> withArgs (as++as') $ start $ foldl (flip id)
                                                            defaultOptions
                                                            opts
        (_, _, _, ms) -> error $ concat ms ++ usageInfo header options

start :: Options -> IO ()
start Options{..} = do
    -- Show version if needed
    when optShowVersion $ putStrLn fullVersion

    bucket <- case optBucket of
        Nothing -> putStrLn "A bucket is required (-b BUCKET)" >> exitFailure
        Just bucket -> return bucket

    -- Load up our "virtual" file system
    efs <- newFileSystem (T.pack bucket) optKeyName
    case efs of
        Right fs -> do
            -- Startup our fuse process
            fuseMain (doxFSOps fs) defaultExceptionHandler

        Left err -> do putStrLn err
                       exitFailure

doxFSOps :: FileSystem -> FuseOperations ()
doxFSOps fs =
    defaultFuseOps { fuseGetFileStat = doxGetFileStat fs
                   --, fuseOpen        = helloOpen
                   --, fuseRead        = helloRead
                   , fuseOpenDirectory = doxOpenDirectory fs
                   , fuseReadDirectory = doxReadDirectory fs
                   , fuseGetFileSystemStats = helloGetFileSystemStats
                   }
doxGetFileStat :: FileSystem -> FilePath -> IO (Either Errno FileStat)
doxGetFileStat _ "/" = getFuseContext >>= return . Right . dirStat
doxGetFileStat FileSystem{..} fp = do
    root <- readMVar fsRoot
    let found = do key <- keyForFile fp root
                   M.lookup key root
    case found of
        Nothing  -> do putStrLn $ "could not find stat on '" ++ fp ++ "'"
                       forM_ (M.keys root) $ putStrLn . ("    " ++)
                       return $ Left eNOENT
        Just Dir -> getFuseContext >>= return . Right . dirStat
        Just (File _) -> getFuseContext >>= return . Right . objectStat

dirStat :: FuseContext -> FileStat
dirStat ctx = do
    FileStat { statEntryType = Directory
             , statFileMode = foldr1 unionFileModes [ ownerReadMode
                                                    , ownerExecuteMode
                                                    , groupReadMode
                                                    , otherReadMode
                                                    ]
             , statLinkCount = 2
             , statFileOwner = fuseCtxUserID ctx
             , statFileGroup = fuseCtxGroupID ctx
             , statSpecialDeviceID = 0
             , statFileSize = 4096
             , statBlocks = 1
             , statAccessTime = 0
             , statModificationTime = 0
             , statStatusChangeTime = 0
             }

objectStat :: FuseContext -> FileStat
objectStat ctx =
    FileStat { statEntryType = RegularFile
             , statFileMode = foldr1 unionFileModes [ ownerReadMode
                                                    , groupReadMode
                                                    , otherReadMode
                                                    ]
             , statLinkCount = 1
             , statFileOwner = fuseCtxUserID ctx
             , statFileGroup = fuseCtxGroupID ctx
             , statSpecialDeviceID = 0
             , statFileSize = 128
             , statBlocks = 1
             , statAccessTime = 0
             , statModificationTime = 0
             , statStatusChangeTime = 0
             }

doxOpenDirectory :: FileSystem -> FilePath -> IO Errno
doxOpenDirectory fs@FileSystem{..} dir = do
    putStrLn $ "opening dir '" ++ dir ++ "'"
    root <- readMVar fsRoot
    let mkey = keyForFile dir root
    case mkey of
        Nothing -> do putStrLn $ "    could not find a key for '" ++ dir ++ "'"
                      return eNOENT
        Just k  -> do files <- getFileNames fs k
                      mapM_ (putStrLn . ("    " ++) . showFile) files
                      return eOK

doxReadDirectory :: FileSystem -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
doxReadDirectory fs@FileSystem{..} dir = do
    putStrLn $ "reading dir '" ++ dir ++ "'"
    root <- readMVar fsRoot
    let mkey = keyForFile dir root
    case mkey of
        Nothing -> do putStrLn $ "    could not find a key for '" ++ dir ++ "'"
                      return $ Left eNOENT
        Just k  -> do putStrLn $ "    found key '" ++ k ++ "'"
                      ctx <- getFuseContext
                      files <- filesWithPrefix fs k
                      files' <- forM (M.toList files) $ \(key, t) -> do
                          let name = firstFileAfter key k
                          putStrLn $ "    " ++ show name ++ " (" ++ key ++ ")" ++ "( " ++ k ++ " )"
                          return $ ((,stat ctx t) . unFileName) <$> name
                      return $ Right $ catMaybes files'
            where stat ctx Dir = dirStat ctx
                  stat ctx (File _) = objectStat ctx


helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
  return $ Right $ FileSystemStats
    { fsStatBlockSize = 512
    , fsStatBlockCount = 1
    , fsStatBlocksFree = 1
    , fsStatBlocksAvailable = 1
    , fsStatFileCount = 5
    , fsStatFilesFree = 10
    , fsStatMaxNameLength = 255
    }

keyForFile :: FilePath -> FileMap -> Maybe FilePath
keyForFile "/" _ = Just "/"
keyForFile ('/':fp) fm = keyForFile fp fm
keyForFile fp fm = msum $ map f looks
    where looks = [fp, fp ++ "/"]
          f k = if M.member k fm then Just k else Nothing
--------------------------------------------------------------------------------
-- Basic ops unrelated to fuse
--------------------------------------------------------------------------------
newFileSystem :: Bucket -> String -> IO (Either String FileSystem)
newFileSystem b credKey = do
    mcreds <- loadAwsCreds $ T.pack credKey
    case mcreds of
        Nothing -> return $ Left $ concat [ "Could not find any AWS creds :(\n"
                                          , "Please add creds to this directory "
                                          , "or another parent directory."
                                          ]
        Just fc -> do m <- newManager tlsManagerSettings
                      let c = snd fc
                      mvar <- newMVar mempty
                      return $ Right $ FileSystem mvar m c b

-- | Return the keys of a directory in the filesystem in a way that's
-- appropriate for a directory listing.
getFileNames :: FileSystem -> FilePath -> IO [FileName]
getFileNames fs dir =
    getDirByKey fs dir >>= loadFileMap fs >>= flip listFileNames dir

listFileNames :: FileSystem -> FilePath -> IO [FileName]
listFileNames fs dir = flip fileNames dir <$> filesWithPrefix fs dir

fileNames :: FileMap -> FilePath -> [FileName]
fileNames fm dir =
    let dir' = if dir == "/" then "" else dir
        ks = M.keys $ M.filterWithKey (\k _ -> dir' `isPrefixOf` k) fm
    in catMaybes $ map (`firstFileAfter` dir') ks

firstFileAfter :: String -> FilePath -> Maybe FileName
firstFileAfter s "/" = firstFileAfter s ""
firstFileAfter s fp =
    if fp `isPrefixOf` s
    then case splitPath $ drop (length fp) s of
        []  -> Nothing
        f:_ -> Just $ strFile f
    else Nothing

filesWithPrefix :: FileSystem -> FilePath -> IO FileMap
filesWithPrefix FileSystem{..} dir = do
    fm <- readMVar fsRoot
    let dir' = if dir == "/" then "" else dir
    return $ M.filterWithKey (\k _ -> dir' `isPrefixOf` k) fm

allKeys :: FileSystem -> IO [FilePath]
allKeys FileSystem{..} = M.keys <$> readMVar fsRoot

getDirByKey :: FileSystem -> FilePath -> IO FileMap
getDirByKey FileSystem{..} dir = do
    let (m,c) = (fsMngr, fsCred)
        mdir  = if dir == "/" || dir == "" then Nothing else Just $ T.pack dir
    r <- runS3 m c $ (getBucket fsBucket) { gbDelimiter = Just "/"
                                          , gbPrefix = mdir
                                          }
    case r of
        Left err  -> do putStrLn $ show err
                        return mempty
        Right gbr -> return $ toFileMap gbr

loadFileMap :: FileSystem -> FileMap -> IO FileSystem
loadFileMap fs@FileSystem{..} fm' = do
    fm <- takeMVar fsRoot
    putMVar fsRoot $ fm' `M.union` fm
    return fs

toFileMap :: GetBucketResponse -> FileMap
toFileMap GetBucketResponse{..} =
    foldl addPrefix (foldl addObject mempty gbrContents) gbrCommonPrefixes
        where addPrefix m' p = M.insert (T.unpack p) Dir m'
              addObject m' o = M.insert (T.unpack $ objectKey o) (File o) m'

mkDir :: FilePath -> FilePath
mkDir = reverse . dropWhile (== '/') . reverse . dropWhile (== '/')

strFile :: String -> FileName
strFile = FileName . dropWhile (== '/') . takeWhile (/= '/')

showFile :: FileName -> String
showFile = unFileName
--------------------------------------------------------------------------------
-- Our types
--------------------------------------------------------------------------------
data FileSystem = FileSystem { fsRoot :: MVar FileMap
                             , fsMngr :: Manager
                             , fsCred :: Credentials
                             , fsBucket :: Bucket
                             }
type FileMap = Map FilePath FileType
data FileType = File ObjectInfo | Dir deriving (Show)
newtype FileName = FileName { unFileName :: String } deriving (Show)
--------------------------------------------------------------------------------
-- AWS Operations
--------------------------------------------------------------------------------
runS3 :: (Transaction r a, ServiceConfiguration r ~ S3Configuration)
      => Manager -> Credentials -> r -> IO (Either S3Error a)
runS3 mngr creds r =
    let scfg = defServiceConfig :: S3Configuration NormalQuery
        cfg  = Configuration Timestamp creds $ defaultLog Warning
    in try $ runResourceT $ pureAws cfg scfg mngr r
