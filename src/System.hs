{-# LANGUAGE TupleSections #-}
module System where

import System.Directory
import System.FilePath
import Aws
import Control.Monad
import Data.Text (Text)

-- | Loads the aws creds by keyname from the currenty directory or parent
-- directories.
loadAwsCreds :: Text -> IO (Maybe (FilePath, Credentials))
loadAwsCreds key = do
    dirs   <- getDirectories
    mcreds <- mapM (flip loadCredentialsFromFile key . (</> ".aws-keys")) dirs
    return $ msum $ zipWith (\d m -> (d,) <$> m) dirs mcreds

-- | Returns the current working directory and each parent directory.
getDirectories :: IO [FilePath]
getDirectories = getCurrentDirectory >>= return . subs
    where subs "/" = ["/"]
          subs fp = fp : (subs $ takeDirectory fp)

-- | Checks a file as an absolute path and relative path - if either path
-- is a valid file then returns a Just filepath.
checkFilePath :: FilePath -> IO (Maybe FilePath)
checkFilePath fp = do
    isAbs <- doesFileExist fp
    if isAbs
    then return $ Just fp
    else do
        cwd   <- getCurrentDirectory
        let relfp = cwd </> fp
        isRel <- doesFileExist relfp
        if isRel
        then return $ Just relfp
        else return $ Nothing
