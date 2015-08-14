{-# LANGUAGE RecordWildCards #-}
module Options where

import System.Console.GetOpt
import System.Info
import qualified Config
import Data.Version (showVersion)
import Paths_dropdox (version)

defaultOptions :: Options
defaultOptions = Options { optShowVersion = False
                         , optKeyName = "default"
                         , optBucket = Nothing
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["version"]
              (NoArg (\opts -> opts{ optShowVersion = True }))
              "Show version info."
          , Option "k" ["keyname"]
              (ReqArg (\s opts -> opts{ optKeyName = s }) "keyname")
              "AWS credential key name."
          , Option "b" ["bucket"]
              (ReqArg (\s opts -> opts{ optBucket = Just s }) "bucket")
              "Destination bucket."
          ]

header :: String
header = "Usage: dropdox [options] MOUNT_POINT"

fullVersion :: String
fullVersion = concat [ programVersion
                     , " ("
                     , "ghc-", Config.cProjectVersion, "-", arch, "-", os
                     , ")"
                     ]

programVersion :: String
programVersion = "dropdox v" ++ showVersion version

data Options = Options { optShowVersion :: Bool
                       , optKeyName     :: String
                       , optBucket      :: Maybe String
                       } deriving (Show, Eq)
