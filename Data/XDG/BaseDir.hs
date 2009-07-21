{-# LANGUAGE CPP #-}

module Data.XDG.BaseDir
    ( getUserDataDir
    , getUserConfigDir
    , getUserCacheDir
    , getSystemDataDirs
    , getSystemConfigDirs
    , getAllDataDirs
    , getAllConfigDirs
    ) where

import Data.Maybe         ( fromMaybe )
import System.FilePath    ( (</>) )
import System.Environment ( getEnvironment )
import System.IO.Error    ( try )
import Text.Regex         ( splitRegex, mkRegex, Regex )
import System.Directory   ( getHomeDirectory )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

defaultDir "XDG_DATA_HOME"   = userRelative $ ".local" </> "share"
defaultDir "XDG_CONFIG_HOME" = userRelative $ ".config"
defaultDir "XDG_CACHE_HOME"  = userRelative $ ".cache"
defaultDir "XDG_DATA_DIRS"   = return $ "/usr/local/share:/usr/share"
defaultDir "XDG_CONFIG_DIRS" = return $ "/etc/xdg"
defaultDir _                 = return $ ""

#else

defaultDir "XDG_DATA_HOME"   = userRelative $ ".local" </> "share"
defaultDir "XDG_CONFIG_HOME" = userRelative $ ".config"
defaultDir "XDG_CACHE_HOME"  = userRelative $ ".cache"
defaultDir "XDG_DATA_DIRS"   = return $ "/usr/local/share:/usr/share"
defaultDir "XDG_CONFIG_DIRS" = return $ "/etc/xdg"
defaultDir _                 = return $ ""

#endif

getUserDataDir      :: String -> IO FilePath
getUserDataDir      = singleDir "XDG_DATA_HOME"

getUserConfigDir    :: String -> IO FilePath
getUserConfigDir    = singleDir "XDG_CONFIG_HOME"

getUserCacheDir     :: String -> IO FilePath
getUserCacheDir     = singleDir "XDG_CACHE_HOME"

getSystemDataDirs   :: String -> IO [FilePath]
getSystemDataDirs   = multiDirs "XDG_DATA_DIRS"

getSystemConfigDirs :: String -> IO [FilePath]
getSystemConfigDirs = multiDirs "XDG_CONFIG_DIRS"

getAllDataDirs      :: String -> IO [FilePath]
getAllDataDirs app = do userDir <- getUserDataDir app
                        sysDirs <- getSystemDataDirs app
                        return $ userDir:sysDirs

getAllConfigDirs    :: String -> IO [FilePath]
getAllConfigDirs app = do userDir <- getUserConfigDir app
                          sysDirs <- getSystemConfigDirs app
                          return $ userDir:sysDirs

singleDir :: String -> String -> IO FilePath
singleDir key app = envLookup key >>= return . (</> app)

multiDirs :: String -> String -> IO [FilePath]
multiDirs key app = envLookup key >>= return . map (</> app) . splitDirs

splitDirs :: String -> [String]
splitDirs = splitRegex $ mkRegex ":"

userRelative :: FilePath -> IO FilePath
userRelative p = getHomeDirectory >>= return . (</> p)

envLookup :: String -> IO String
envLookup key = do env <- getEnvironment
                   case lookup key of
                        Just val = val
                        Nothing  = getDefault key
