{-# LANGUAGE CPP #-}

module System.Environment.XDG.BaseDir
    ( getUserDataDir
    , getUserDataFile
    , getUserConfigDir
    , getUserConfigFile
    , getUserCacheDir
    , getUserCacheFile
    , getSystemDataDirs
    , getSystemDataFiles
    , getSystemConfigDirs
    , getSystemConfigFiles
    , getAllDataDirs
    , getAllDataFiles
    , getAllConfigDirs
    , getAllConfigFiles
    ) where

import Data.Maybe         ( fromMaybe )
import System.FilePath    ( (</>) )
import System.Environment ( getEnvironment, getEnv )
import System.IO.Error    ( try )
import Text.Regex         ( splitRegex, mkRegex, Regex )
import System.Directory   ( getHomeDirectory )
import Control.Monad      ( liftM2 )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

splitDirs :: String -> [String]
splitDirs = splitRegex $ mkRegex ";"

getDefault "XDG_DATA_HOME"   = getEnv "AppData"
getDefault "XDG_CONFIG_HOME" = userRelative $ "Local Settings"
getDefault "XDG_CACHE_HOME"  = userRelative $ "Local Settings" </> "Cache"
getDefault "XDG_DATA_DIRS"   = getEnv "ProgramFiles"
getDefault "XDG_CONFIG_DIRS" = getEnv "ProgramFiles"
getDefault _                 = return ""

#else

splitDirs :: String -> [String]
splitDirs = splitRegex $ mkRegex ":"

getDefault "XDG_DATA_HOME"   = userRelative $ ".local" </> "share"
getDefault "XDG_CONFIG_HOME" = userRelative $ ".config"
getDefault "XDG_CACHE_HOME"  = userRelative $ ".cache"
getDefault "XDG_DATA_DIRS"   = return $ "/usr/local/share:/usr/share"
getDefault "XDG_CONFIG_DIRS" = return $ "/etc/xdg"
getDefault _                 = return $ ""

#endif

-- | Get the directory for user-specific data files.
getUserDataDir      :: String -> IO FilePath
getUserDataDir      = singleDir "XDG_DATA_HOME"
-- | Get the directory for user-specific configuration files.
getUserConfigDir    :: String -> IO FilePath
getUserConfigDir    = singleDir "XDG_CONFIG_HOME"
-- | Get the directory for user-specific cache files.
getUserCacheDir     :: String -> IO FilePath
getUserCacheDir     = singleDir "XDG_CACHE_HOME"
-- | Get a list of the system-wide data directories.
getSystemDataDirs   :: String -> IO [FilePath]
getSystemDataDirs   = multiDirs "XDG_DATA_DIRS"
-- | Get a list of the system-wide configuration directories.
getSystemConfigDirs :: String -> IO [FilePath]
getSystemConfigDirs = multiDirs "XDG_CONFIG_DIRS"
-- | Get a list of all data directories.
getAllDataDirs      :: String -> IO [FilePath]
getAllDataDirs a    = liftM2 (:) (getUserDataDir a) (getSystemDataDirs a)
-- | Get a list of all configuration directories.
getAllConfigDirs    :: String -> IO [FilePath]
getAllConfigDirs a  = liftM2 (:) (getUserConfigDir a) (getSystemConfigDirs a)
-- | Get the path to a specific user data file.
getUserDataFile          :: String -> String -> IO FilePath
getUserDataFile a f      = fmap (</> f) $ getUserDataDir a
-- | Get the path to a specific user configuration file.
getUserConfigFile        :: String -> String -> IO FilePath
getUserConfigFile a f    = fmap (</> f) $ getUserConfigDir a
-- | Get the path to a specific user cache file.
getUserCacheFile         :: String -> String -> IO FilePath
getUserCacheFile a f     = fmap (</> f) $ getUserCacheDir a
-- | Get a list of all paths for a specific system data file.
getSystemDataFiles       :: String -> String -> IO [FilePath]
getSystemDataFiles a f   = fmap (map (</> f)) $ getSystemDataDirs a
-- | Get a list of all paths for a specific system configuration file.
getSystemConfigFiles     :: String -> String -> IO [FilePath]
getSystemConfigFiles a f = fmap (map (</> f)) $ getSystemConfigDirs a
-- | Get a list of all paths for a specific data file.
getAllDataFiles          :: String -> String -> IO [FilePath]
getAllDataFiles a f      = fmap (map (</> f)) $ getAllDataDirs a
-- | Get a list of all paths for a specific configuration file.
getAllConfigFiles        :: String -> String -> IO [FilePath]
getAllConfigFiles a f    = fmap (map (</> f)) $ getAllConfigDirs a

singleDir :: String -> String -> IO FilePath
singleDir key app = envLookup key >>= return . (</> app)

multiDirs :: String -> String -> IO [FilePath]
multiDirs key app = envLookup key >>= return . map (</> app) . splitDirs

envLookup :: String -> IO String
envLookup key = do env <- getEnvironment
                   case lookup key env of
                        Just val -> return val
                        Nothing  -> getDefault key

userRelative :: FilePath -> IO FilePath
userRelative p = getHomeDirectory >>= return . (</> p)
