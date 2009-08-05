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

getUserDataDir      = singleDir "XDG_DATA_HOME"
getUserConfigDir    = singleDir "XDG_CONFIG_HOME"
getUserCacheDir     = singleDir "XDG_CACHE_HOME"
getSystemDataDirs   = multiDirs "XDG_DATA_DIRS"
getSystemConfigDirs = multiDirs "XDG_CONFIG_DIRS"
getAllDataDirs a    = liftM2 (:) (getUserDataDir a) (getSystemDataDirs a)
getAllConfigDirs a  = liftM2 (:) (getUserConfigDir a) (getSystemConfigDirs a)

getUserDataFile a f      = fmap (</> f) $ getUserDataDir a
getUserConfigFile a f    = fmap (</> f) $ getUserConfigDir a
getUserCacheFile a f     = fmap (</> f) $ getUserCacheDir a
getSystemDataFiles a f   = fmap (map (</> f)) $ getSystemDataDirs a
getSystemConfigFiles a f = fmap (map (</> f)) $ getSystemConfigDirs a
getAllDataFiles a f      = fmap (map (</> f)) $ getAllDataDirs a
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
