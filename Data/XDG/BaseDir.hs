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

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

#else

import System.Directory   ( getHomeDirectory )
import Text.Regex         ( splitRegex, mkRegex, Regex )

#endif

getUserDataDir      :: String -> IO FilePath
getUserConfigDir    :: String -> IO FilePath
getUserCacheDir     :: String -> IO FilePath
getSystemDataDirs   :: String -> IO [FilePath]
getSystemConfigDirs :: String -> IO [FilePath]
getAllDataDirs      :: String -> IO [FilePath]
getAllConfigDirs    :: String -> IO [FilePath]

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

getUserDataDir app = undefined
getUserConfigDir app = undefined
getUserCacheDir app = undefined
getSystemDataDirs app = undefined
getSystemConfigDirs app = undefined

#else

getUserDataDir app = getXdgDir app (".local" </> "share") "XDG_DATA_HOME"
getUserConfigDir app = getXdgDir app ".config" "XDG_CONFIG_HOME"
getUserCacheDir app = getXdgDir app ".cache" "XDG_CACHE_HOME"

getSystemDataDirs app = do
    dataDirs <- envLookup "XDG_DATA_DIRS"
    let defaultDirs = "/usr/local/share:/usr/share"
    let dirsList = splitRegex (mkRegex ":") $ fromMaybe defaultDirs dataDirs
    return . map (</> app) $ dirsList

getSystemConfigDirs app = do
    dataDirs <- envLookup "XDG_CONFIG_DIRS"
    let defaultDirs = "/etc/xdg"
    let dirsList = splitRegex (mkRegex ":") $ fromMaybe defaultDirs dataDirs
    return . map (</> app) $ dirsList

getXdgDir :: String -> FilePath -> String -> IO FilePath
getXdgDir app defPath envEntry = do
    homeDir <- getHomeDirectory
    let defaultDir = homeDir </> defPath
    xdgDir <- envLookup envEntry
    return $ fromMaybe defaultDir xdgDir </> app

#endif

getAllDataDirs app = do userDir <- getUserDataDir app
                        sysDirs <- getSystemDataDirs app
                        return $ userDir:sysDirs

getAllConfigDirs app = do userDir <- getUserConfigDir app
                          sysDirs <- getSystemConfigDirs app
                          return $ userDir:sysDirs

envLookup :: String -> IO (Maybe String)
envLookup key = getEnvironment >>= (return . lookup key)
