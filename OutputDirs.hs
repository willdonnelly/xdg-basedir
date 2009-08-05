import System.Environment.XDG.BaseDir

name = "FooBarApp"
file = "boringFile.ext"

showIO a = a >>= print

main = do showIO $ getUserDataDir name
          showIO $ getUserConfigDir name
          showIO $ getUserCacheDir name
          showIO $ getSystemDataDirs name
          showIO $ getSystemConfigDirs name
          showIO $ getAllDataDirs name
          showIO $ getAllConfigDirs name
          showIO $ getUserDataFile name file
          showIO $ getUserConfigFile name file
          showIO $ getUserCacheFile name file
          showIO $ getSystemDataFiles name file
          showIO $ getSystemConfigFiles name file
          showIO $ getAllDataFiles name file
          showIO $ getAllConfigFiles name file
