import Data.XDG.BaseDir

name = "FooBarApp"

showIO a = a >>= print

main = do showIO $ getUserDataDir name
          showIO $ getUserConfigDir name
          showIO $ getUserCacheDir name
          showIO $ getSystemDataDirs name
          showIO $ getSystemConfigDirs name
          showIO $ getAllDataDirs name
          showIO $ getAllConfigDirs name
