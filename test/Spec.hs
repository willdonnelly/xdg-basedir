{-# LANGUAGE CPP #-}

module Main where

import Data.List ( isPrefixOf )
import System.Environment ( setEnv, unsetEnv )
import System.Environment.XDG.BaseDir ( getAllDataDirs )
import Test.Hspec ( Spec, hspec, describe, it, shouldSatisfy )

spec :: Spec
spec =
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
    return () -- nothing to test for Windows
#elif MIN_VERSION_base(4,7,0)
    describe "getAllDataDirs" $ do
        it "works when $HOME is set" $ do
            setEnv "HOME" "/atesthome"
            getAllDataDirs "test" >>= (`shouldSatisfy` (isPrefixOf "/atesthome" . head))

        it "works when $HOME isn't set" $ do
            unsetEnv "HOME"
            getAllDataDirs "test" >>= (`shouldSatisfy` ((> 0) . length))
#else
    return () -- don't have (un)setEnv in base <4.7, so can't test
#endif

main :: IO ()
main = hspec spec
