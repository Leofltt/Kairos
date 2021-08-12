{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_Kairos (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,3,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/leofltt/Desktop/Kairos/.stack-work/install/x86_64-osx/4c9ecf5ef6c92c550f92b313d30d9aa0f224c4263ed0313d6a3fc228d3191438/9.0.1/bin"
libdir     = "/Users/leofltt/Desktop/Kairos/.stack-work/install/x86_64-osx/4c9ecf5ef6c92c550f92b313d30d9aa0f224c4263ed0313d6a3fc228d3191438/9.0.1/lib/x86_64-osx-ghc-9.0.1/Kairos-0.3.4-Hwr6pTPmSWl3JXeNkEV1l8-Kairos-test"
dynlibdir  = "/Users/leofltt/Desktop/Kairos/.stack-work/install/x86_64-osx/4c9ecf5ef6c92c550f92b313d30d9aa0f224c4263ed0313d6a3fc228d3191438/9.0.1/lib/x86_64-osx-ghc-9.0.1"
datadir    = "/Users/leofltt/Desktop/Kairos/.stack-work/install/x86_64-osx/4c9ecf5ef6c92c550f92b313d30d9aa0f224c4263ed0313d6a3fc228d3191438/9.0.1/share/x86_64-osx-ghc-9.0.1/Kairos-0.3.4"
libexecdir = "/Users/leofltt/Desktop/Kairos/.stack-work/install/x86_64-osx/4c9ecf5ef6c92c550f92b313d30d9aa0f224c4263ed0313d6a3fc228d3191438/9.0.1/libexec/x86_64-osx-ghc-9.0.1/Kairos-0.3.4"
sysconfdir = "/Users/leofltt/Desktop/Kairos/.stack-work/install/x86_64-osx/4c9ecf5ef6c92c550f92b313d30d9aa0f224c4263ed0313d6a3fc228d3191438/9.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Kairos_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Kairos_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Kairos_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Kairos_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Kairos_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Kairos_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
