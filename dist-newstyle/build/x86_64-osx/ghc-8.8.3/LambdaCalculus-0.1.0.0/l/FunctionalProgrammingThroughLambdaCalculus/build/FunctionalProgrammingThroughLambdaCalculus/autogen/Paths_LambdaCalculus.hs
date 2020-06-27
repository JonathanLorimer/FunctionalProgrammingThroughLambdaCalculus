{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_LambdaCalculus (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jonathanlorimer/.cabal/bin"
libdir     = "/Users/jonathanlorimer/.cabal/lib/x86_64-osx-ghc-8.8.3/LambdaCalculus-0.1.0.0-inplace-FunctionalProgrammingThroughLambdaCalculus"
dynlibdir  = "/Users/jonathanlorimer/.cabal/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/jonathanlorimer/.cabal/share/x86_64-osx-ghc-8.8.3/LambdaCalculus-0.1.0.0"
libexecdir = "/Users/jonathanlorimer/.cabal/libexec/x86_64-osx-ghc-8.8.3/LambdaCalculus-0.1.0.0"
sysconfdir = "/Users/jonathanlorimer/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LambdaCalculus_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LambdaCalculus_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "LambdaCalculus_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "LambdaCalculus_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LambdaCalculus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LambdaCalculus_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)