{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_brick (
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
version = Version [2,9] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/pritin/.cabal/bin"
libdir     = "/home/pritin/.cabal/lib/x86_64-linux-ghc-8.8.4/brick-2.9-inplace"
dynlibdir  = "/home/pritin/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/pritin/.cabal/share/x86_64-linux-ghc-8.8.4/brick-2.9"
libexecdir = "/home/pritin/.cabal/libexec/x86_64-linux-ghc-8.8.4/brick-2.9"
sysconfdir = "/home/pritin/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "brick_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "brick_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "brick_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "brick_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "brick_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "brick_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
