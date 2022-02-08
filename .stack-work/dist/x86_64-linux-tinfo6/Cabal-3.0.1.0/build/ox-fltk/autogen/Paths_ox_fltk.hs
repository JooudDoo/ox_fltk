{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_ox_fltk (
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
version = Version [0,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sega/Documents/ox_fltk/.stack-work/install/x86_64-linux-tinfo6/bdb7e9e39abf2a7c9a6aa992f176d85d88fd63ca8f5947629c7c40bfe7951c8f/8.8.2/bin"
libdir     = "/home/sega/Documents/ox_fltk/.stack-work/install/x86_64-linux-tinfo6/bdb7e9e39abf2a7c9a6aa992f176d85d88fd63ca8f5947629c7c40bfe7951c8f/8.8.2/lib/x86_64-linux-ghc-8.8.2/ox-fltk-0.0.0.1-DAaKkoyBjtg8KEUF91VbYL-ox-fltk"
dynlibdir  = "/home/sega/Documents/ox_fltk/.stack-work/install/x86_64-linux-tinfo6/bdb7e9e39abf2a7c9a6aa992f176d85d88fd63ca8f5947629c7c40bfe7951c8f/8.8.2/lib/x86_64-linux-ghc-8.8.2"
datadir    = "/home/sega/Documents/ox_fltk/.stack-work/install/x86_64-linux-tinfo6/bdb7e9e39abf2a7c9a6aa992f176d85d88fd63ca8f5947629c7c40bfe7951c8f/8.8.2/share/x86_64-linux-ghc-8.8.2/ox-fltk-0.0.0.1"
libexecdir = "/home/sega/Documents/ox_fltk/.stack-work/install/x86_64-linux-tinfo6/bdb7e9e39abf2a7c9a6aa992f176d85d88fd63ca8f5947629c7c40bfe7951c8f/8.8.2/libexec/x86_64-linux-ghc-8.8.2/ox-fltk-0.0.0.1"
sysconfdir = "/home/sega/Documents/ox_fltk/.stack-work/install/x86_64-linux-tinfo6/bdb7e9e39abf2a7c9a6aa992f176d85d88fd63ca8f5947629c7c40bfe7951c8f/8.8.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ox_fltk_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ox_fltk_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "ox_fltk_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "ox_fltk_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ox_fltk_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ox_fltk_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
