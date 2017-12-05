{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_server (
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
version = Version [0,1,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Paul\\Documents\\comma2\\tripVisualization\\server\\.stack-work\\install\\3573363d\\bin"
libdir     = "C:\\Users\\Paul\\Documents\\comma2\\tripVisualization\\server\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2\\server-0.1.0.1-A87ge1buU3B90FB70286D6"
dynlibdir  = "C:\\Users\\Paul\\Documents\\comma2\\tripVisualization\\server\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Paul\\Documents\\comma2\\tripVisualization\\server\\.stack-work\\install\\3573363d\\share\\x86_64-windows-ghc-8.0.2\\server-0.1.0.1"
libexecdir = "C:\\Users\\Paul\\Documents\\comma2\\tripVisualization\\server\\.stack-work\\install\\3573363d\\libexec"
sysconfdir = "C:\\Users\\Paul\\Documents\\comma2\\tripVisualization\\server\\.stack-work\\install\\3573363d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "server_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "server_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "server_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "server_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "server_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "server_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
