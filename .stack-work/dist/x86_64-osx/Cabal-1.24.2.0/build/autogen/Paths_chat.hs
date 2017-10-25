{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_chat (
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

bindir     = "/Users/dylanhobbs/chat/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/bin"
libdir     = "/Users/dylanhobbs/chat/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/lib/x86_64-osx-ghc-8.0.2/chat-0.1.0.0-DkI6kIrlI9sFwbJMVEcbWY"
dynlibdir  = "/Users/dylanhobbs/chat/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/dylanhobbs/chat/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/share/x86_64-osx-ghc-8.0.2/chat-0.1.0.0"
libexecdir = "/Users/dylanhobbs/chat/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/libexec"
sysconfdir = "/Users/dylanhobbs/chat/.stack-work/install/x86_64-osx/lts-9.10/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chat_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chat_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
