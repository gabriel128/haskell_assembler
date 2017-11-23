{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_assembler (
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

bindir     = "/home/gabriel/dev/workspaces/nand2tetris/projects/assembler/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/bin"
libdir     = "/home/gabriel/dev/workspaces/nand2tetris/projects/assembler/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2/assembler-0.1.0.0"
dynlibdir  = "/home/gabriel/dev/workspaces/nand2tetris/projects/assembler/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/gabriel/dev/workspaces/nand2tetris/projects/assembler/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/share/x86_64-linux-ghc-8.0.2/assembler-0.1.0.0"
libexecdir = "/home/gabriel/dev/workspaces/nand2tetris/projects/assembler/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/libexec"
sysconfdir = "/home/gabriel/dev/workspaces/nand2tetris/projects/assembler/.stack-work/install/x86_64-linux-nopie/lts-9.14/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "assembler_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "assembler_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "assembler_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "assembler_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "assembler_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "assembler_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
