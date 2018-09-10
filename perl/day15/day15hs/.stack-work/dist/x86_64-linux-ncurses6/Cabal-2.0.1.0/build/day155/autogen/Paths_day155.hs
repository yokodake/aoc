{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_day155 (
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

bindir     = "/home/ngyj/code/perl/aoc17/day155/.stack-work/install/x86_64-linux-ncurses6/lts-11.13/8.2.2/bin"
libdir     = "/home/ngyj/code/perl/aoc17/day155/.stack-work/install/x86_64-linux-ncurses6/lts-11.13/8.2.2/lib/x86_64-linux-ghc-8.2.2/day155-0.1.0.0-AVumkgidnwd7XBzhN8MxeQ-day155"
dynlibdir  = "/home/ngyj/code/perl/aoc17/day155/.stack-work/install/x86_64-linux-ncurses6/lts-11.13/8.2.2/lib/x86_64-linux-ghc-8.2.2"
datadir    = "/home/ngyj/code/perl/aoc17/day155/.stack-work/install/x86_64-linux-ncurses6/lts-11.13/8.2.2/share/x86_64-linux-ghc-8.2.2/day155-0.1.0.0"
libexecdir = "/home/ngyj/code/perl/aoc17/day155/.stack-work/install/x86_64-linux-ncurses6/lts-11.13/8.2.2/libexec/x86_64-linux-ghc-8.2.2/day155-0.1.0.0"
sysconfdir = "/home/ngyj/code/perl/aoc17/day155/.stack-work/install/x86_64-linux-ncurses6/lts-11.13/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "day155_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "day155_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "day155_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "day155_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "day155_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "day155_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
