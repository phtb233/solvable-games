{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_solvable_games (
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

bindir     = "/home/stephen/Documents/git/solvable-games/.stack-work/install/x86_64-linux/18141e4c5c95b53bd7893a4a13d78ead3f5575dc9743c5ec6341ad497cb526fa/8.0.2/bin"
libdir     = "/home/stephen/Documents/git/solvable-games/.stack-work/install/x86_64-linux/18141e4c5c95b53bd7893a4a13d78ead3f5575dc9743c5ec6341ad497cb526fa/8.0.2/lib/x86_64-linux-ghc-8.0.2/solvable-games-0.1.0.0"
dynlibdir  = "/home/stephen/Documents/git/solvable-games/.stack-work/install/x86_64-linux/18141e4c5c95b53bd7893a4a13d78ead3f5575dc9743c5ec6341ad497cb526fa/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/stephen/Documents/git/solvable-games/.stack-work/install/x86_64-linux/18141e4c5c95b53bd7893a4a13d78ead3f5575dc9743c5ec6341ad497cb526fa/8.0.2/share/x86_64-linux-ghc-8.0.2/solvable-games-0.1.0.0"
libexecdir = "/home/stephen/Documents/git/solvable-games/.stack-work/install/x86_64-linux/18141e4c5c95b53bd7893a4a13d78ead3f5575dc9743c5ec6341ad497cb526fa/8.0.2/libexec"
sysconfdir = "/home/stephen/Documents/git/solvable-games/.stack-work/install/x86_64-linux/18141e4c5c95b53bd7893a4a13d78ead3f5575dc9743c5ec6341ad497cb526fa/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "solvable_games_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "solvable_games_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "solvable_games_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "solvable_games_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "solvable_games_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "solvable_games_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
