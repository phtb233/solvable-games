module Paths_solvable_games (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/stephen/Documents/ComputerScienceMSc/Semester2/Project/games/complete/solvable-games-cabal/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/bin"
libdir     = "/home/stephen/Documents/ComputerScienceMSc/Semester2/Project/games/complete/solvable-games-cabal/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/lib/x86_64-linux-ghc-7.10.3/solvable-games-0.1.0.0-6uGb1izsW2EIjT3XmHfit3"
datadir    = "/home/stephen/Documents/ComputerScienceMSc/Semester2/Project/games/complete/solvable-games-cabal/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/share/x86_64-linux-ghc-7.10.3/solvable-games-0.1.0.0"
libexecdir = "/home/stephen/Documents/ComputerScienceMSc/Semester2/Project/games/complete/solvable-games-cabal/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/libexec"
sysconfdir = "/home/stephen/Documents/ComputerScienceMSc/Semester2/Project/games/complete/solvable-games-cabal/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "solvable_games_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "solvable_games_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "solvable_games_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "solvable_games_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "solvable_games_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
