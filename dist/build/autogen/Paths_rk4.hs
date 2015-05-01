module Paths_rk4 (
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

bindir     = "/Users/Sumner/.cabal/bin"
libdir     = "/Users/Sumner/.cabal/lib/x86_64-osx-ghc-7.8.3/rk4-0.1.0.0"
datadir    = "/Users/Sumner/.cabal/share/x86_64-osx-ghc-7.8.3/rk4-0.1.0.0"
libexecdir = "/Users/Sumner/.cabal/libexec"
sysconfdir = "/Users/Sumner/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rk4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rk4_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rk4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rk4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rk4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
