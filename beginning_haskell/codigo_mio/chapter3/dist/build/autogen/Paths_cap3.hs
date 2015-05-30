module Paths_cap3 (
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
version = Version [0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/freinn/.cabal/bin"
libdir     = "/home/freinn/.cabal/lib/x86_64-linux-ghc-7.10.1/cap3_KRZ2KRiZFO1I6CabtoRtzG"
datadir    = "/home/freinn/.cabal/share/x86_64-linux-ghc-7.10.1/cap3-0.0.1"
libexecdir = "/home/freinn/.cabal/libexec"
sysconfdir = "/home/freinn/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cap3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cap3_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cap3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cap3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cap3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
