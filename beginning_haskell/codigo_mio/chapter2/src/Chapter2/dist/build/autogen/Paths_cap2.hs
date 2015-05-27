module Paths_cap2 (
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
libdir     = "/home/freinn/.cabal/lib/x86_64-linux-ghc-7.10.1/cap2_3uYHm9SgIHsCfhIZ9JMLGV"
datadir    = "/home/freinn/.cabal/share/x86_64-linux-ghc-7.10.1/cap2-0.0.1"
libexecdir = "/home/freinn/.cabal/libexec"
sysconfdir = "/home/freinn/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cap2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cap2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cap2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cap2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cap2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
