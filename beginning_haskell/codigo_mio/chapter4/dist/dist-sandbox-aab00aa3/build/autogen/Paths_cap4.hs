module Paths_cap4 (
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

bindir     = "/home/freinn/libroshaskell/beginning_haskell/codigo_mio/chapter4/.cabal-sandbox/bin"
libdir     = "/home/freinn/libroshaskell/beginning_haskell/codigo_mio/chapter4/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.1/cap4_Edv5b1OFqxkK4IKQqt33qF"
datadir    = "/home/freinn/libroshaskell/beginning_haskell/codigo_mio/chapter4/.cabal-sandbox/share/x86_64-linux-ghc-7.10.1/cap4-0.1.0.0"
libexecdir = "/home/freinn/libroshaskell/beginning_haskell/codigo_mio/chapter4/.cabal-sandbox/libexec"
sysconfdir = "/home/freinn/libroshaskell/beginning_haskell/codigo_mio/chapter4/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cap4_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cap4_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "cap4_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cap4_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cap4_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
