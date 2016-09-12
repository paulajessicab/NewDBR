module Paths_DBR (
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
version = Version [0,1,5] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/paula/.cabal/bin"
libdir     = "/home/paula/.cabal/lib/x86_64-linux-ghc-7.10.3/DBR-0.1.5-KNyaD8NT5OfGsPatCsauHG"
datadir    = "/home/paula/.cabal/share/x86_64-linux-ghc-7.10.3/DBR-0.1.5"
libexecdir = "/home/paula/.cabal/libexec"
sysconfdir = "/home/paula/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "DBR_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "DBR_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "DBR_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DBR_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DBR_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
