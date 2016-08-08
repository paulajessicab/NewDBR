module Paths_NewDBR (
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
version = Version [0,4] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/paula/.cabal/bin"
libdir     = "/home/paula/.cabal/lib/x86_64-linux-ghc-7.10.3/NewDBR-0.4-DBl3gskjcxXCxTQ4xyfeL9"
datadir    = "/home/paula/.cabal/share/x86_64-linux-ghc-7.10.3/NewDBR-0.4"
libexecdir = "/home/paula/.cabal/libexec"
sysconfdir = "/home/paula/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "NewDBR_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "NewDBR_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "NewDBR_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "NewDBR_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "NewDBR_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
