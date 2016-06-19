module Paths_Euterpea (
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
version = Version [2,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.3\\Euterpea-2.0.0-FnXzr6DLgKG2ekkusQO6hx"
datadir    = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\i386-windows-ghc-7.10.3\\Euterpea-2.0.0"
libexecdir = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\Euterpea-2.0.0-FnXzr6DLgKG2ekkusQO6hx"
sysconfdir = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Euterpea_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Euterpea_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Euterpea_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Euterpea_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Euterpea_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
