{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Euterpea (
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
version = Version [2,0,7] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\Euterpea-2.0.7-CXOgQczX4spA9r7NG1Ia3Q"
dynlibdir  = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\Euterpea-2.0.7"
libexecdir = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\Euterpea-2.0.7-CXOgQczX4spA9r7NG1Ia3Q\\x86_64-windows-ghc-8.4.3\\Euterpea-2.0.7"
sysconfdir = "C:\\Users\\Admin\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Euterpea_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Euterpea_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Euterpea_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Euterpea_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Euterpea_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Euterpea_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
