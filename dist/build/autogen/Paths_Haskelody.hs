{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Haskelody (
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

bindir     = "C:\\Users\\Tomek\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Tomek\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\Haskelody-0.1.0.0-G2S4gIlfdKg2CsaA69F7Yq"
dynlibdir  = "C:\\Users\\Tomek\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1"
datadir    = "C:\\Users\\Tomek\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.2.1\\Haskelody-0.1.0.0"
libexecdir = "C:\\Users\\Tomek\\AppData\\Roaming\\cabal\\Haskelody-0.1.0.0-G2S4gIlfdKg2CsaA69F7Yq\\x86_64-windows-ghc-8.2.1\\Haskelody-0.1.0.0"
sysconfdir = "C:\\Users\\Tomek\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Haskelody_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Haskelody_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Haskelody_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Haskelody_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Haskelody_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Haskelody_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
