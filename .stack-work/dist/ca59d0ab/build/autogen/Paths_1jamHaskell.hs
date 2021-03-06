{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_1jamHaskell (
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

bindir     = "D:\\Engineer\\ProgLang\\Haskell\\1jamHaskell\\.stack-work\\install\\7abedeba\\bin"
libdir     = "D:\\Engineer\\ProgLang\\Haskell\\1jamHaskell\\.stack-work\\install\\7abedeba\\lib\\x86_64-windows-ghc-8.0.2\\1jamHaskell-0.1.0.0-Fw5AgGpdDU34lRL5sfGx5y"
dynlibdir  = "D:\\Engineer\\ProgLang\\Haskell\\1jamHaskell\\.stack-work\\install\\7abedeba\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "D:\\Engineer\\ProgLang\\Haskell\\1jamHaskell\\.stack-work\\install\\7abedeba\\share\\x86_64-windows-ghc-8.0.2\\1jamHaskell-0.1.0.0"
libexecdir = "D:\\Engineer\\ProgLang\\Haskell\\1jamHaskell\\.stack-work\\install\\7abedeba\\libexec"
sysconfdir = "D:\\Engineer\\ProgLang\\Haskell\\1jamHaskell\\.stack-work\\install\\7abedeba\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "1jamHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "1jamHaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "1jamHaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "1jamHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "1jamHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "1jamHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
