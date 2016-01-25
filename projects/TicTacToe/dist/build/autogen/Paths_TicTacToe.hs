module Paths_TicTacToe (
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

bindir     = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\TicTacToe-0.1.0.0-BA2IW3a73EGA1k1DFNRA6l"
datadir    = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\TicTacToe-0.1.0.0"
libexecdir = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\TicTacToe-0.1.0.0-BA2IW3a73EGA1k1DFNRA6l"
sysconfdir = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TicTacToe_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TicTacToe_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TicTacToe_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TicTacToe_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TicTacToe_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
