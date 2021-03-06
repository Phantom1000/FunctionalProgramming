module Paths_tr (
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

bindir     = "/home/phantom/tr-0.1.0.0/.stack-work/install/x86_64-linux-tinfo6/20e5e82c7d9bfb2c52f8184e014b956a314683272175e19b22f325ff1390aa13/7.10.2/bin"
libdir     = "/home/phantom/tr-0.1.0.0/.stack-work/install/x86_64-linux-tinfo6/20e5e82c7d9bfb2c52f8184e014b956a314683272175e19b22f325ff1390aa13/7.10.2/lib/x86_64-linux-ghc-7.10.2/tr-0.1.0.0-JHneF6tdHeqFJn5wnSxbWA"
datadir    = "/home/phantom/tr-0.1.0.0/.stack-work/install/x86_64-linux-tinfo6/20e5e82c7d9bfb2c52f8184e014b956a314683272175e19b22f325ff1390aa13/7.10.2/share/x86_64-linux-ghc-7.10.2/tr-0.1.0.0"
libexecdir = "/home/phantom/tr-0.1.0.0/.stack-work/install/x86_64-linux-tinfo6/20e5e82c7d9bfb2c52f8184e014b956a314683272175e19b22f325ff1390aa13/7.10.2/libexec"
sysconfdir = "/home/phantom/tr-0.1.0.0/.stack-work/install/x86_64-linux-tinfo6/20e5e82c7d9bfb2c52f8184e014b956a314683272175e19b22f325ff1390aa13/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tr_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
