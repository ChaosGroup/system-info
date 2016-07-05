{-# Language CPP #-}
module System.Information
  ( OS(..), os
  , CPUName, CPUNames, cpuNames
  ) where

#ifdef linux_HOST_OS
import Data.List (isPrefixOf)

#elif mingw32_HOST_OS
import Control.Monad (forM)
import System.Win32.Registry
  ( hKEY_LOCAL_MACHINE
  , regOpenKey, regCloseKey, regQueryInfoKey, regQueryValue
  , subkeys)

#endif


data OS = Linux | Windows | Other
  deriving (Eq, Show)

newtype CPUName = CPUName String
  deriving (Eq, Ord, Show)

type CPUNames = [CPUName]


os :: OS
os =
#ifdef linux_HOST_OS
  Linux
#elif mingw32_HOST_OS
  Windows
#else
  Other
#endif

cpuNames :: IO CPUNames
cpuNames =
#ifdef linux_HOST_OS
  linuxCPUNames
#elif mingw32_HOST_OS
  windowsCPUNames
#else
  pure []
#endif

#ifdef linux_HOST_OS
linuxCPUNames :: IO CPUNames
linuxCPUNames = do
  lines' <- lines <$> readFile "/proc/cpuinfo"
  pure . map (CPUName . unwords . drop 3 . words) $
    filter ("model name" `isPrefixOf`) lines'

#elif mingw32_HOST_OS
windowsCPUNames :: IO CPUNames
windowsCPUNames = do
  cpus <- regOpenKey hKEY_LOCAL_MACHINE "Hardware\\Description\\System\\CentralProcessor"
  n <- subkeys <$> regQueryInfoKey cpus
  res <- forM [0..n-1] $ \i -> do
    cpu <- regOpenKey cpus $ show i
    cpuName <- regQueryValue cpu $ Just "ProcessorNameString"
    regCloseKey cpu
    pure cpuName

  regCloseKey cpus
  pure $ map (CPUName . unwords . words) res

#endif
