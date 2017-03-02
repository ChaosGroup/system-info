{-# Language CPP #-}
{-|
Module      : System.Information
Description : Getting system information
Copyright   : 2016 ChaosGroup
License     : MIT
Maintainer  : daniel.taskoff@chaosgroup.com
Stability   : experimental
Portability : non-portable (GHC extensions)
-}

module System.Information
  (
  -- * OS
    OS(..), os
  -- * CPU
  , CPUName, CPUNames, cpuNames
  , numLogicalCores, LogicalCores(unLogicalCores), CPU, CPUs
  , cpus, showCPUs
  ) where

import Control.Applicative (liftA2)
import Data.List (group, sort)

#ifdef linux_HOST_OS
import Data.List (isPrefixOf)

#elif mingw32_HOST_OS
import Control.Monad (forM)
import System.Win32.Registry
  ( hKEY_LOCAL_MACHINE
  , regOpenKey, regCloseKey, regQueryInfoKey, regQueryValue
  , subkeys)

#endif


-- | A datatype representing the different OSes
--
-- Currenty, only Linux and Windows OSes are recognised
data OS = Linux | Windows | Other
  deriving (Eq, Show)

-- | Get the current OS
os :: OS
os =
#ifdef linux_HOST_OS
  Linux
#elif mingw32_HOST_OS
  Windows
#else
  Other
#endif


-- | A wrapper for a CPU's name
newtype CPUName = CPUName String
  deriving (Eq, Ord)

instance Show CPUName where
  show (CPUName name) = name

type CPUNames = [CPUName]

-- | Number of logical cores
newtype LogicalCores = LogicalCores { unLogicalCores :: Word }
  deriving (Show)
type CPU   = (CPUName, LogicalCores)
type CPUs  = [CPU]


-- | Get the names of the available CPUs
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
-- | Linux specific implementation
linuxCPUNames :: IO CPUNames
linuxCPUNames = do
  lines' <- lines <$> readFile "/proc/cpuinfo"
  pure . map (CPUName . unwords . drop 3 . words) $
    filter ("model name" `isPrefixOf`) lines'

#elif mingw32_HOST_OS
-- | Windows specific implementation
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

-- | Get the number of logical CPU cores
numLogicalCores :: IO LogicalCores
numLogicalCores = LogicalCores . fromIntegral . length <$> cpuNames

-- | Get the names and number of logical cores of the available CPUs
cpus :: IO CPUs
cpus = map (liftA2 (,) head (LogicalCores . fromIntegral . length)) . group . sort <$> cpuNames

-- | Pretty show 'CPUs'
showCPUs :: CPUs -> String
showCPUs = unlines . map (\(CPUName c, n) -> concat
  [c, ", # of logical cores: ", show $ unLogicalCores n])
