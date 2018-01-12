{-# Language CPP, OverloadedStrings, ScopedTypeVariables #-}
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
    OS, getOS
  -- * CPU
  , CPUName, CPUNames, cpuNames
  , numLogicalCores, LogicalCores(unLogicalCores), CPU, CPUs
  , cpus, showCPUs
  ) where

import Control.Applicative
import Control.Exception (try, SomeException)
import Data.List (group, sort)
import System.Process (readProcess)
import Data.Attoparsec.Text (parse, maybeResult, anyChar, endOfLine, manyTill, space, string)
import Data.Text (Text, pack)

#ifdef darwin_HOST_OS
import Data.Either (either)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

#elif linux_HOST_OS
import Data.List (isPrefixOf)

#elif mingw32_HOST_OS
import Control.Monad (forM)
import Data.List (intercalate)
import System.Win32.Registry
  ( hKEY_LOCAL_MACHINE
  , regOpenKey, regCloseKey, regQueryInfoKey, regQueryValue
  , subkeys)

#endif


-- | A datatype representing different OSes
newtype OS = OS String

instance Show OS where
  show (OS os) = os

-- | Get the current OS' name
getOS :: IO (Maybe OS)
getOS = do
  eResult <- try $ readProcess
#ifdef darwin_HOST_OS
    "sw_vers" [] ""
#elif linux_HOST_OS
    "lsb_release" ["-d"] ""
#elif mingw32_HOST_OS
    "systeminfo" [] ""
#endif

  case eResult of
    Left (_ :: SomeException) ->
      either (const Nothing :: SomeException -> Maybe OS)
             (Just . OS) <$> try (readProcess "tr" ["-d", "\\n"] =<< readProcess "uname" ["-sr"] "")
    Right res -> pure $ OS <$> flip parseLineAfter res
#ifdef darwin_HOST_OS
      "ProductName:"
#elif linux_HOST_OS
      "Description:"
#elif mingw32_HOST_OS
      "OS Name:"
#endif

parseLineAfter :: String -> String -> Maybe String
parseLineAfter separator = maybeResult .
  parse (manyTill anyChar (string (pack separator))  *> many space *> manyTill anyChar endOfLine) . pack
-- ^ skip everything before `separator` and return what is left until the end of the line


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
#ifdef darwin_HOST_OS
  macOSCPUNames
#elif linux_HOST_OS
  linuxCPUNames
#elif mingw32_HOST_OS
  windowsCPUNames
#endif

#ifdef darwin_HOST_OS
-- | macOS specific implementation
macOSCPUNames :: IO CPUNames
macOSCPUNames = do
  eCPU <- try $ readProcess "sysctl" ["machdep.cpu.brand_string", "machdep.cpu.thread_count"] ""

  case eCPU of
    Left (_ :: SomeException) -> pure []
    Right cpus -> do
      let cpuString = "machdep.cpu.brand_string:"
          nString   = "machdep.cpu.thread_count:"

      let mCPU = CPUName <$> parseLineAfter cpuString cpus
          mN   = readMaybe =<< parseLineAfter nString cpus

      case (mCPU, mN) of
        (Just cpu, Just n) -> pure $ replicate n cpu
        _ -> pure []

#elif linux_HOST_OS
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
