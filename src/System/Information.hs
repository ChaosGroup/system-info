{-# Language DerivingStrategies #-}
{-# Language GeneralisedNewtypeDeriving #-}
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
    OS, os
  ) where

import Foreign.C.String (CWString, peekCWString)
import Foreign.Marshal.Alloc (free)


-- | A datatype representing different operating systems.
newtype OS = OS String
  deriving newtype Show

-- | Get the current OS' name
os :: IO OS
os = OS <$> do
  os' <- c_getOS
  res <- peekCWString os'
  free os'
  pure res

foreign import ccall safe "getOS"
  c_getOS :: IO CWString
