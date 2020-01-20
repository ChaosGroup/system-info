{-# Language BlockArguments #-}
{-# Language DerivingStrategies #-}
{-# Language GeneralisedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.OS
-- Copyright   :  (c) ChaosGroup, 2020
-- License     :  MIT
--
-- Maintainer  :  daniel.taskoff@gmail.com
-- Stability   :  experimental
--
-- Get the name of the current operating system.
-----------------------------------------------------------------------------

module System.OS
  (
  -- * 'OS'
    OS, os
  ) where

import Foreign.C.String (CWString, peekCWString)
import Foreign.Marshal.Alloc (free)
import System.IO.Unsafe (unsafePerformIO)


-- | The name of the current operating system.
newtype OS = OS String
  deriving newtype Show

-- | Get the name of the current operating system.
os :: OS
os = unsafePerformIO do
  -- unsafePerformIO and NOINLINE guarantee that c_getOS won't be called more than once
  os' <- c_getOS
  res <- peekCWString os'
  free os'
  pure $ OS res
{-# NOINLINE os #-}

foreign import ccall safe "getOS"
  c_getOS :: IO CWString