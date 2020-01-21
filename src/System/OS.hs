{-# Language BlockArguments #-}
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
    OS(..), os
  ) where

import Foreign.C.String (CWString, peekCWString)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)
import System.IO.Unsafe (unsafePerformIO)


-- | The name of the current operating system.
newtype OS = OS { unOS :: String }

-- | Try to get the name of the current operating system.
os :: Maybe OS
os = unsafePerformIO do
  -- unsafePerformIO and NOINLINE guarantee that c_getOS won't be called more than once
  osptr <- c_getOS
  if osptr == nullPtr
  then pure Nothing
  else do
    res <- peekCWString osptr
    free osptr
    pure $ Just $ OS res
{-# NOINLINE os #-}

foreign import ccall safe "getOS"
  c_getOS :: IO CWString
