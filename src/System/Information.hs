{-# Language CPP #-}
module System.Information
  ( OS(..), os
  ) where

data OS = Linux | Windows | Other
  deriving (Eq, Show)

os :: OS
os =
#ifdef linux_HOST_OS
  Linux
#elif mingw32_HOST_OS
  Windows
#else
  Other
#endif
