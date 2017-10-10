module Data.Packer.MessagePack.Internal.Util where

import           Data.Packer
import           Data.Word

unpackPeekWord8 :: Unpacking Word8
unpackPeekWord8 = do
  pos <- unpackGetPosition
  w   <- getWord8
  unpackSetPosition pos
  return w
