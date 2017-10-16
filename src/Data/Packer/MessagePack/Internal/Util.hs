{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Packer.MessagePack.Internal.Util where

import           Control.Monad.IO.Class
import           Data.Packer
import           Data.Word
import           UnliftIO.Exception

import           Data.Packer.MessagePack.Internal.Exceptions

unpackPeekWord8 :: Unpacking Word8
unpackPeekWord8 = do
  pos <- unpackGetPosition
  w   <- getWord8
  unpackSetPosition pos
  return w

shrinkType :: forall f a b.
              (Applicative f, Integral a, Integral b, Bounded a, Bounded b)
           => f b -> a -> f b
shrinkType overflowFail a =
  if minBound <= a' && a' <= maxBound
  then pure a'
  else overflowFail
  where a' = fromIntegral a :: b

shrinkTypeIO :: forall m a b.
                (MonadIO m, Integral a, Integral b, Bounded a, Bounded b)
             => a -> m b
shrinkTypeIO = shrinkType (throwIO (MsgPackDeserializationFailure "Integer Overflow"))
