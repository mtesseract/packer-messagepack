{-# LANGUAGE OverloadedStrings #-}

module Data.Packer.MessagePack.Internal.Exceptions where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Packer
import           Data.Text
import           Data.Word
import           UnliftIO.Exception

data MsgPackException = MsgPackDeserializationFailure Text
                      | MsgPackSerializationFailure Text
  deriving (Show)

instance Exception MsgPackException

deserializationFailure :: MonadIO m => Text -> m a
deserializationFailure msg =
  throwIO $ MsgPackDeserializationFailure ("Expected " <> msg)

deserializationAssert :: Word8 -> Text -> Unpacking ()
deserializationAssert w msg = do
  w' <- getWord8
  unless (w == w') $
    deserializationFailure msg
