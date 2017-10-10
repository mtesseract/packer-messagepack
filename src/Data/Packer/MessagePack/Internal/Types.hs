{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Packer.MessagePack.Internal.Types
  ( ToMsgPack(..)
  , FromMsgPack(..)
  , Object(..)
  , MsgPackException(..)
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Bool
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString                            as ByteString
import           Data.Int
import           Data.Map                                   (Map)
import qualified Data.Map                                   as Map
import           Data.Monoid
import           Data.Packer
import           Data.Packer.MessagePack.Internal.Constants
import           Data.Packer.MessagePack.Internal.Util
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Text.Encoding
import           Data.Word
import           GHC.Generics
import           UnliftIO.Exception

class ToMsgPack a where
  toMsgPack :: a -> Packing ()

class FromMsgPack a where
  fromMsgPack :: Unpacking a

-- Types

data Object = ObjectString Text
            | ObjectBinary ByteString
            | ObjectUInt Word64
            | ObjectInt Int64
            | ObjectBool Bool
            | ObjectFloat32 Double
            | ObjectFloat64 Double
            | ObjectArray [Object]
            | ObjectMap (Map Object Object)
            | ObjectNil
  deriving (Eq, Show, Ord, Generic)

data MsgPackException = MsgPackDeserializationFailure Text
                      | MsgPackSerializationFailure Text
  deriving (Show)

instance Exception MsgPackException

-- Instances

instance ToMsgPack Bool where
  toMsgPack = putWord8 . bool markerBoolFalse markerBoolTrue

instance FromMsgPack Bool where
  fromMsgPack = getWord8 >>= \w ->
    if | w == markerBoolTrue  -> return True
       | w == markerBoolFalse -> return False
       | otherwise            -> deserializationFailure "Bool"

-- Word8

instance ToMsgPack Word8 where
  toMsgPack x = putWord8 markerUInt8 >> putWord8 x

instance FromMsgPack Word8 where
  fromMsgPack = deserializationAssert markerUInt8 "Word8" >> getWord8

-- Int8

instance ToMsgPack Int8 where
  toMsgPack x = putWord8 markerInt8 >> putWord8 (fromIntegral x)

instance FromMsgPack Int8 where
  fromMsgPack = do
    deserializationAssert markerInt8 "Int8"
    fromIntegral <$> getWord8

-- Word16

instance ToMsgPack Word16 where
  toMsgPack x = putWord8 markerUInt16 >> putWord16BE x

instance FromMsgPack Word16 where
  fromMsgPack = do
    deserializationAssert markerUInt16 "Word16"
    getWord16BE

-- Int16

instance ToMsgPack Int16 where
  toMsgPack x = putWord8 markerInt16 >> putWord16BE (fromIntegral x)

instance FromMsgPack Int16 where
  fromMsgPack = do
    deserializationAssert markerInt16 "Int16"
    fromIntegral <$> getWord16BE

-- Word32

instance ToMsgPack Word32 where
  toMsgPack x = putWord8 markerUInt32 >> putWord32BE x

instance FromMsgPack Word32 where
  fromMsgPack = do
    deserializationAssert markerUInt32 "Word32"
    getWord32BE

-- Int32

instance ToMsgPack Int32 where
  toMsgPack x = putWord8 markerInt32 >> putWord32BE (fromIntegral x)

instance FromMsgPack Int32 where
  fromMsgPack = do
    deserializationAssert markerInt32 "Int32"
    fromIntegral <$> getWord32BE

-- Word64

instance ToMsgPack Word64 where
  toMsgPack x = putWord8 markerUInt64 >> putWord64BE x

instance FromMsgPack Word64 where
  fromMsgPack = do
    deserializationAssert markerUInt64 "Word64"
    getWord64BE

-- Int64

instance ToMsgPack Int64 where
  toMsgPack x = putWord8 markerInt64 >> putWord64BE (fromIntegral x)

instance FromMsgPack Int64 where
  fromMsgPack = do
    deserializationAssert markerInt64 "Int64"
    fromIntegral <$> getWord64BE

-- Variably sized integers.

newtype VarInt = VarInt Int64 deriving (Eq, Show, Ord)

newtype VarUInt = VarUInt Word64 deriving (Eq, Show, Ord)

instance ToMsgPack VarUInt where
  toMsgPack (VarUInt x)
    | x < 2^7   = putWord8  (fromIntegral x :: Word8)
    | x < 2^8   = toMsgPack (fromIntegral x :: Word8)
    | x < 2^16  = toMsgPack (fromIntegral x :: Word16)
    | x < 2^32  = toMsgPack (fromIntegral x :: Word32)
    | otherwise = toMsgPack x

instance ToMsgPack VarInt where
  toMsgPack (VarInt x)
    | 0 <= x && x <= 2^7 = putWord8 (fromIntegral x)
    | -2^5 <= x && x < 0 =
        putWord8 (fromIntegral x)
    | -2^7 <= x && x < 2^8 =
        toMsgPack (fromIntegral x :: Int8)
    | -2^15 <= x && x < 2^15 =
        toMsgPack (fromIntegral x :: Int16)
    | -2^31 <= x && x < 2^31 =
        toMsgPack (fromIntegral x :: Int32)
    | otherwise =
        toMsgPack x

-- Object

instance ToMsgPack Object where
  toMsgPack = \case
    ObjectInt i     -> toMsgPack (VarInt i)
    ObjectUInt i    -> toMsgPack (VarUInt i)
    ObjectMap m     -> toMsgPack m
    ObjectArray a   -> toMsgPack a
    ObjectString s  -> toMsgPack s
    ObjectNil       -> putWord8 markerNil
    ObjectBool b    -> toMsgPack b
    ObjectBinary bs -> toMsgPack bs
    -- ext
    -- fixext
    ObjectFloat32 b -> toMsgPack b
    ObjectFloat64 b -> toMsgPack b

instance FromMsgPack Object where
  fromMsgPack = do
    w <- unpackPeekWord8
    if | w == markerNil -> do
           _ <- getWord8
           return ObjectNil
       | w == markerBoolTrue ->
           getWord8 >> return (ObjectBool True)
       | w == markerBoolFalse ->
           getWord8 >> return (ObjectBool False)
       | not (testBit w 7) ->
           ObjectInt . fromIntegral <$> getWord8
       | (w .&. 0b11100000) == 0b11100000 -> do
           _ <- getWord8
           return $ ObjectInt (fromIntegral (fromIntegral w :: Int8))
       | w .&. 0b11100000 == markerFixStr
         || w == markerStr8
         || w == markerStr16
         || w == markerStr32 ->
           ObjectString <$> fromMsgPack
       | w == markerBin8
         || w == markerBin16
         || w == markerBin32 ->
           ObjectBinary <$> fromMsgPack
       | (w .&. 0b11110000) == 0b10010000
         || w == markerArray16
         || w == markerArray32 ->
         ObjectArray <$> fromMsgPack
       | (w .&. 0b11110000) == 0b10000000 ->
         ObjectMap <$> fromMsgPack
       | w == markerFloat32 ->
         ObjectFloat32 <$> fromMsgPack
       | w == markerFloat64 ->
         ObjectFloat64 <$> fromMsgPack
       | w == markerStr8 -> ObjectString <$> fromMsgPack
       | otherwise ->
         let msg = "Invalid MessagePack marker: " <> Text.pack (show w)
         in throwIO $ MsgPackDeserializationFailure msg

-- Float32

instance ToMsgPack Float where
  toMsgPack x = do
    putWord8 markerFloat32
    putFloat32BE x

instance FromMsgPack Float where
  fromMsgPack = do
    deserializationAssert markerFloat32 "Float32"
    getFloat32BE

-- Float64

instance ToMsgPack Double where
  toMsgPack x = do
    putWord8 markerFloat64
    putFloat64BE x

instance FromMsgPack Double where
  fromMsgPack = do
    deserializationAssert markerFloat64 "Float64"
    getFloat64BE

-- Binary String

instance ToMsgPack ByteString where
  toMsgPack x
    | l < 2^8 = do
        putWord8 markerBin8
        putWord8 $ fromIntegral l
        putBytes x
    | l < 2^16 = do
        putWord8 markerBin16
        putWord16BE $ fromIntegral l
        putBytes x
    | l < 2^32 = do
        putWord8 markerBin32
        putWord32BE $ fromIntegral l
        putBytes x
    | otherwise =
        throwIO $ MsgPackSerializationFailure "ByteString too long"

    where l = ByteString.length x

instance FromMsgPack ByteString where
  fromMsgPack = do
    w <- getWord8
    if | w == markerBin8 -> do
           l <- getWord8
           getBytes (fromIntegral l)
       | w == markerBin16 -> do
           l <- getWord16BE
           getBytes (fromIntegral l)
       | w == markerBin32 -> do
           l <- getWord32BE
           getBytes (fromIntegral l) -- FIXME overflow?
       | otherwise ->
           let msg = "Missing marker: Raw Bin (found " <> Text.pack (show w) <> ")"
           in throwIO $ MsgPackDeserializationFailure msg

-- String

instance ToMsgPack Text where
  toMsgPack x
    | l < 32 = do
        putWord8 (markerFixStr .|. fromIntegral l)
        putBytes (encodeUtf8 x)
    | l < 2^8 = do
        putWord8 markerStr8
        putWord8 (fromIntegral l .&. 0xFF)
        putBytes (encodeUtf8 x)
    | l < 2^16 = do
        putWord8 markerStr16
        putWord16BE (fromIntegral l .&. 0xFFFF)
        putBytes (encodeUtf8 x)
    | l < 2^32 = do
        putWord8 markerStr32
        putWord32BE (fromIntegral l .&. 0xFFFFFFFF)
        putBytes (encodeUtf8 x)
    | otherwise =
        throwIO $ MsgPackSerializationFailure "Text too long"

    where l = Text.length x

instance FromMsgPack Text where
  fromMsgPack = do
    w <- getWord8
    if | (w .&. 0b11100000) == markerFixStr ->
           decodeUtf8 <$> getBytes (fromIntegral (w .&. 0b00011111))
       | w == markerStr8 -> do
           l <- getWord8
           decodeUtf8 <$> getBytes (fromIntegral l)
       | w == markerStr16 -> do
           l <- getWord16BE
           decodeUtf8 <$> getBytes (fromIntegral l)
       | w == markerStr32 -> do
           l <- getWord32BE
           decodeUtf8 <$> getBytes (fromIntegral l) -- FIXME overflow?
       | otherwise ->
           throwIO $ MsgPackDeserializationFailure ("Missing Marker: Raw String (found " <> Text.pack (show w) <> ")")

-- Array

instance ToMsgPack [Object] where
  toMsgPack array
    | l < 16 = do
        putWord8 $ markerFixArray .|. (fromIntegral l .&. 0x0F)
        mapM_ toMsgPack array
    | l < 2^16 = do
        putWord8 markerArray16
        putWord16BE $ fromIntegral l
        mapM_ toMsgPack array
    | l < 2^32 = do
        putWord8 markerArray32
        putWord32BE $ fromIntegral l
        mapM_ toMsgPack array
    | otherwise =
        throwIO $ MsgPackSerializationFailure "Array too long"

    where l = length array

instance FromMsgPack [Object] where
  fromMsgPack = do
    w <- getWord8 -- FIXME, overflow
    l <- if | w .&. 0b11110000 == 0b10010000 ->
              return . fromIntegral $ w .&. 0b00001111
            | w == markerArray16 ->
                fromIntegral <$> getWord16BE
            | w == markerArray32 ->
                fromIntegral <$> getWord32BE
            | otherwise ->
              let msg = "Missing Marker: Array (found " <> Text.pack (show w) <> ")"
              in throwIO $ MsgPackDeserializationFailure msg
    replicateM (fromIntegral l) fromMsgPack

-- Map

instance ToMsgPack (Map Object Object) where
  toMsgPack m
    | l < 16 = do
        putWord8 $ markerFixMap .|. (fromIntegral l .&. 0x0F)
        mapM_ toMsgPack objects
    | l < 2^16 = do
        putWord8 markerMap16
        putWord16BE $ fromIntegral l
        mapM_ toMsgPack objects
    | l < 2^32 = do
        putWord8 markerMap32
        putWord32BE $ fromIntegral l
        mapM_ toMsgPack objects
    | otherwise =
        throwIO $ MsgPackSerializationFailure "Map too long"

    where l = Map.size m
          objects = concatMap (\(k,v) -> [k,v]) (Map.toList m)

instance FromMsgPack (Map Object Object) where
  fromMsgPack = do
    w <- getWord8 -- FIXME, overflow
    l <- if | w .&. 0b11110000 == 0b10000000 ->
              return . fromIntegral $ w .&. 0b00001111
            | w == markerMap16 -> fromIntegral <$> getWord16BE
            | w == markerMap32 -> fromIntegral <$> getWord32BE
    Map.fromList <$> replicateM l ((,) <$> fromMsgPack <*> fromMsgPack)

deserializationFailure :: MonadIO m => Text -> m a
deserializationFailure msg =
  throwIO $ MsgPackDeserializationFailure ("Expected " <> msg)

deserializationAssert :: Word8 -> Text -> Unpacking ()
deserializationAssert w msg = do
  w' <- getWord8
  unless (w == w') $
    deserializationFailure msg
