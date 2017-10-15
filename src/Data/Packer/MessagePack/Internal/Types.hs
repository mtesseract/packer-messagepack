{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Packer.MessagePack.Internal.Types
  ( ToMsgPack(..)
  , FromMsgPack(..)
  , Object(..)
  , MsgPackException(..)
  ) where

import           Control.Exception.Safe                     (MonadThrow, throw)
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
import qualified Data.Text.Encoding                         as Text
import           Data.Word
import           GHC.Generics
import           UnliftIO.Exception

class ToMsgPack a where
  toMsgPack :: a -> Packing ()
  msgPackSize :: MonadThrow m => a -> m Int64

class FromMsgPack a where
  fromMsgPack :: Unpacking a

toMsgPackUInt :: Word64 -> Packing ()
toMsgPackUInt x
  | x < 2^7   = putWord8 (fromIntegral x)
  | x < 2^8   = putWord8 markerUInt8  >> putWord8    (fromIntegral x)
  | x < 2^16  = putWord8 markerUInt16 >> putWord16BE (fromIntegral x)
  | x < 2^32  = putWord8 markerUInt32 >> putWord32BE (fromIntegral x)
  | otherwise = putWord8 markerUInt64 >> putWord64BE (fromIntegral x)

fromMsgPackUInt :: Unpacking Word64
fromMsgPackUInt = do
  w <- getWord8
  if | w .&. 0b10000000 == 0 -> return (fromIntegral w)
     | w == markerUInt8      -> fromIntegral <$> getWord8
     | w == markerUInt16     -> fromIntegral <$> getWord16BE
     | w == markerUInt32     -> fromIntegral <$> getWord32BE
     | w == markerUInt64     -> fromIntegral <$> getWord64BE
     | otherwise             -> throwIO (exn w)

  where exn w =
          MsgPackDeserializationFailure ("Invalid UInt Marker: " <> Text.pack (show w))

sizeMsgPackUInt :: Word64 -> Int64
sizeMsgPackUInt x
  | x < 2^7   = 1
  | x < 2^8   = 2
  | x < 2^16  = 3
  | x < 2^32  = 5
  | otherwise = 9

toMsgPackInt :: Int64 -> Packing ()
toMsgPackInt x
  | 0     <= x && x <= 2^7  = putWord8 (fromIntegral x)
  | -2^5  <= x && x <  0    = putWord8 (fromIntegral x)
  | -2^7  <= x && x <  2^8  = putWord8 markerInt8  >> putWord8 (fromIntegral x)
  | -2^15 <= x && x <  2^15 = putWord8 markerInt16 >> putWord16BE (fromIntegral x)
  | -2^31 <= x && x <  2^31 = putWord8 markerInt32 >> putWord32BE (fromIntegral x)
  | otherwise               = putWord8 markerInt64 >> putWord64BE (fromIntegral x)

fromMsgPackInt :: Unpacking Int64
fromMsgPackInt = do
  w <- getWord8
  let w' = fromIntegral w :: Int8
  if | w .&. 0b10000000 == 0 -> return (fromIntegral w)
     | -2^5 <= w' && w' < 0  -> return (fromIntegral w)
     | w == markerInt8       -> fromIntegral <$> getWord8
     | w == markerInt16      -> fromIntegral <$> getWord16BE
     | w == markerInt32      -> fromIntegral <$> getWord32BE
     | w == markerInt64      -> fromIntegral <$> getWord64BE
     | otherwise             -> throwIO (exn w)

  where exn w =
          MsgPackDeserializationFailure ("Invalid Int Marker: " <> Text.pack (show w))

sizeMsgPackInt :: Int64 -> Int64
sizeMsgPackInt x
  | 0     <= x && x <= 2^7  = 1
  | -2^5  <= x && x <  0    = 1
  | -2^7  <= x && x <  2^8  = 2
  | -2^15 <= x && x <  2^15 = 3
  | -2^31 <= x && x <  2^31 = 5
  | otherwise               = 9

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
  msgPackSize _ = return 1

instance FromMsgPack Bool where
  fromMsgPack = getWord8 >>= \w ->
    if | w == markerBoolTrue  -> return True
       | w == markerBoolFalse -> return False
       | otherwise            -> deserializationFailure "Bool"

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

-- Word8

instance ToMsgPack Word8 where
  toMsgPack = toMsgPack . (fromIntegral :: Word8 -> Word64)
  msgPackSize = msgPackSize . (fromIntegral :: Word8 -> Word64)

instance FromMsgPack Word8 where
  fromMsgPack = fromMsgPackUInt >>= shrinkTypeIO

-- Int8

instance ToMsgPack Int8 where
  toMsgPack = toMsgPack . (fromIntegral :: Int8 -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int8 -> Int64)

instance FromMsgPack Int8 where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- Word16

instance ToMsgPack Word16 where
  toMsgPack = toMsgPack . (fromIntegral :: Word16 -> Word64)
  msgPackSize = msgPackSize . (fromIntegral :: Word16 -> Word64)

instance FromMsgPack Word16 where
  fromMsgPack = fromMsgPackUInt >>= shrinkTypeIO

-- Int16

instance ToMsgPack Int16 where
  toMsgPack = toMsgPack . (fromIntegral :: Int16 -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int16 -> Int64)

instance FromMsgPack Int16 where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- Word32

instance ToMsgPack Word32 where
  toMsgPack = toMsgPack . (fromIntegral :: Word32 -> Word64)
  msgPackSize = msgPackSize . (fromIntegral :: Word32 -> Word64)

instance FromMsgPack Word32 where
  fromMsgPack = fromMsgPackUInt >>= shrinkTypeIO

-- Int32

instance ToMsgPack Int32 where
  toMsgPack = toMsgPack . (fromIntegral :: Int32 -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int32 -> Int64)

instance FromMsgPack Int32 where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- Word64

instance ToMsgPack Word64 where
  toMsgPack = toMsgPackUInt
  msgPackSize = return . sizeMsgPackUInt

instance FromMsgPack Word64 where
  fromMsgPack = fromMsgPackUInt

-- Int64

instance ToMsgPack Int64 where
  toMsgPack   = toMsgPackInt
  msgPackSize = return . sizeMsgPackInt

instance FromMsgPack Int64 where
  fromMsgPack = fromMsgPackInt

-- Object

instance ToMsgPack Object where
  toMsgPack = \case
    ObjectInt i     -> toMsgPack i
    ObjectUInt i    -> toMsgPack i
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
  msgPackSize = \case
    ObjectInt i     -> msgPackSize i
    ObjectUInt i    -> msgPackSize i
    ObjectMap m     -> msgPackSize m
    ObjectArray a   -> msgPackSize a
    ObjectString s  -> msgPackSize s
    ObjectNil       -> return 1
    ObjectBool b    -> msgPackSize b
    ObjectBinary bs -> msgPackSize bs
    -- ext
    -- fixext
    ObjectFloat32 b -> msgPackSize b
    ObjectFloat64 b -> msgPackSize b

instance FromMsgPack Object where
  fromMsgPack = do
    w <- unpackPeekWord8
    let w' = fromIntegral w :: Int8
    if | w == markerNil ->
           skipWord >> return ObjectNil
       | w == markerBoolTrue  ->
           skipWord >> return (ObjectBool True)
       | w == markerBoolFalse ->
           skipWord >> return (ObjectBool False)
       | not (testBit w 7) ->
           ObjectInt . fromIntegral <$> getWord8
       | -2^5 <= w' && w' < 0 ->
           skipWord >> return (ObjectInt (fromIntegral (fromIntegral w :: Int8)))
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
       | otherwise ->
         let msg = "Invalid MessagePack marker: " <> Text.pack (show w)
         in throwIO $ MsgPackDeserializationFailure msg

      where skipWord = void getWord8

-- Float32

instance ToMsgPack Float where
  toMsgPack x = do
    putWord8 markerFloat32
    putFloat32BE x
  msgPackSize _ = return 5

instance FromMsgPack Float where
  fromMsgPack = do
    deserializationAssert markerFloat32 "Float32"
    getFloat32BE

-- Float64

instance ToMsgPack Double where
  toMsgPack x = do
    putWord8 markerFloat64
    putFloat64BE x
  msgPackSize _ = return 9

instance FromMsgPack Double where
  fromMsgPack = do
    deserializationAssert markerFloat64 "Float64"
    getFloat64BE

-- Binary String

instance ToMsgPack ByteString where
  toMsgPack bs
    | l < 2^8 = do
        putWord8 markerBin8
        putWord8 $ fromIntegral l
        putBytes bs
    | l < 2^16 = do
        putWord8 markerBin16
        putWord16BE $ fromIntegral l
        putBytes bs
    | l < 2^32 = do
        putWord8 markerBin32
        putWord32BE $ fromIntegral l
        putBytes bs
    | otherwise =
        throwIO $ MsgPackSerializationFailure "ByteString too long"

    where l = ByteString.length bs

  msgPackSize bs =
    if | l < 2^8   -> return (2 + l)
       | l < 2^16  -> return (3 + l)
       | l < 2^32  -> return (5 + l)
       | otherwise -> throw $ MsgPackSerializationFailure "ByteString too long"

    where l = fromIntegral (ByteString.length bs) -- FIXME

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
  toMsgPack t
    | l < 32 = do
        putWord8 (markerFixStr .|. fromIntegral l)
        putBytes bs
    | l < 2^8 = do
        putWord8 markerStr8
        putWord8 (fromIntegral l .&. 0xFF)
        putBytes bs
    | l < 2^16 = do
        putWord8 markerStr16
        putWord16BE (fromIntegral l .&. 0xFFFF)
        putBytes bs
    | l < 2^32 = do
        putWord8 markerStr32
        putWord32BE (fromIntegral l .&. 0xFFFFFFFF)
        putBytes bs
    | otherwise =
        throwIO $ MsgPackSerializationFailure "Text too long"

    where bs = Text.encodeUtf8 t
          l  = ByteString.length bs

  msgPackSize t =
    if | l < 32    -> return (1 + l)
       | l < 2^8   -> return (2 + l)
       | l < 2^16  -> return (3 + l)
       | l < 2^32  -> return (5 + l)
       | otherwise -> throw $ MsgPackSerializationFailure "Text too long"

    where bs = Text.encodeUtf8 t
          l  = fromIntegral $ ByteString.length bs

instance FromMsgPack Text where
  fromMsgPack = do
    w <- getWord8
    if | (w .&. 0b11100000) == markerFixStr ->
           Text.decodeUtf8 <$> getBytes (fromIntegral (w .&. 0b00011111))
       | w == markerStr8 -> getWord8 >>= getText
       | w == markerStr16 -> getWord16BE >>= getText
       | w == markerStr32 -> getWord32BE >>= getText
       | otherwise -> throwIO (exn w)

      where exn w = MsgPackDeserializationFailure (exnMsg w)
            exnMsg w = "Missing Marker: Raw String (found " <> Text.pack (show w) <> ")"
            getText l = Text.decodeUtf8 <$> getBytes (fromIntegral l) -- FIXME?

-- Array

instance ToMsgPack a => ToMsgPack [a] where
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

  msgPackSize array =
    if | l < 16    -> (1 +) <$> arraySize array
       | l < 2^16  -> (3 +) <$> arraySize array
       | l < 2^32  -> (5 +) <$> arraySize array
       | otherwise -> throw exn

    where l = fromIntegral (length array)
          arraySize a = sum <$> mapM msgPackSize a
          exn = MsgPackSerializationFailure "Array too long"

instance FromMsgPack a => FromMsgPack [a] where
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

-- Pairs

instance (ToMsgPack a, ToMsgPack b) => ToMsgPack (a, b) where
  toMsgPack (a, b) = toMsgPack a >> toMsgPack b
  msgPackSize (a, b) = liftM2 (+) (msgPackSize a) (msgPackSize b)

instance (FromMsgPack a, FromMsgPack b) => FromMsgPack (a, b) where
  fromMsgPack = liftM2 (,) fromMsgPack fromMsgPack

-- Map

instance (ToMsgPack k, ToMsgPack v) => ToMsgPack (Map k v) where
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
          objects = Map.toList m

  msgPackSize m = sum <$> mapM msgPackSize (Map.toList m)

instance (Ord k, Ord v, FromMsgPack k, FromMsgPack v) => FromMsgPack (Map k v) where
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
