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

import           Control.Exception.Safe                      (MonadThrow, throw)
import           Control.Monad
import           Data.Bits
import           Data.Bool
import           Data.ByteString                             (ByteString)
import qualified Data.ByteString                             as ByteString
import           Data.Int
import           Data.Map                                    (Map)
import qualified Data.Map                                    as Map
import           Data.Monoid
import           Data.Packer
import           Data.Packer.MessagePack.Internal.Constants
import           Data.Packer.MessagePack.Internal.Exceptions
import           Data.Packer.MessagePack.Internal.Util
import           Data.Text                                   (Text)
import qualified Data.Text                                   as Text
import qualified Data.Text.Encoding                          as Text
import           Data.Word
import           GHC.Generics
import           UnliftIO.Exception

-- | Type class for values which support MessagePack serialization.
class ToMsgPack a where
  -- | Serializes the provided value as MessagePack within a 'Packing'
  -- monad.
  toMsgPack :: a -> Packing ()
  -- | Computes the size of the serialized data in bytes.
  msgPackSize :: MonadThrow m => a -> m Int

-- | Type class for values which support MessagePack deserialization.
class FromMsgPack a where
  -- | Deserializes a MessagePack value in an 'Unpacking' monad.
  fromMsgPack :: Unpacking a

-- | Serialize a single 'Word64' value. Depending on the size of the
-- value, one of the follow formats is used:
--
--  - Pos FixInt
--  - UInt  8
--  - UInt 16
--  - UInt 32
--  - UInt 64
toMsgPackUInt :: Word64 -> Packing ()
toMsgPackUInt x
  | x < 2^7   = putWord8 (fromIntegral x)
  | x < 2^8   = putWord8 markerUInt8  >> putWord8    (fromIntegral x)
  | x < 2^16  = putWord8 markerUInt16 >> putWord16BE (fromIntegral x)
  | x < 2^32  = putWord8 markerUInt32 >> putWord32BE (fromIntegral x)
  | otherwise = putWord8 markerUInt64 >> putWord64BE (fromIntegral x)

-- | Computes the deserialization size of the provided 'Word64'
-- number.
sizeMsgPackUInt :: Word64 -> Int
sizeMsgPackUInt x
  | x < 2^7   = 1
  | x < 2^8   = 2
  | x < 2^16  = 3
  | x < 2^32  = 5
  | otherwise = 9

-- | Deserialize an unsigned integer as a 'Word64' value.
fromMsgPackUInt :: Unpacking Word64
fromMsgPackUInt = do
  w <- getWord8
  if | hasMarkerPosFixNum w  -> fromIntegral <$> pure w
     | w == markerUInt8      -> fromIntegral <$> getWord8
     | w == markerUInt16     -> fromIntegral <$> getWord16BE
     | w == markerUInt32     -> fromIntegral <$> getWord32BE
     | w == markerUInt64     -> fromIntegral <$> getWord64BE
     | otherwise             -> throwIO (exn w)
  where exn w    = MsgPackDeserializationFailure (exnMsg w)
        exnMsg w = "Invalid UInt Marker: " <> Text.pack (show w)

-- | Serialize a single 'Int64' value. Depending on the size of the
-- value, one of the follow formats is used:
--
--  - Pos FixInt
--  - Neg FixInt
--  - Int  8
--  - Int 16
--  - Int 32
--  - Int 64
toMsgPackInt :: Int64 -> Packing ()
toMsgPackInt x
  | 0     <= x && x < 2^7  = putWord8 (fromIntegral x)
  | -2^5  <= x && x < 0    = putWord8 (fromIntegral x)
  | -2^7  <= x && x < 2^7  = putWord8 markerInt8  >> putWord8    (fromIntegral x)
  | -2^15 <= x && x < 2^15 = putWord8 markerInt16 >> putWord16BE (fromIntegral x)
  | -2^31 <= x && x < 2^31 = putWord8 markerInt32 >> putWord32BE (fromIntegral x)
  | otherwise              = putWord8 markerInt64 >> putWord64BE (fromIntegral x)

-- | Computes the deserialization size of the provided 'Int64' number.
sizeMsgPackInt :: Int64 -> Int
sizeMsgPackInt x
  | 0     <= x && x < 2^7  = 1
  | -2^5  <= x && x < 0    = 1
  | -2^7  <= x && x < 2^7  = 2
  | -2^15 <= x && x < 2^15 = 3
  | -2^31 <= x && x < 2^31 = 5
  | otherwise              = 9

-- | Deserialize a signed integer as an 'Int64' value.
fromMsgPackInt :: Unpacking Int64
fromMsgPackInt = do
  w <- getWord8
  if | hasMarkerPosFixNum w -> fromIntegral <$> pure w
     | hasMarkerNegFixNum w -> fromIntegral . (fromIntegral :: Word8  -> Int8)  <$> pure w
     | w == markerInt8      -> fromIntegral . (fromIntegral :: Word8  -> Int8)  <$> getWord8
     | w == markerInt16     -> fromIntegral . (fromIntegral :: Word16 -> Int16) <$> getWord16BE
     | w == markerInt32     -> fromIntegral . (fromIntegral :: Word32 -> Int32) <$> getWord32BE
     | w == markerInt64     -> fromIntegral . (fromIntegral :: Word64 -> Int64) <$> getWord64BE
     | otherwise            -> throwIO (exn w)
  where exn w    = MsgPackDeserializationFailure (exnMsg w)
        exnMsg w = "Invalid Int Marker: " <> Text.pack (show w)

-- | Data type wrapping any supported MessagePack value.
data Object = ObjectString Text
            | ObjectBinary ByteString
            | ObjectUInt Word64
            | ObjectInt Int64
            | ObjectBool Bool
            | ObjectFloat32 Float
            | ObjectFloat64 Double
            | ObjectArray [Object]
            | ObjectMap (Map Object Object)
            | ObjectNil
  deriving (Eq, Show, Ord, Generic)

-- | ToMsgPack instance for boolean values. This implements
-- serialization for the MessagePack bool format family.
instance ToMsgPack Bool where
  toMsgPack = putWord8 . bool markerBoolFalse markerBoolTrue
  msgPackSize _ = return 1

-- | FromMsgPack instance for boolean values. This implements
-- deserialization for the MessagePack bool format family.
instance FromMsgPack Bool where
  fromMsgPack = getWord8 >>= \w ->
    if | w == markerBoolTrue  -> return True
       | w == markerBoolFalse -> return False
       | otherwise            -> deserializationFailure "Bool"

-- | ToMsgPack instance for 'Word8' values. This implements
-- serialization for those unsigned values within the MessagePack int
-- format family, which fit in a 'Word8': positive fixint, uint8.
instance ToMsgPack Word8 where
  toMsgPack = toMsgPack . (fromIntegral :: Word8 -> Word64)
  msgPackSize = msgPackSize . (fromIntegral :: Word8 -> Word64)

-- | FromMsgPack instance for 'Word8' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in a 'Word8': positive fixint, uint8.
-- Deserializing bigger values will cause a
-- 'MsgPackDeserializationFailure' exception to be thrown.
instance FromMsgPack Word8 where
  fromMsgPack = fromMsgPackUInt >>= shrinkTypeIO

-- | ToMsgPack instance for 'Int' values. This implements
-- serialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int' (at least positive fixint,
-- negative fixint, int8, int16).
instance ToMsgPack Int where
  toMsgPack = toMsgPack . (fromIntegral :: Int -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int -> Int64)

-- | ToMsgPack instance for 'Int8' values. This implements
-- serialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int8': positive fixint, negative
-- fixint, int8.
instance ToMsgPack Int8 where
  toMsgPack = toMsgPack . (fromIntegral :: Int8 -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int8 -> Int64)

-- | FromMsgPack instance for 'Int' values. This implements
-- deserialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int' (at least positive fixint,
-- negative fixint, int8, int16). Deserializing bigger values will
-- cause a 'MsgPackDeserializationFailure' exception to be thrown.
instance FromMsgPack Int where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- | FromMsgPack instance for 'Int8' values. This implements
-- deserialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int8': positive fixint, negative
-- fixint, int8. Deserializing bigger values will cause a
-- 'MsgPackDeserializationFailure' exception to be thrown.
instance FromMsgPack Int8 where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- | ToMsgPack instance for 'Word16' values. This implements
-- serialization for those unsigned values within the MessagePack int
-- format family, which fit in a 'Word16': positive fixint, uint8,
-- uint16.
instance ToMsgPack Word16 where
  toMsgPack = toMsgPack . (fromIntegral :: Word16 -> Word64)
  msgPackSize = msgPackSize . (fromIntegral :: Word16 -> Word64)

-- | FromMsgPack instance for 'Word16' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in a 'Word16': positive fixint, uint8,
-- uint16. Deserializing bigger values will cause a
-- 'MsgPackDeserializationFailure' exception to be thrown.
instance FromMsgPack Word16 where
  fromMsgPack = fromMsgPackUInt >>= shrinkTypeIO

-- | ToMsgPack instance for 'Int16' values. This implements
-- serialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int16': positive fixint, negative
-- fixint, int8, int16.
instance ToMsgPack Int16 where
  toMsgPack = toMsgPack . (fromIntegral :: Int16 -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int16 -> Int64)

-- | FromMsgPack instance for 'Int16' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in an 'Int16': positive fixint,
-- negative fixint, int8, int16. Deserializing bigger values will
-- cause a 'MsgPackDeserializationFailure' exception to be thrown.
instance FromMsgPack Int16 where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- | ToMsgPack instance for 'Word32' values. This implements
-- serialization for those unsigned values within the MessagePack int
-- format family, which fit in a 'Word32': positive fixint, uint8,
-- uint16, uint32.
instance ToMsgPack Word32 where
  toMsgPack = toMsgPack . (fromIntegral :: Word32 -> Word64)
  msgPackSize = msgPackSize . (fromIntegral :: Word32 -> Word64)

-- | FromMsgPack instance for 'Word32' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in a 'Word32': positive fixint, uint8,
-- uint16, uint32. Deserializing bigger values will cause a
-- 'MsgPackDeserializationFailure' exception to be thrown.
instance FromMsgPack Word32 where
  fromMsgPack = fromMsgPackUInt >>= shrinkTypeIO

-- | ToMsgPack instance for 'Int32' values. This implements
-- serialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int32': positive fixint, negative
-- fixint, int8, int16, int32.
instance ToMsgPack Int32 where
  toMsgPack = toMsgPack . (fromIntegral :: Int32 -> Int64)
  msgPackSize = msgPackSize . (fromIntegral :: Int32 -> Int64)

-- | FromMsgPack instance for 'Int32' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in an 'Int16': positive fixint,
-- negative fixint, int8, int16, int32. Deserializing bigger values
-- will cause a 'MsgPackDeserializationFailure' exception to be
-- thrown.
instance FromMsgPack Int32 where
  fromMsgPack = fromMsgPackInt >>= shrinkTypeIO

-- | ToMsgPack instance for '64' values. This implements
-- serialization for those unsigned values within the MessagePack int
-- format family, which fit in a 'Word64': positive fixint, uint8,
-- uint16, uint32, uint64.
instance ToMsgPack Word64 where
  toMsgPack = toMsgPackUInt
  msgPackSize = return . sizeMsgPackUInt

-- | FromMsgPack instance for 'Word64' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in a 'Word64': positive fixint, uint8,
-- uint16, uint32, uint64.
instance FromMsgPack Word64 where
  fromMsgPack = fromMsgPackUInt

-- | ToMsgPack instance for 'Int64' values. This implements
-- serialization for those signed values within the MessagePack int
-- format family, which fit in an 'Int64': positive fixint, negative
-- fixint, int8, int16, int32, int64.
instance ToMsgPack Int64 where
  toMsgPack   = toMsgPackInt
  msgPackSize = return . sizeMsgPackInt

-- | FromMsgPack instance for 'Int64' values. This implements
-- deserialization for those unsigned values within the MessagePack
-- int format family, which fit in an 'Int64': positive fixint,
-- negative fixint, int8, int16, int32, int64.
instance FromMsgPack Int64 where
  fromMsgPack = fromMsgPackInt

-- | ToMsgPack instance for float values. This implements
-- serialization for the MessagePack float32 format family.
instance ToMsgPack Float where
  toMsgPack x = putWord8 markerFloat32 >> putFloat32BE x
  msgPackSize _ = return 5

-- | FromMsgPack instance for float values. This implements
-- deserialization for the MessagePack float32 format family.
instance FromMsgPack Float where
  fromMsgPack = deserializationAssert markerFloat32 "Float32" >> getFloat32BE

-- | ToMsgPack instance for double values. This implements
-- serialization for the MessagePack float64 format family.
instance ToMsgPack Double where
  toMsgPack x = putWord8 markerFloat64 >> putFloat64BE x
  msgPackSize _ = return 9

-- | FromMsgPack instance for double values. This implements
-- deserialization for the MessagePack float64 format family.
instance FromMsgPack Double where
  fromMsgPack = do
    deserializationAssert markerFloat64 "Float64"
    getFloat64BE

-- | ToMsgPack instance for 'ByteString's. This implements
-- serialization for the MessagePack bin format family for raw binary
-- strings up to a length of @2^32 - 1@.
instance ToMsgPack ByteString where
  toMsgPack bs
    | l < 2^8   = putWord8 markerBin8  >> putWord8    l >> putBytes bs
    | l < 2^16  = putWord8 markerBin16 >> putWord16BE l >> putBytes bs
    | l < 2^32  = putWord8 markerBin32 >> putWord32BE l >> putBytes bs
    | otherwise = failWithException

    where l :: Integral a => a
          l = fromIntegral $ ByteString.length bs
          failWithException = throwIO $ MsgPackSerializationFailure "ByteString too long"

  msgPackSize bs =
    if | l < 2^8   -> return (2 + l)
       | l < 2^16  -> return (3 + l)
       | l < 2^32  -> return (5 + l)
       | otherwise -> throw $ MsgPackSerializationFailure "ByteString too long"

    where l = fromIntegral (ByteString.length bs) -- FIXME

-- | FromMsgPack instance for 'ByteString's. This implements
-- deserialization for the MessagePack bin format family for raw binary
-- strings up to a length of @2^32 - 1@.
instance FromMsgPack ByteString where
  fromMsgPack = do
    w <- getWord8
    l <- if | w == markerBin8  -> fromIntegral <$> getWord8
            | w == markerBin16 -> fromIntegral <$> getWord16BE
            | w == markerBin32 -> fromIntegral <$> getWord32BE
            | otherwise        -> failWithException w
    getBytes l -- FIXME overflow

      where failWithException w =
              let msg = "Missing marker: Raw Bin (found " <> Text.pack (show w) <> ")"
              in throwIO $ MsgPackDeserializationFailure msg

-- | ToMsgPack instance for 'Text's. This implements serialization for
-- the MessagePack str format family for UTF8 encoded strings up to a
-- length of @2^32 - 1@.
instance ToMsgPack Text where
  toMsgPack t
    | l < 2^5   = putWord8 (markerFixStr .|. l)                          >> putBytes bs
    | l < 2^8   = putWord8 markerStr8  >> putWord8    (l .&.       0xFF) >> putBytes bs
    | l < 2^16  = putWord8 markerStr16 >> putWord16BE (l .&.     0xFFFF) >> putBytes bs
    | l < 2^32  = putWord8 markerStr32 >> putWord32BE (l .&. 0xFFFFFFFF) >> putBytes bs
    | otherwise = failWithException

    where bs = Text.encodeUtf8 t
          l :: Integral a => a
          l  = fromIntegral $ ByteString.length bs
          failWithException = throwIO $ MsgPackSerializationFailure "Text too long"

  msgPackSize t =
    if | l < 32    -> return (1 + l)
       | l < 2^8   -> return (2 + l)
       | l < 2^16  -> return (3 + l)
       | l < 2^32  -> return (5 + l)
       | otherwise -> throw $ MsgPackSerializationFailure "Text too long"

    where bs = Text.encodeUtf8 t
          l  = fromIntegral $ ByteString.length bs

-- | FromMsgPack instance for 'Text'. This implements deserialization
-- for the MessagePack str format family for UTF8 encoded strings up
-- to a length of @2^32 - 1@.
instance FromMsgPack Text where
  fromMsgPack = do
    w <- getWord8
    l <- if | hasMarkerFixStr w -> fromIntegral <$> pure (w .&. 0b00011111)
            | w == markerStr8   -> fromIntegral <$> getWord8
            | w == markerStr16  -> fromIntegral <$> getWord16BE
            | w == markerStr32  -> fromIntegral <$> getWord32BE
            | otherwise         -> failWithException w
    Text.decodeUtf8 <$> getBytes l

      where failWithException w = throwIO $ MsgPackDeserializationFailure (exnMsg w)
            exnMsg w = "Missing Marker: Raw String (found " <> Text.pack (show w) <> ")"

-- | ToMsgPack instance for lists. This implements serialization for
-- the MessagePack array format family for collections of up to a
-- length of @2^32 - 1@.
instance ToMsgPack a => ToMsgPack [a] where
  toMsgPack array
    | l < 16    = putWord8 (markerFixArray .|. (fromIntegral l .&. 0x0F)) >> mapM_ toMsgPack array
    | l < 2^16  = putWord8 markerArray16 >> putWord16BE (fromIntegral l)  >> mapM_ toMsgPack array
    | l < 2^32  = putWord8 markerArray32 >> putWord32BE (fromIntegral l)  >> mapM_ toMsgPack array
    | otherwise =  throwIO $ MsgPackSerializationFailure "Array too long"

    where l = length array

  msgPackSize array =
    if | l < 16    -> (1 +) <$> arraySize array
       | l < 2^16  -> (3 +) <$> arraySize array
       | l < 2^32  -> (5 +) <$> arraySize array
       | otherwise -> throw exn

    where l = fromIntegral (length array)
          arraySize a = sum <$> mapM msgPackSize a
          exn = MsgPackSerializationFailure "Array too long"

-- | FromMsgPack instance for lists. This implements deserialization
-- for the MessagePack array format family for collections of up to a
-- length of @2^32 - 1@.
instance FromMsgPack a => FromMsgPack [a] where
  fromMsgPack = do
    w <- getWord8 -- FIXME, overflow
    l <- if | hasMarkerFixArray w -> fromIntegral <$> pure (w .&. 0b00001111)
            | w == markerArray16  -> fromIntegral <$> getWord16BE
            | w == markerArray32  -> fromIntegral <$> getWord32BE
            | otherwise           -> failWithException w
    replicateM (fromIntegral l) fromMsgPack

      where failWithException w =
              let msg = "Missing Marker: Array (found " <> Text.pack (show w) <> ")"
              in throwIO $ MsgPackDeserializationFailure msg

-- | ToMsgPack instance for pairs. This instance serializes the first
-- value of the pair and then the second value of the pair.
instance (ToMsgPack a, ToMsgPack b) => ToMsgPack (a, b) where
  toMsgPack (a, b) = toMsgPack a >> toMsgPack b
  msgPackSize (a, b) = liftM2 (+) (msgPackSize a) (msgPackSize b)

-- | FromMsgPack instance for pairs. This instance deserializes the
-- first value of the pair and then the second value of the pair.
instance (FromMsgPack a, FromMsgPack b) => FromMsgPack (a, b) where
  fromMsgPack = liftM2 (,) fromMsgPack fromMsgPack

-- | FromMsgPack instance for maps. This implements deserialization
-- for the MessagePack map format family for maps of up to @2^32 - 1@
-- keys resp. values.
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

  msgPackSize m =
    if | l < 16    -> (1 +) <$> mapSize
       | l < 2^16  -> (3 +) <$> mapSize
       | l < 2^32  -> (5 +) <$> mapSize
       | otherwise -> throw exn

    where l = fromIntegral (Map.size m)
          mapSize = sum <$> mapM msgPackSize (Map.toList m)
          exn = MsgPackSerializationFailure "Map too long"


-- | FromMsgPack instance for 'Map's. This implements deserialization
-- for the MessagePack map format family for maps of up to @2^32 - 1@
-- keys resp. values.
instance (Ord k, Ord v, FromMsgPack k, FromMsgPack v) => FromMsgPack (Map k v) where
  fromMsgPack = do
    w <- getWord8 -- FIXME, overflow
    l <- if | hasMarkerFixMap w -> fromIntegral <$> pure (w .&. 0b00001111)
            | w == markerMap16  -> fromIntegral <$> getWord16BE
            | w == markerMap32  -> fromIntegral <$> getWord32BE
    Map.fromList <$> replicateM l ((,) <$> fromMsgPack <*> fromMsgPack)

-- | ToMsgPack instance for general MessagePack 'Object's.
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

-- | Data type modelling all known MessagePack markers.
data Marker = MarkerNil
            | MarkerTrue
            | MarkerFalse
            | MarkerPosFixnum
            | MarkerNegFixnum
            | MarkerWord8
            | MarkerWord16
            | MarkerWord32
            | MarkerWord64
            | MarkerInt8
            | MarkerInt16
            | MarkerInt32
            | MarkerInt64
            | MarkerFixStr
            | MarkerStr8
            | MarkerStr16
            | MarkerStr32
            | MarkerBin8
            | MarkerBin16
            | MarkerBin32
            | MarkerFixArray
            | MarkerArray16
            | MarkerArray32
            | MarkerFixMap
            | MarkerMap16
            | MarkerMap32
            | MarkerFloat32
            | MarkerFloat64
            deriving (Show, Eq)

-- | Parse the provided MessagePack 'Word8' as MessagePack 'Marker'.
parseMarker :: Word8 -> Maybe Marker
parseMarker w
  | hasMarkerPosFixNum w  = pure MarkerPosFixnum
  | hasMarkerNegFixNum w  = pure MarkerNegFixnum
  | hasMarkerFixStr w     = pure MarkerFixStr
  | hasMarkerFixArray w   = pure MarkerFixArray
  | hasMarkerFixMap w     = pure MarkerFixMap
  | w == markerNil        = pure MarkerNil
  | w == markerBoolTrue   = pure MarkerTrue
  | w == markerBoolFalse  = pure MarkerFalse
  | w == markerStr8       = pure MarkerStr8
  | w == markerInt8       = pure MarkerInt8
  | w == markerInt16      = pure MarkerInt16
  | w == markerInt32      = pure MarkerInt32
  | w == markerInt64      = pure MarkerInt64
  | w == markerUInt8      = pure MarkerWord8
  | w == markerUInt16     = pure MarkerWord16
  | w == markerUInt32     = pure MarkerWord32
  | w == markerUInt64     = pure MarkerWord64
  | w == markerStr8       = pure MarkerStr8
  | w == markerStr16      = pure MarkerStr16
  | w == markerStr32      = pure MarkerStr32
  | w == markerBin8       = pure MarkerBin8
  | w == markerBin16      = pure MarkerBin16
  | w == markerBin32      = pure MarkerBin32
  | w == markerArray16    = pure MarkerArray16
  | w == markerArray32    = pure MarkerArray32
  | w == markerFixMap     = pure MarkerFixMap
  | w == markerMap16      = pure MarkerMap16
  | w == markerMap32      = pure MarkerMap32
  | w == markerFloat32    = pure MarkerFloat32
  | w == markerFloat64    = pure MarkerFloat64
  | otherwise = Nothing

-- | Check if the provided 'Word8' contains a FixStr marker.
hasMarkerFixStr :: Word8 -> Bool
hasMarkerFixStr w =
  w .&. 0b11100000 == markerFixStr

-- | Check if the provided 'Word8' contains a FixArray marker.
hasMarkerFixArray :: Word8 -> Bool
hasMarkerFixArray w =
  w .&. 0b11110000 == markerFixArray

-- | Check if the provided 'Word8' contains a FixMap marker.
hasMarkerFixMap :: Word8 -> Bool
hasMarkerFixMap w =
  w .&. 0b11110000 == markerFixMap

-- | Check if the provided 'Word8' contains a Pos FixNum marker.
hasMarkerPosFixNum :: Word8 -> Bool
hasMarkerPosFixNum  w =
  w .&. 0b10000000 == 0

-- | Check if the provided 'Word8' contains a Neg FixNum marker.
hasMarkerNegFixNum :: Word8 -> Bool
hasMarkerNegFixNum w =
  let wInt8 = fromIntegral w :: Int8
  in -2^5 <= wInt8 && wInt8 < 0

-- | Given a MessagePack marker, deserialize an object.
--
-- Note: A positive fix num will cause the object to be deserialized
-- as a ObjectInt, not an ObjectUInt.
parseObject :: Marker -> Unpacking Object
parseObject MarkerNil       = pure ObjectNil            <*  skipWord
parseObject MarkerTrue      = ObjectBool                <$> fromMsgPack
parseObject MarkerFalse     = ObjectBool                <$> fromMsgPack
parseObject MarkerPosFixnum = ObjectInt  . fromIntegral <$> (fromMsgPack :: Unpacking Int8)
parseObject MarkerNegFixnum = ObjectInt  . fromIntegral <$> (fromMsgPack :: Unpacking Int8)
parseObject MarkerWord8     = ObjectUInt . fromIntegral <$> (fromMsgPack :: Unpacking Word8)
parseObject MarkerWord16    = ObjectUInt . fromIntegral <$> (fromMsgPack :: Unpacking Word16)
parseObject MarkerWord32    = ObjectUInt . fromIntegral <$> (fromMsgPack :: Unpacking Word32)
parseObject MarkerWord64    = ObjectUInt                <$> fromMsgPack
parseObject MarkerInt8      = ObjectInt  . fromIntegral <$> (fromMsgPack :: Unpacking Int8)
parseObject MarkerInt16     = ObjectInt  . fromIntegral <$> (fromMsgPack :: Unpacking Int16)
parseObject MarkerInt32     = ObjectInt  . fromIntegral <$> (fromMsgPack :: Unpacking Int32)
parseObject MarkerInt64     = ObjectInt                 <$> fromMsgPack
parseObject MarkerFixStr    = ObjectString              <$> fromMsgPack
parseObject MarkerStr8      = ObjectString              <$> fromMsgPack
parseObject MarkerStr16     = ObjectString              <$> fromMsgPack
parseObject MarkerStr32     = ObjectString              <$> fromMsgPack
parseObject MarkerBin8      = ObjectBinary              <$> fromMsgPack
parseObject MarkerBin16     = ObjectBinary              <$> fromMsgPack
parseObject MarkerBin32     = ObjectBinary              <$> fromMsgPack
parseObject MarkerFixArray  = ObjectArray               <$> fromMsgPack
parseObject MarkerArray16   = ObjectArray               <$> fromMsgPack
parseObject MarkerArray32   = ObjectArray               <$> fromMsgPack
parseObject MarkerFixMap    = ObjectMap                 <$> fromMsgPack
parseObject MarkerMap16     = ObjectMap                 <$> fromMsgPack
parseObject MarkerMap32     = ObjectArray               <$> fromMsgPack
parseObject MarkerFloat32   = ObjectFloat32             <$> fromMsgPack
parseObject MarkerFloat64   = ObjectFloat64             <$> fromMsgPack

-- | FromMsgPack instance for general MessagePack 'Object's.
instance FromMsgPack Object where
  fromMsgPack = do
    w <- unpackPeekWord8
    case parseMarker w of
      Just marker -> parseObject marker
      Nothing     -> let msg = "Invalid MessagePack marker: " <> Text.pack (show w)
                     in throwIO $ MsgPackDeserializationFailure msg

-- | Skip a single Word8 in the provided 'Unpacking' monad.
skipWord :: Unpacking ()
skipWord = void getWord8
