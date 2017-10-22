{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NegativeLiterals    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Control.Exception.Safe
import           Data.Bits
import           Data.Bool
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as ByteString
import           Data.Foldable
import           Data.Int
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Packer
import           Data.Packer.MessagePack
import           Data.Proxy
import           Data.Text               (Text)
import qualified Data.Text.Encoding      as Text
import           Data.Word

data Bit = Bit0 | Bit1
  deriving (Eq, Show, Ord)

data BitException = BitParseFailure String
  deriving (Show, Typeable)

instance Exception BitException

showBits :: [Bit] -> String
showBits = map convert
  where convert Bit0 = '0'
        convert Bit1 = '1'

parseBits :: MonadThrow m => String -> m [Bit]
parseBits s = case parseBits' s of
  Right bits -> return bits
  Left  _    -> throwM $ BitParseFailure "Failure to bits"

parseBits' :: String -> Either String [Bit]
parseBits' = sequence . map parseBit
  where parseBit '1' = Right Bit1
        parseBit '0' = Right Bit0
        parseBit c   = Left ("Invalid character: " <> show c)

toBits :: FiniteBits a => a -> [Bit]
toBits a =
  reverse $ map (\i -> bool Bit0 Bit1 (testBit a i)) [0..(n - 1)]
  where n = finiteBitSize a

fromBits :: (Num a, Bits a) => [Bit] -> a
fromBits s =
  foldl' f 0 (zip (reverse s) [0..])
  where f accu (c, i) =
          case c of
            Bit1 -> setBit accu i
            Bit0 -> accu

nthWord :: (Bits a, Integral a) => a -> Int -> Word8
nthWord = go
  where go a 0 = fromIntegral $ a .&. 0xFF
        go a i = go (a `shiftR` 8) (i - 1)

genPosFixInt :: (MonadGen m, Integral a) => m a
genPosFixInt =
  Gen.integral (Range.linear 0 (2^7 - 1))

genNegFixInt :: forall m a. (MonadGen m, Integral a) => m a
genNegFixInt =
  fromIntegral . (fromIntegral :: a -> Int8)
  <$> Gen.integral (Range.linear (- 2^5) (- 1))

checkSerialization :: (Show a, Eq a, FromMsgPack a, ToMsgPack a)
                    => a -> [Word8] -> PropertyT IO ()
checkSerialization a ws = do
  annotate $ show a
  size <- fromIntegral <$> msgPackSize a
  let bsSerialized = runPacking size (toMsgPack a)
      bsExpected   = ByteString.pack ws
  annotate . show $ (ByteString.length bsSerialized)
  annotate . show $ (ByteString.length bsExpected)
  ByteString.unpack bsSerialized === ByteString.unpack bsExpected
  annotate $ show bsSerialized
  a === runUnpacking fromMsgPack bsSerialized

extractWordsBE :: forall a. (Integral a, FiniteBits a) => a -> [Word8]
extractWordsBE = go nWords []

  where nWords = (finiteBitSize (undefined :: a) + 7) `div` 8

        go 0 accu _ = accu
        go i accu a =
          let a' = fromIntegral (a `shiftR` 8)
          in go (i - 1)  (fromIntegral (a .&. 0xFF) : accu) a'


-- Nil

prop_nil :: Property
prop_nil = property $ do
  nil <- forAll $ Gen.constant ObjectNil
  checkSerialization nil [0xC0]

-- Bool

prop_bool :: Property
prop_bool = property $ do
  b <- forAll $ Gen.bool
  checkSerialization b [if b then 0xC3 else 0xC2]

prop_obj_bool :: Property
prop_obj_bool = property $ do
  b <- forAll Gen.bool
  checkSerialization (ObjectBool b) [if b then 0xC3 else 0xC2]

_prop_pos_fixnum_integral ::
  forall a. (Show a, FromMsgPack a, ToMsgPack a, Integral a)
  => Proxy a -> PropertyT IO ()
_prop_pos_fixnum_integral _proxy = do
  a :: a <- forAll genPosFixInt
  checkSerialization a [fromIntegral a .&. 0b01111111]

_prop_obj_pos_fixnum_integral ::
  forall a. (Show a, FromMsgPack a, ToMsgPack a, Integral a)
  => Proxy a -> PropertyT IO ()
_prop_obj_pos_fixnum_integral _proxy = do
  a :: a <- forAll genPosFixInt
  checkSerialization (ObjectInt (fromIntegral a)) [fromIntegral a .&. 0b01111111]

prop_pos_fixnum_int8 :: Property
prop_pos_fixnum_int8 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Int8)

prop_obj_pos_fixnum_int8 :: Property
prop_obj_pos_fixnum_int8 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Int8)

prop_pos_fixnum_int16 :: Property
prop_pos_fixnum_int16 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Int16)

prop_obj_pos_fixnum_int16 :: Property
prop_obj_pos_fixnum_int16 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Int16)

prop_pos_fixnum_int32 :: Property
prop_pos_fixnum_int32 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Int32)

prop_obj_pos_fixnum_int32 :: Property
prop_obj_pos_fixnum_int32 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Int32)

prop_pos_fixnum_int64 :: Property
prop_pos_fixnum_int64 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Int64)

prop_obj_pos_fixnum_int64 :: Property
prop_obj_pos_fixnum_int64 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Int64)

prop_obj_pos_fixnum_word8 :: Property
prop_obj_pos_fixnum_word8 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Word8)

prop_pos_fixnum_word16 :: Property
prop_pos_fixnum_word16 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Word16)

prop_obj_pos_fixnum_word16 :: Property
prop_obj_pos_fixnum_word16 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Word16)

prop_pos_fixnum_word32 :: Property
prop_pos_fixnum_word32 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Word32)

prop_obj_pos_fixnum_word32 :: Property
prop_obj_pos_fixnum_word32 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Word32)

prop_pos_fixnum_word64 :: Property
prop_pos_fixnum_word64 = property $ _prop_pos_fixnum_integral (Proxy :: Proxy Word64)

prop_obj_pos_fixnum_word64 :: Property
prop_obj_pos_fixnum_word64 = property $ _prop_obj_pos_fixnum_integral (Proxy :: Proxy Word64)

_prop_neg_fixnum_integral ::
  forall a. (Show a, FromMsgPack a, ToMsgPack a, Integral a)
  => Proxy a -> PropertyT IO ()
_prop_neg_fixnum_integral _proxy = do
  a :: a <- forAll genNegFixInt
  checkSerialization a [fromIntegral a .&. 0b11111111]

_prop_obj_neg_fixnum_integral ::
  forall a. (Show a, FromMsgPack a, ToMsgPack a, Integral a)
  => Proxy a -> PropertyT IO ()
_prop_obj_neg_fixnum_integral _proxy = do
  a :: a <- forAll genNegFixInt
  checkSerialization (ObjectInt (fromIntegral a)) [fromIntegral a .&. 0b11111111]

prop_neg_fixnum_int8 :: Property
prop_neg_fixnum_int8 = property $ _prop_neg_fixnum_integral (Proxy :: Proxy Int8)

prop_obj_neg_fixnum_int8 :: Property
prop_obj_neg_fixnum_int8 = property $ _prop_obj_neg_fixnum_integral (Proxy :: Proxy Int8)

prop_neg_fixnum_int16 :: Property
prop_neg_fixnum_int16 = property $ _prop_neg_fixnum_integral (Proxy :: Proxy Int16)

prop_obj_neg_fixnum_int16 :: Property
prop_obj_neg_fixnum_int16 = property $ _prop_obj_neg_fixnum_integral (Proxy :: Proxy Int16)

prop_neg_fixnum_int32 :: Property
prop_neg_fixnum_int32 = property $ _prop_neg_fixnum_integral (Proxy :: Proxy Int32)

prop_obj_neg_fixnum_int32 :: Property
prop_obj_neg_fixnum_int32 = property $ _prop_obj_neg_fixnum_integral (Proxy :: Proxy Int32)

prop_neg_fixnum_int64 :: Property
prop_neg_fixnum_int64 = property $ _prop_neg_fixnum_integral (Proxy :: Proxy Int64)

prop_obj_neg_fixnum_int64 :: Property
prop_obj_neg_fixnum_int64 = property $ _prop_obj_neg_fixnum_integral (Proxy :: Proxy Int64)

genWord8 :: forall m a. (MonadGen m, Integral a) => m a
genWord8 =
  fromIntegral . (fromIntegral :: a -> Word8)
  <$> Gen.integral (Range.linear (2^7) (2^8 - 1))

prop_word8_word8 :: Property
prop_word8_word8 = property $ do
  i :: Word8 <- forAll genWord8
  checkSerialization i [0xCC, fromIntegral i]

prop_obj_word8 :: Property
prop_obj_word8 = property $ do
  i <- forAll genWord8
  checkSerialization (ObjectUInt i) [0xCC, fromIntegral i]

prop_word8_word16 :: Property
prop_word8_word16 = property $ do
  i :: Word16 <- forAll genWord8
  checkSerialization i [0xCC, fromIntegral i]

prop_word8_word32 :: Property
prop_word8_word32 = property $ do
  i :: Word32 <- forAll genWord8
  checkSerialization i [0xCC, fromIntegral i]

prop_word8_word64 :: Property
prop_word8_word64 = property $ do
  i :: Word64 <- forAll genWord8
  checkSerialization i [0xCC, fromIntegral i]


-- UInt16

genWord16 :: forall m a. (MonadGen m, Integral a) => m a
genWord16 =
  fromIntegral . (fromIntegral :: a -> Word16)
  <$> Gen.integral (Range.linear (2^8) (2^16 - 1))

prop_word16_word16 :: Property
prop_word16_word16 = property $ do
  i :: Word16 <- forAll genWord16
  checkSerialization i [0xCD, nthWord i 1, nthWord i 0]

prop_obj_word16 :: Property
prop_obj_word16 = property $ do
  i :: Word16 <- forAll genWord16
  checkSerialization (ObjectUInt (fromIntegral i)) (0xCD : extractWordsBE i)

prop_word16_word32 :: Property
prop_word16_word32 = property $ do
  i :: Word16 <- forAll genWord16
  checkSerialization (fromIntegral i :: Word32) (0xCD : extractWordsBE i)

prop_word16_word64 :: Property
prop_word16_word64 = property $ do
  i :: Word64 <- forAll genWord16
  checkSerialization i [0xCD, nthWord i 1, nthWord i 0]

-- UInt32

genWord32 :: forall m a. (MonadGen m, Integral a) => m a
genWord32 =
  fromIntegral . (fromIntegral :: a -> Word32)
  <$> Gen.integral (Range.linear (2^16) (2^32 - 1))

prop_word32_word32 :: Property
prop_word32_word32 = property $ do
  i :: Word32 <- forAll genWord32
  checkSerialization i (0xCE : extractWordsBE i)

prop_obj_word32 :: Property
prop_obj_word32 = property $ do
  i :: Word32 <- forAll genWord32
  checkSerialization (ObjectUInt (fromIntegral i)) (0xCE : extractWordsBE i)

prop_word32_word64 :: Property
prop_word32_word64 = property $ do
  i :: Word32 <- forAll genWord32
  checkSerialization (fromIntegral i :: Word64) (0xCE : extractWordsBE i)

-- UInt64

genWord64 :: forall m a. (MonadGen m, Integral a) => m a
genWord64 =
  fromIntegral . (fromIntegral :: a -> Word64)
  <$> Gen.integral (Range.linear (2^32) (2^64 - 1))

prop_word64_word64 :: Property
prop_word64_word64 = property $ do
  i :: Word64 <- forAll genWord64
  checkSerialization i (0xCF : extractWordsBE i)

prop_obj_word64 :: Property
prop_obj_word64 = property $ do
  i :: Word64 <- forAll genWord64
  checkSerialization (ObjectUInt i) (0xCF : extractWordsBE i)

-- Int8

genInt8 :: forall m a. (MonadGen m, Integral a) => m a
genInt8 =
  fromIntegral . (fromIntegral :: a -> Int8)
  <$> Gen.integral (Range.linear (-2^5 - 1) (-2^7))

prop_int8_int8 :: Property
prop_int8_int8 = property $ do
  i :: Int8 <- forAll genInt8
  checkSerialization i [0xD0, fromIntegral i]

prop_obj_int8 :: Property
prop_obj_int8 = property $ do
  i :: Int8 <- forAll genInt8
  checkSerialization (ObjectInt (fromIntegral i)) (0xD0 : extractWordsBE i)

prop_int8_int16 :: Property
prop_int8_int16 = property $ do
  i :: Int16 <- forAll genInt8
  checkSerialization i (0xD0 : extractWordsBE i)

prop_int8_int32 :: Property
prop_int8_int32 = property $ do
  i :: Int32 <- forAll genInt8
  checkSerialization i (0xD0 : extractWordsBE i)

prop_int8_int64 :: Property
prop_int8_int64 = property $ do
  i :: Int64 <- forAll genInt8
  checkSerialization i (0xD0 : extractWordsBE i)

-- Int

prop_int8_int :: Property
prop_int8_int = property $ do
  i :: Int8 <- forAll genInt8
  checkSerialization (fromIntegral i :: Int) (0xD0 : extractWordsBE i)

prop_int16_int :: Property
prop_int16_int = property $ do
  i :: Int16 <- forAll genInt16
  checkSerialization (fromIntegral i :: Int) (0xD1 : extractWordsBE i)

-- Int16

genInt16 :: forall m a. (MonadGen m, Integral a) => m a
genInt16 =
  fromIntegral . (fromIntegral :: a -> Int16)
  <$> Gen.choice [ Gen.integral (Range.linear (-2^15) (-2^ 7 - 1))
                 , Gen.integral (Range.linear ( 2^ 7) ( 2^15 - 1)) ]

prop_int16_int16 :: Property
prop_int16_int16 = property $ do
  i :: Int16 <- forAll genInt16
  checkSerialization i (0xD1 : extractWordsBE i)

prop_obj_int16 :: Property
prop_obj_int16 = property $ do
  i :: Int16 <- forAll genInt16
  checkSerialization (ObjectInt (fromIntegral i)) (0xD1 : extractWordsBE i)

prop_int16_int32 :: Property
prop_int16_int32 = property $ do
  i :: Int32 <- forAll genInt16
  checkSerialization i (0xD1 : extractWordsBE (fromIntegral i :: Int16))

prop_int16_int64 :: Property
prop_int16_int64 = property $ do
  i :: Int64 <- forAll genInt16
  checkSerialization i (0xD1 : extractWordsBE (fromIntegral i :: Int16))

-- Int32

genInt32 :: forall m a. (MonadGen m, Integral a) => m a
genInt32 =
  fromIntegral . (fromIntegral :: a -> Int32)
  <$> Gen.choice [ Gen.integral (Range.linear (-2^31) (-2^15 - 1))
                 , Gen.integral (Range.linear ( 2^15) ( 2^31 - 1))]

prop_int32_int32 :: Property
prop_int32_int32 = property $ do
  i :: Int32 <- forAll genInt32
  checkSerialization i (0xD2 : extractWordsBE (fromIntegral i :: Int32))

prop_obj_int32 :: Property
prop_obj_int32 = property $ do
  i :: Int32 <- forAll genInt32
  checkSerialization (ObjectInt (fromIntegral i)) (0xD2 : extractWordsBE i)

prop_int32_int64 :: Property
prop_int32_int64 = property $ do
  i :: Int64 <- forAll genInt32
  checkSerialization i (0xD2 : extractWordsBE (fromIntegral i :: Int32))

-- Int64

genInt64 :: forall m a. (MonadGen m, Integral a) => m a
genInt64 =
  fromIntegral . (fromIntegral :: a -> Int64)
  <$> Gen.choice [ Gen.integral (Range.linear (-2^63) (-2^31 - 1))
                 , Gen.integral (Range.linear ( 2^31) ( 2^63 - 1))]

prop_int64_int64 :: Property
prop_int64_int64 = property $ do
  i :: Int64 <- forAll genInt64
  checkSerialization i (0xD3 : extractWordsBE (fromIntegral i :: Int64))

prop_obj_int64 :: Property
prop_obj_int64 = property $ do
  i :: Int64 <- forAll genInt64
  checkSerialization i (0xD3 : extractWordsBE i)

-- Float32

genFloat32 :: MonadGen m => m Float
genFloat32 = Gen.float (Range.exponentialFloat (- 2^32) (2^32))

prop_float32 :: Property
prop_float32 = property $ do
  f <- forAll genFloat32
  checkSerialization f (0xCA : ByteString.unpack (runPacking 16 (putFloat32BE f)))

floatWordsBE :: Float -> [Word8]
floatWordsBE f =
  ByteString.unpack (runPacking 16 (putFloat32BE f))

doubleWordsBE :: Double -> [Word8]
doubleWordsBE f =
  ByteString.unpack (runPacking 16 (putFloat64BE f))

prop_obj_float32 :: Property
prop_obj_float32 = property $ do
  f <- forAll genFloat32
  checkSerialization (ObjectFloat32 f) (0xCA : floatWordsBE f)

-- Float64

genFloat64 :: MonadGen m => m Double
genFloat64 = Gen.double (Range.exponentialFloat (- 2^64) (2^64))

prop_float64 :: Property
prop_float64 = property $ do
  f <- forAll genFloat64
  checkSerialization f (0xCB : ByteString.unpack (runPacking 16 (putFloat64BE f)))

prop_obj_float64 :: Property
prop_obj_float64 = property $ do
  f <- forAll genFloat64
  checkSerialization (ObjectFloat64 f) (0xCB : doubleWordsBE f)

-- Produces much more ASCII characters than unicode in order to
-- increase the change that generated texts will have a UTF decoded
-- bytestring in the requested range.
genChar :: MonadGen m => m Char
genChar = Gen.frequency [ (1, Gen.unicode)
                        , (9, Gen.ascii) ]

genStr :: MonadGen m => Range Int -> m Text
genStr range =
  Gen.filter (decodedTextInRange range) (Gen.text range genChar)

decodedTextInRange :: Range Int -> Text -> Bool
decodedTextInRange range text =
  let l = ByteString.length (Text.encodeUtf8 text)
      lower = Range.lowerBound 100 range
      upper = Range.upperBound 100 range
  in lower <= l && l <= upper

decodeTextLength :: Integral a => Text -> (ByteString, a)
decodeTextLength t =
  let bs = Text.encodeUtf8 t
      n  = fromIntegral $ ByteString.length bs
  in (bs, n)

-- FixStr

genFixStr :: MonadGen m => m Text
genFixStr = genStr $ Range.linear 0 (2^5 - 1)

prop_fixstr :: Property
prop_fixstr = property $ do
  t <- forAll genFixStr
  let (bs, n :: Word8) = decodeTextLength t
  checkSerialization t $ (fromIntegral n .|. 0b10100000) : ByteString.unpack bs

prop_obj_fixstr :: Property
prop_obj_fixstr = property $ do
  t <- forAll genFixStr
  let (bs, n :: Word8) = decodeTextLength t
  checkSerialization (ObjectString t) $ (fromIntegral n .|. 0b10100000) : ByteString.unpack bs

-- Str8

genStr8 :: MonadGen m => m Text
genStr8 = genStr $ Range.linear (2^5) (2^8 - 1)

prop_str8 :: Property
prop_str8 = property $ do
  s <- forAll genStr8
  let (bs, l :: Word8) = decodeTextLength s
  checkSerialization s (0xD9 : (extractWordsBE l ++ ByteString.unpack bs))

prop_obj_str8 :: Property
prop_obj_str8 = property $ do
  s <- forAll genStr8
  let (bs, l :: Word8) = decodeTextLength s
  checkSerialization (ObjectString s) (0xD9 : (extractWordsBE l ++ ByteString.unpack bs))

-- Str16

genStr16 :: MonadGen m => m Text
genStr16 = genStr $ Range.linear (2^8) (2^9 - 1) -- 2^16 - 1 would be too slow.

prop_str16 :: Property
prop_str16 = property $ do
  s <- forAll genStr16
  let (bs, l :: Word16) = decodeTextLength s
  checkSerialization s (0xDA : (extractWordsBE l ++ ByteString.unpack bs))

prop_obj_str16 :: Property
prop_obj_str16 = property $ do
  s <- forAll genStr16
  let (bs, l :: Word16) = decodeTextLength s
  checkSerialization (ObjectString s) (0xDA : (extractWordsBE l ++ ByteString.unpack bs))

-- Bin8

genBin8 :: MonadGen m => m ByteString
genBin8 = Gen.bytes $ Range.linear 0 (2^8 - 1)

prop_bin8 :: Property
prop_bin8 = property $ do
  b <- forAll genBin8
  let l = fromIntegral (ByteString.length b) :: Word8
  checkSerialization b (0xC4 : (extractWordsBE l ++ ByteString.unpack b))

prop_obj_bin8 :: Property
prop_obj_bin8 = property $ do
  b <- forAll genBin8
  let l = fromIntegral (ByteString.length b) :: Word8
  checkSerialization (ObjectBinary b) (0xC4 : (extractWordsBE l ++ ByteString.unpack b))

-- Bin16

genBin16 :: MonadGen m => m ByteString
genBin16 = Gen.bytes $ Range.linear (2^8) (2^9 - 1) -- 2^16 - 1 would be too slow.

prop_bin16 :: Property
prop_bin16 = property $ do
  b <- forAll genBin16
  let l = fromIntegral (ByteString.length b) :: Word16
  checkSerialization b (0xC5 : (extractWordsBE l ++ ByteString.unpack b))

prop_obj_bin16 :: Property
prop_obj_bin16 = property $ do
  b <- forAll genBin16
  let l = fromIntegral (ByteString.length b) :: Word16
  checkSerialization (ObjectBinary b) (0xC5 : (extractWordsBE l ++ ByteString.unpack b))


genObject :: MonadGen m => m Object
genObject =
  Gen.frequency [ (1, ObjectBinary <$> genBin8)
                , (1, pure ObjectNil)
                , (1, ObjectBool <$> Gen.bool)
                , (1, ObjectString <$> genFixStr)
                , (1, ObjectString <$> genStr8)
                , (1, ObjectBinary <$> genBin8)
                , (1, ObjectInt <$> genInt16)
                , (1, ObjectUInt <$> genWord16)
                ]

genObjectWithSize :: MonadGen m => m (Maybe (Object, Int64))
genObjectWithSize = do
  obj <- genObject
  case msgPackSize obj of
    Just size -> return $ Just (obj, size)
    Nothing   -> return Nothing

serializeObj :: Object -> Int64 -> ByteString
serializeObj obj size =
  runPacking (fromIntegral size) (toMsgPack obj)

genObjectPairWithSize :: MonadGen m => m (Maybe ((Object, Int64), (Object, Int64)))
genObjectPairWithSize = do
  a <- genObjectWithSize
  b <- genObjectWithSize
  return $ (,) <$> a <*> b

prop_array :: Property
prop_array = property $ do
  objsWithSize <- catMaybes <$> (forAll $ Gen.list (Range.linear (2^4) (2^6)) genObjectWithSize)
  let objs = map fst objsWithSize
      n = fromIntegral (length objs) :: Int16
      objsSerialized = ByteString.unpack . mconcat . map (uncurry serializeObj)  $ objsWithSize
  checkSerialization (ObjectArray objs) (0xDC : (extractWordsBE n ++ objsSerialized))

serializeObjPair :: (Object, Int64) -> (Object, Int64) -> ByteString
serializeObjPair (obj0, size0) (obj1, size1) =
  runPacking (fromIntegral (size0 + size1)) (toMsgPack obj0 >> toMsgPack obj1)

filterDuplicates :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
filterDuplicates = Map.toList . Map.fromList

prop_fixmap :: Property
prop_fixmap = property $ do
  objsWithSize <- filterDuplicates . catMaybes <$> (forAll $ Gen.list (Range.linear 0 15) genObjectPairWithSize)
  let objPairs = map (\(a, b) -> (fst a, fst b)) objsWithSize
      n = fromIntegral (length objPairs) :: Int16
      objsSerialized = ByteString.unpack . mconcat . map (uncurry serializeObjPair)  $ objsWithSize
  annotate . show $ n
  checkSerialization (ObjectMap (Map.fromList objPairs)) ((0b10000000 .|. (fromIntegral n)) : objsSerialized)

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main =
  tests >>= \case
  True  -> putStrLn "Tests Passed"
  False -> putStrLn "Tests Failed"
