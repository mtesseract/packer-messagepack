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
import           Data.Monoid
import           Data.Packer
import           Data.Packer.MessagePack
import           Data.Proxy
import qualified Data.Text               as Text
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

checkIntegralSerialization :: (Eq a, FromMsgPack a, ToMsgPack a, Bounded a, Show a, Integral a)
                   => (a -> [Word8]) -> Property
checkIntegralSerialization f = property $ do
  a <- forAll $ Gen.integral Range.linearBounded
  checkSerialization a (f a)

checkSerialization :: (Show a, Eq a, FromMsgPack a, ToMsgPack a)
                    => a -> [Word8] -> PropertyT IO ()
checkSerialization a ws = do
  annotate $ show a
  size <- fromIntegral <$> msgPackSize a
  let bsSerialized = runPacking size (toMsgPack a)
      bsExpected   = ByteString.pack ws
  bsSerialized === bsExpected
  annotate $ show bsSerialized
  a === runUnpacking fromMsgPack bsSerialized

nthWord :: (Bits a, Integral a) => Int -> a -> Word8
nthWord = go
  where go 0 a = fromIntegral $ a .&. 0xFF
        go i a = go (i - 1) (a `shiftR` 8)

prop_pos_fixnum :: Property
prop_pos_fixnum = property $ do
  a <- (fromIntegral :: Int8 -> Int64) <$> (forAll $ Gen.int8 Range.linearBounded)
  checkSerialization (ObjectInt (fromIntegral a .&. 0b01111111))
                      [fromIntegral a .&. 0b01111111]

prop_neg_fixnum :: Property
prop_neg_fixnum = property $ do
  a <- (fromIntegral :: Int8 -> Int64) <$> (forAll $ Gen.filter (\x -> -32 <= x && x < 0) $ Gen.int8 Range.linearBounded)
  checkSerialization (ObjectInt (fromIntegral a)) [fromIntegral a]

prop_nil :: Property
prop_nil = property $ do
  nil <- forAll $ Gen.constant ObjectNil
  checkSerialization nil [0xC0]

prop_float32 :: Property
prop_float32 = property $ do
  f <- forAll $ Gen.float (Range.exponentialFloat (- 2^32) (2^32))
  let bs = runPacking 16 (putFloat32BE f)
  checkSerialization f (0xCA : ByteString.unpack bs)

prop_float64 :: Property
prop_float64 = property $ do
  f <- forAll $ Gen.double (Range.exponentialFloat (- 2^64) (2^64))
  let bs = runPacking 16 (putFloat64BE f)
  checkSerialization f (0xCB : ByteString.unpack bs)

extractWordsBE :: forall a. (Integral a, FiniteBits a) => a -> [Word8]
extractWordsBE = go nWords []

  where nWords = (finiteBitSize (undefined :: a) + 7) `div` 8

        go 0 accu _ = accu
        go i accu a =
          let a' = fromIntegral (a `shiftR` 8)
          in go (i - 1)  (fromIntegral (a .&. 0xFF) : accu) a'


checkBytesDeserialization :: forall a. (Num a, Integral a, FiniteBits a)
                          => Word8 -> Int -> Int -> Proxy a -> Property
checkBytesDeserialization marker lowerBound upperBound _ = property $ do
  bs <- forAll $ Gen.bytes (Range.linear lowerBound upperBound)
  let l = extractWordsBE (fromIntegral (ByteString.length bs) :: a)
      d = ByteString.unpack bs
  checkSerialization bs (marker : l ++ d)

prop_bin8 :: Property
prop_bin8 = checkBytesDeserialization 0xC4 0 (2^8 - 1) (Proxy :: Proxy Word8)

checkStringDeserialization :: forall a. (Num a, Integral a, FiniteBits a)
                           => Word8 -> Int -> Int -> Proxy a -> Property
checkStringDeserialization marker lowerBound upperBound _ = property $ do
  t <- forAll $ Gen.text (Range.linear lowerBound (upperBound `div` 2)) (return 'λ')
  let bs = Text.encodeUtf8 t
      l = extractWordsBE (fromIntegral (ByteString.length bs) :: a)
  checkSerialization t (marker : l ++ (ByteString.unpack bs))

prop_fixstr :: Property
prop_fixstr = property $ do
  t <- forAll $ Gen.text (Range.linear 0 ((2^5 - 1) `div` 2)) (return 'λ')
  let bs = Text.encodeUtf8 t
      n = ByteString.length bs
  checkSerialization t $ (fromIntegral n .|. 0b10100000) : ByteString.unpack bs

prop_str8 :: Property
prop_str8 = checkStringDeserialization 0xD9 (2^5) (2^8 - 1) (Proxy :: Proxy Word8)

prop_str16 :: Property
prop_str16 = checkStringDeserialization 0xDA (2^8) (2^10 - 1) (Proxy :: Proxy Word16)

objectSize :: Object -> Int
objectSize (ObjectInt _)     = 10
objectSize (ObjectUInt _)    = 10
objectSize (ObjectBool _)    = 1
objectSize ObjectNil         = 1
objectSize (ObjectString s)  = fromIntegral (Text.length s) + 5
objectSize (ObjectBinary b)  = fromIntegral (ByteString.length b) + 5
objectSize (ObjectFloat32 _) = 5
objectSize (ObjectFloat64 _) = 9
objectSize (ObjectArray a) = 5 + sum (map objectSize a)
objectSize (ObjectMap m) = 5 + (sum . map objectSize . concatMap (\(k,v) -> [k, v]) $ (Map.toList m))

checkObjectDeserialization :: Object -> (ByteString -> t) -> t
checkObjectDeserialization obj predicate = do
  let bs = runPacking (objectSize obj) (toMsgPack obj)
  predicate bs

prop_obj_int_pos_fixnum :: Property
prop_obj_int_pos_fixnum = property $ do
  a <- forAll $ Gen.word8 (Range.linear 0 (2^7 - 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs -> do
    ByteString.unpack bs === [a]

prop_obj_int_neg_fixnum :: Property
prop_obj_int_neg_fixnum = property $ do
  a <- forAll $ Gen.int8 (Range.linear (- 2^5) (- 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs -> do
    let w = head (ByteString.unpack bs)
    ByteString.length bs === 1
    w === fromIntegral a

prop_obj_int8_neg :: Property
prop_obj_int8_neg = property $ do
  a <- forAll $ Gen.int8 (Range.linear (- 2^7) (- 2^5 - 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === [0xD0, fromIntegral a]

prop_obj_int16_pos :: Property
prop_obj_int16_pos = property $ do
  a <- forAll $ Gen.int16 (Range.linear (2^8) (2^15 - 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xD1 : extractWordsBE a

prop_obj_int16_neg :: Property
prop_obj_int16_neg = property $ do
  a <- forAll $ Gen.int16 (Range.linear (- 2^15) (2^5 - 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xD1 : extractWordsBE a

prop_obj_int32_pos :: Property
prop_obj_int32_pos = property $ do
  a <- forAll $ Gen.int32 (Range.linear (2^15) (2^31 - 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xD2 : extractWordsBE a

prop_obj_int32_neg :: Property
prop_obj_int32_neg = property $ do
  a <- forAll $ Gen.int32 (Range.linear (- 2^31) (- 2^15))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xD2 : extractWordsBE a

prop_obj_int64_pos :: Property
prop_obj_int64_pos = property $ do
  a <- forAll $ Gen.int64 (Range.linear (2^31) (2^34 - 1))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xD3 : extractWordsBE a

prop_obj_int64_neg :: Property
prop_obj_int64_neg = property $ do
  a <- forAll $ Gen.int64 (Range.linear (- 2^63) (- 2^31))
  checkObjectDeserialization (ObjectInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xD3 : extractWordsBE a

prop_obj_uint_pos_fixnum :: Property
prop_obj_uint_pos_fixnum = property $ do
  a <- forAll $ Gen.word8 (Range.linear 0 (2^7 - 1))
  checkObjectDeserialization (ObjectUInt (fromIntegral a)) $ \bs -> do
    ByteString.unpack bs === [a]

prop_obj_uint8 :: Property
prop_obj_uint8 = property $ do
  a <- forAll $ Gen.word8 (Range.linear (2^7) (2^8 - 1))
  checkObjectDeserialization (ObjectUInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === [0xCC, fromIntegral a]

prop_obj_int16 :: Property
prop_obj_int16 = property $ do
  a <- forAll $ Gen.word16 (Range.linear (2^8) (2^16 - 1))
  checkObjectDeserialization (ObjectUInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xCD : extractWordsBE a

prop_obj_int32 :: Property
prop_obj_int32 = property $ do
  a <- forAll $ Gen.word32 (Range.linear (2^16) (2^32 - 1))
  checkObjectDeserialization (ObjectUInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xCE : extractWordsBE a

prop_obj_int64 :: Property
prop_obj_int64 = property $ do
  a <- forAll $ Gen.word64 (Range.linear (2^32) (2^64 - 1))
  checkObjectDeserialization (ObjectUInt (fromIntegral a)) $ \bs ->
    ByteString.unpack bs === 0xCF : extractWordsBE a

someObjects :: [Object]
someObjects = [ ObjectNil
              , ObjectBool True
              , ObjectBool False
              , ObjectString "Hello!"
              , ObjectBinary (ByteString.pack (map fromInteger [0..1000]))
              ]
              ++ map ObjectInt [(-10)..10]

randomObject :: MonadGen m => m Object
randomObject = do
  i <- Gen.int (Range.linear 0 (length someObjects - 1))
  return $ someObjects !! i

prop_array :: Property
prop_array = property $ do
  a <- forAll $ Gen.list (Range.linear (2^4) (2^6)) randomObject
  let objsSerialized :: [Word8] = ByteString.unpack . mconcat . map (\o -> runPacking (objectSize o) (toMsgPack o)) $ a
  checkSerialization a (0xDC : (extractWordsBE (fromIntegral (length a) :: Int16)
                                ++ objsSerialized))

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main =
  tests >>= \case
  True  -> putStrLn "Tests Passed"
  False -> putStrLn "Tests Failed"
