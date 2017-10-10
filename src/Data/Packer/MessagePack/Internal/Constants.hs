{-# LANGUAGE BinaryLiterals #-}

module Data.Packer.MessagePack.Internal.Constants where

import           Data.Word

markerBoolTrue :: Word8
markerBoolTrue = 0xC2

markerBoolFalse :: Word8
markerBoolFalse = 0xC3

markerUInt8 :: Word8
markerUInt8 = 0xCC

markerInt8 :: Word8
markerInt8 = 0xD0

markerUInt16 :: Word8
markerUInt16 = 0xCD

markerInt16 :: Word8
markerInt16 = 0xD1

markerUInt32 :: Word8
markerUInt32 = 0xCE

markerInt32 :: Word8
markerInt32 = 0xD2

markerUInt64 :: Word8
markerUInt64 = 0xCF

markerInt64 :: Word8
markerInt64 = 0xD3

markerNil :: Word8
markerNil = 0xC0

markerBin8 :: Word8
markerBin8 = 0xC4

markerBin16 :: Word8
markerBin16 = 0xC5

markerBin32 :: Word8
markerBin32 = 0xC6

markerFloat32 :: Word8
markerFloat32 = 0xCA

markerFloat64 :: Word8
markerFloat64 = 0xCB

markerFixStr :: Word8
markerFixStr = 0b10100000

markerStr8 :: Word8
markerStr8 = 0xD9

markerStr16 :: Word8
markerStr16 = 0xDA

markerStr32 :: Word8
markerStr32 = 0xDB

markerFixArray :: Word8
markerFixArray = 0b10010000

markerArray16 :: Word8
markerArray16 = 0xDC

markerArray32 :: Word8
markerArray32 = 0xDD

markerFixMap :: Word8
markerFixMap = 0b10000000

markerMap16 :: Word8
markerMap16 = 0xDE

markerMap32 :: Word8
markerMap32 = 0xDF
