# packer-messagepack [![Hackage version](https://img.shields.io/hackage/v/packer-messagepack.svg?label=Hackage)](https://hackage.haskell.org/package/packer-messagepack) [![Stackage version](https://www.stackage.org/package/packer-messagepack/badge/lts?label=Stackage)](https://www.stackage.org/package/packer-messagepack) [![Build Status](https://travis-ci.org/mtesseract/packer-messagepack.svg?branch=master)](https://travis-ci.org/mtesseract/packer-messagepack)

### About

This package provides [MessagePack](http://msgpack.org/index.html)
serialization / deserialization built on top of
[Packer](https://hackage.haskell.org/package/packer).

More precisely, this package exposes the following:

- the type class `ToMsgPack`:

```haskell
class ToMsgPack a where
  toMsgPack :: a -> Packing ()
  msgPackSize :: MonadThrow m => a -> m Int64
```

- the type class `FromMsgPack`:

```haskell
class FromMsgPack a where
  fromMsgPack :: Unpacking a
```

- the type `Object`:

```haskell
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
```

- Instances for the following types:

  - Bool
  - Word8
  - Word16
  - Word32
  - Word64
  - Int8
  - Int16
  - Int32
  - Int64
  - Float
  - Double
  - ByteString
  - Text

  - Furthermore there are instances for

    - lists `[a]`, if the type `a` is an instance of `FromMsgPack`
      resp. `ToMsgPack`.

    - maps `Map k v` if the types `k` and `v` are instances of
      `FromMsgPack` resp. `ToMsgPack`.
