# packer-messagepack [![Hackage version](https://img.shields.io/hackage/v/packer-messagepack.svg?label=Hackage)](https://hackage.haskell.org/package/packer-messagepack) [![Build Status](https://travis-ci.org/mtesseract/packer-messagepack.svg?branch=master)](https://travis-ci.org/mtesseract/packer-messagepack)

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
  - Int
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
  - Object

  - Furthermore there are instances for

    - lists `[a]`, if the type `a` is an instance of `FromMsgPack`
      resp. `ToMsgPack`.

    - maps `Map k v` if the types `k` and `v` are instances of
      `FromMsgPack` resp. `ToMsgPack`.

### Usage

For example, to serialize a number into a MessagePack encoded
ByteString, use:

```haskell
let n = 2342 :: Int
size <- msgPackSize n
let bytes = runPacking size (toMsgPack n)
```

To deserialize a `ByteString` you can use `fromMsgPack` specialized to
`fromMsgPack :: Unpacking Object` in case the type of the next
MessagePack object is not known. For example:

```haskell
let obj = runUnpacking fromMsgPack bytes :: Object
```

On the other hand, if a specific type is expected, `fromMsgPack` can
be used specialized to the respective type as follows:

```haskell
let n' = runUnpacking fromMsgPack bytes :: Int
```

Note that a MessagePack signed (resp. unsigned) integer can be as big
as an `Int64` (resp. `Word64`). Therefore, if you want to make sure
that there are no overflow problems, use `Int64` (resp. `Word64`)
during deserialization.

### Stackage

Currently, [Packer](https://hackage.haskell.org/package/packer) is not
included in [Stackage](https://www.stackage.org/) yet. Therefore, if
you would like to use this package together with Stackage, you could
pull them in via extra-deps. For example:

```
extra-deps: [packer-VERSION, packer-messagepack-VERSION]
```
