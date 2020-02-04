# Bech32 library

The library implements Bech32, an address format specified by
[BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).

It is a checksummed Base32 format and a standard for native segregated witness
output addresses.

# Usage

## Encoding data

```hs
λ import Prelude
λ import Codec.Binary.Bech32
λ import Data.Text.Encoding
λ messageToEncode = "I'm sorry Dave, I'm afraid I can't do that."
λ dataPart = dataPartFromBytes $ encodeUtf8 messageToEncode
λ Right prefix = humanReadablePartFromText "example"
λ encode prefix dataPart
Right "example1fynk6grndae8y7fqg3shvefvypyjwmfqv9n8yctfvssyjgrrv9hzwapqv3hjqargv96zu3pzau9"
```

## Decoding data

```hs
λ import Prelude
λ import Codec.Binary.Bech32
λ import Data.Text.Encoding
λ input = "example1fynk6grndae8y7fqg3shvefvypyjwmfqv9n8yctfvssyjgrrv9hzwapqv3hjqargv96zu3pzau9"
λ Right (prefix, dataPart) = decode input
λ decodeUtf8 <$> dataPartToBytes dataPart
Just "I'm sorry Dave, I'm afraid I can't do that."
```

For more inspiration, have a look at the property tests within the `test`
directory.
