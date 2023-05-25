<a href="https://hackage.haskell.org/package/bech32"><img src="https://img.shields.io/hackage/v/bech32?style=for-the-badge" /></a>
<a href="https://github.com/input-output-hk/bech32/releases"><img src="https://img.shields.io/github/release/input-output-hk/bech32.svg?style=for-the-badge" /></a>

## `bech32` command-line

A small executable is available for rapid conversion in the console. You can obtain a pre-compiled `bech32` binary for Linux, macOS, and Windows as part of the [`cardano-wallet` Releases](https://github.com/input-output-hk/cardano-wallet/releases).

```console
Usage: bech32 [PREFIX]
  Convert to and from bech32 strings. Data are read from standard input.

Available options:
  -h,--help                Show this help text
  PREFIX                   An optional human-readable prefix (e.g. 'addr').
                             - When provided, the input text is decoded from
                               various encoding formats and re-encoded to
                               bech32 using the given prefix.
                             - When omitted, the input text is decoded from
                               bech32 to base16.
  -v,--version             output version information and exit

Supported encoding formats: Base16, Bech32 & Base58.
```

### Examples

#### To Bech32:

```console
$ bech32 base16_ <<< 706174617465
base16_1wpshgct5v5r5mxh0

$ bech32 base58_ <<< Ae2tdPwUPEYy
base58_1p58rejhd9592uusa8pzj2

$ bech32 new_prefix <<< old_prefix1wpshgcg2s33x3
new_prefix1wpshgcgeak9mv
```

#### From Bech32

```console
$ bech32 <<< base16_1wpshgct5v5r5mxh0
706174617465
```

## Bech32 Haskell Libraries

The repository provides [Haskell](https://www.haskell.org/) libraries for
working with the **Bech32** address format, as specified by
[BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).

There are **two** libraries provided:

* [`bech32`](http://hackage.haskell.org/package/bech32)

    Core primitives for encoding and decoding Bech32 strings.

* [`bech32-th`](http://hackage.haskell.org/package/bech32-th)

    Template-Haskell specific extensions, including quasiquoters
    for compile-time parsing of string literals.

## Contents

   * [Documentation](#documentation)
   * [Usage](#usage)
      * [Encoding Data](#encoding-data)
      * [Decoding Data](#decoding-data)
   * [Contributing](#contributing)

## Documentation

For comprehensive instructions on how to use these libraries, see the Haddock documentation:

* [Documentation for `bech32`](https://hackage.haskell.org/package/bech32/docs/Codec-Binary-Bech32.html)
* [Documentation for `bech32-th`](https://hackage.haskell.org/package/bech32-th/docs/Codec-Binary-Bech32-TH.html)

## Usage

### Encoding Data

```hs
>>> import Prelude
>>> import Codec.Binary.Bech32
>>> import Data.Text.Encoding
```

First, prepare a human-readable prefix:
```hs
>>> Right prefix = humanReadablePartFromText "example"
```

Next, prepare a data payload:
```hs
>>> messageToEncode = "Lorem ipsum dolor sit amet!"
>>> dataPart = dataPartFromBytes $ encodeUtf8 messageToEncode
```

Finally, produce a Bech32 string:
```hs
>>> encode prefix dataPart
Right "example1f3hhyetdyp5hqum4d5sxgmmvdaezqumfwssxzmt9wsss9un3cx"
```

### Decoding Data

```hs
>>> import Prelude
>>> import Codec.Binary.Bech32
>>> import Data.Text.Encoding
```

First, decode the input:

```hs
>>> input = "example1f3hhyetdyp5hqum4d5sxgmmvdaezqumfwssxzmt9wsss9un3cx"
>>> Right (prefix, dataPart) = decode input
```

Next, examine the decoded human-readable prefix:

```hs
>>> humanReadablePartToText prefix
"example"
```

Finally, examine the decoded data payload:

```hs
>>> decodeUtf8 <$> dataPartToBytes dataPart
Just "Lorem ipsum dolor sit amet!"
```

## Contributing

If you find a bug or you'd like to propose a feature, please feel free to raise
an issue on our [issue tracker](https://github.com/input-output-hk/bech32/issues).

Pull requests are welcome!

When creating a pull request, please make sure that your code adheres to our
[coding standards](https://github.com/input-output-hk/cardano-wallet/wiki/Coding-Standards).
