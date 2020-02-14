[![Build Status](https://travis-ci.org/input-output-hk/bech32.svg?branch=master)](https://travis-ci.org/input-output-hk/bech32)

# Bech32 Library

The library implements Bech32, an address format specified by
[BIP-0173](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki).

It is a checksummed Base32 format and a standard for native segregated witness
addresses.

## Contents

   * [Documentation](#documentation)
   * [Usage](#usage)
      * [Encoding Data](#encoding-data)
      * [Decoding Data](#decoding-data)
   * [Contributing](#contributing)

## Documentation

For comprehensive instructions on how to use this library, see the [Haddock documentation](https://hackage.haskell.org/package/bech32/docs/Codec-Binary-Bech32.html).

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

Pull requests are welcome! When creating a pull request, please make sure that
your code adheres to our [coding standards](https://github.com/input-output-hk/cardano-wallet/wiki/Coding-Standards).
