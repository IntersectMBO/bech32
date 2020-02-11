-- |
-- Copyright: © 2017 Marko Bencun, 2018-2019 IOHK
-- License: Apache-2.0
--
-- Implementation of the [Bech32]
-- (https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
-- address format.
--
-- Based on an [original implementation](https://github.com/sipa/bech32/tree/bdc264f84014c234e908d72026b7b780122be11f/ref/haskell)
-- by [Marko Bencun](https://github.com/sipa).

module Codec.Binary.Bech32
    (
    -- * Encoding
      encode

    -- * Decoding
    , decode

    -- * Error handling
    , EncodingError (..)
    , DecodingError (..)

    -- * Encoding data payloads
    , DataPart
    , dataPartFromBytes
    , dataPartFromText
    , dataPartToBytes
    , dataPartToText

    -- * Encoding human-readable prefixes
    , HumanReadablePart
    , HumanReadablePartError (..)
    , humanReadablePartFromText
    , humanReadablePartToText

    -- * Encoding with greater leniency
    , encodeLenient
    , decodeLenient

    -- * Constants
    , dataCharList
    , encodedStringMaxLength
    , encodedStringMinLength
    , humanReadablePartMinLength
    , humanReadablePartMaxLength
    , humanReadableCharMinBound
    , humanReadableCharMaxBound
    , separatorChar
    ) where

import Codec.Binary.Bech32.Internal
