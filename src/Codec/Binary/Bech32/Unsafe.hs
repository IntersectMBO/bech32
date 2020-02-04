-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Unsafe functions for manipulating Bech32 strings.
--
module Codec.Binary.Bech32.Unsafe
    (
      -- * Human-Readable Part
      unsafeHumanReadablePartFromText
    )
    where

import Codec.Binary.Bech32.Internal
