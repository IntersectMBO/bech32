{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module contains Template-Haskell-specific extensions to the
-- [Bech32 library](https://github.com/input-output-hk/bech32).

module Codec.Binary.Bech32.TH
    (
      -- ** Quasi-Quotation Support
      humanReadablePart
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartFromText, humanReadablePartToText )
import Control.Exception
    ( throw )
import Data.Text
    ( Text )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Exp, Q )

import qualified Data.Text as T

-- | A quasiquoter for Bech32 human-readable prefixes.
--
-- This quasiquoter makes it possible to construct values of type
-- 'HumanReadablePart' at compile time, using string literals.
--
-- Failure to parse a string literal will result in a __compile-time error__.
--
-- See 'Codec.Binary.Bech32.HumanReadablePartError' for the set of possible
-- errors that can be raised.
--
-- Example:
--
-- >>> :set -XQuasiQuotes
-- >>> import Codec.Binary.Bech32
-- >>> import Codec.Binary.Bech32.TH
-- >>> let addrPrefix = [humanReadablePart|addr|]
-- >>> addrPrefix
-- HumanReadablePart "addr"
-- >>> :t addrPrefix
-- addrPrefix :: HumanReadablePart
--
humanReadablePart :: QuasiQuoter
humanReadablePart = QuasiQuoter
    { quoteExp  = quoteHumanReadablePart
    , quotePat  = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec  = notHandled "declarations"
    }
  where
    notHandled things =
      error $ things <>
          " are not handled by the Bech32 humanReadablePart quasiquoter."

quoteHumanReadablePart :: String -> Q Exp
quoteHumanReadablePart = quote
    . T.unpack
    . humanReadablePartToText
    . unsafeHumanReadablePart
    . T.pack
  where
    quote t = [| unsafeHumanReadablePart t |]

unsafeHumanReadablePart :: Text -> HumanReadablePart
unsafeHumanReadablePart = either throw id . humanReadablePartFromText
