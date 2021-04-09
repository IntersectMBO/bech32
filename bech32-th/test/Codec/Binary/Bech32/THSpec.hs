{-# LANGUAGE BangPatterns #-}

module Codec.Binary.Bech32.THSpec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32
    ( CharPosition (..)
    , HumanReadablePartError (..)
    , humanReadableCharMaxBound
    , humanReadableCharMinBound
    , humanReadablePartMaxLength
    , humanReadablePartMinLength
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import Control.Monad
    ( forM_ )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (quoteExp) )
import Language.Haskell.TH.Syntax
    ( Exp (..), runQ )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy, shouldThrow )

spec :: Spec
spec =
    describe "Quasi-Quotations" $

        describe "Human-Readable Prefixes" $ do
            let mkHumanReadablePartExp = runQ . quoteExp humanReadablePart

            describe "Parsing valid human-readable prefixes should succeed." $
                forM_ validHumanReadableParts $ \hrp ->
                    it (show hrp) $
                        mkHumanReadablePartExp hrp >>=
                            (`shouldSatisfy` isAppE)

            describe "Parsing invalid human-readable prefixes should fail." $
                forM_ invalidHumanReadableParts $ \(hrp, expectedError) ->
                    it (show hrp) $
                        forceViaShowM (mkHumanReadablePartExp hrp)
                            `shouldThrow` (== expectedError)

forceViaShowM :: (Monad m, Show a) => m a -> m a
forceViaShowM f = do
    a <- f
    let !_ = length (show a)
    return a

-- | Matches only function application expressions.
--
isAppE :: Exp -> Bool
isAppE AppE {} = True
isAppE _       = False

-- | A selection of valid human-readable prefixes, that when parsed with the
--   'humanReadablePart' quasiquoter should not result in an exception.
--
-- Note that this is not by any means intended to be an exhaustive list.
-- The underlying parsing logic, provided by `humanReadablePartFromText`,
-- is already tested in the `bech32` package.
--
validHumanReadableParts :: [String]
validHumanReadableParts =
    [ replicate humanReadablePartMinLength humanReadableCharMinBound
    , replicate humanReadablePartMaxLength humanReadableCharMaxBound
    , "addr"
    ]

-- | A selection of invalid human-readable prefixes, along with the errors that
--   we expect to see if we attempt to parse them with the 'humanReadablePart'
--   quasi-quoter.
--
-- Note that this is not by any means intended to be an exhaustive list.
-- The underlying parsing logic, provided by `humanReadablePartFromText`,
-- is already tested in the `bech32` package.
--
invalidHumanReadableParts :: [(String, HumanReadablePartError)]
invalidHumanReadableParts =
    [ ( replicate (pred minLen) minChar
      , HumanReadablePartTooShort
      )
    , ( replicate (succ maxLen) maxChar
      , HumanReadablePartTooLong
      )
    , ( replicate (succ minLen) (pred minChar)
      , HumanReadablePartContainsInvalidChars (CharPosition <$> [0 .. minLen])
      )
    , ( replicate (succ minLen) (succ maxChar)
      , HumanReadablePartContainsInvalidChars (CharPosition <$> [0 .. minLen])
      )
    ]
  where
    minChar = humanReadableCharMinBound
    maxChar = humanReadableCharMaxBound
    minLen = humanReadablePartMinLength
    maxLen = humanReadablePartMaxLength
