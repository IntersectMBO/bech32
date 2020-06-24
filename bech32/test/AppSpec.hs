{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AppSpec
    ( spec
    ) where

import Prelude

import Codec.Binary.Bech32
    ( dataPartFromBytes, humanReadablePartFromText )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, encodeBase58 )
import Data.Text
    ( Text )
import System.Process
    ( readProcess )
import Test.Hspec
    ( Spec, describe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..), choose, vector, withMaxSuccess )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, run )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec =
    describe "bech32 command-line" $ do
        specDecode
        specEncode base16
        specEncode (bech32 "bech32")
        specEncode base58

-- | Check that, for a given encoder, any encoded string can be decoded and
-- re-encoded to bech32 using the given prefix.
specEncode
    :: (String -> String)
    -> Spec
specEncode encode = prop ("can re-encode encoded strings " <> encode "...") $
    \(MinString str) ->
        withMaxSuccess 1000 $ monadicIO $ do
            out <- run $ readProcess "bech32" ["prefix"] (encode str)
            assert (init out == bech32 "prefix" str)

-- | Check that any bech32-encoded string can be decoded successfully.
specDecode :: Spec
specDecode = prop "any bech32 string can be decoded to hex" $
    \(MinString str) ->
        withMaxSuccess 1000 $ monadicIO $ do
            out <- run $ readProcess "bech32" [] (bech32 "bech32" str)
            assert (init out == base16 str)

base16 :: String -> String
base16 = fromUtf8 . convertToBase Base16 . utf8

bech32 :: Text -> String -> String
bech32 txt = T.unpack . Bech32.encodeLenient hrp . dataPartFromBytes . utf8
  where
    Right hrp = humanReadablePartFromText txt


base58 :: String -> String
base58 = fromUtf8 . encodeBase58 bitcoinAlphabet . utf8

utf8 :: String -> ByteString
utf8 = T.encodeUtf8 . T.pack

fromUtf8 :: ByteString -> String
fromUtf8 = T.unpack . T.decodeUtf8

-- Generate strings of a minimal length.
newtype MinString = MinString String deriving (Eq, Show)

instance Arbitrary MinString where
    arbitrary =
        MinString <$> (choose (8, 100) >>= vector)
    shrink (MinString str) =
        MinString <$> filter ((> 8) . length) (shrink str)
