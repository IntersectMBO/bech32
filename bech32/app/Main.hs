{-# LANGUAGE NamedFieldPuns #-}

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart
    , dataPartFromBytes
    , dataPartToBytes
    , humanReadablePartFromText
    )
import Control.Arrow
    ( left, right )
import Control.Monad
    ( guard )
import Data.ByteArray.Encoding
    ( convertFromBase, convertToBase )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, unAlphabet )
import Data.Char
    ( isHexDigit, isLetter, isLower, isUpper, toLower )
import Data.Either.Extra
    ( maybeToEither )
import Data.Maybe
    ( fromJust )
import Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , customExecParser
    , eitherReader
    , help
    , helper
    , info
    , metavar
    , optional
    , prefs
    , progDesc
    , showHelpOnEmpty
    , (<|>)
    )
import System.IO
    ( BufferMode (..), Handle, hSetBuffering, stderr, stdin, stdout )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.Internal as Bech32
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = setup >> parse >>= run

newtype Cmd = Cmd
  { prefix :: Maybe HumanReadablePart
  } deriving (Show)

-- | Enable ANSI colors on Windows and correct output buffering
setup :: IO ()
setup =
    mapM_ hSetup [stderr, stdout]
  where
    hSetup :: Handle -> IO ()
    hSetup h = hSetBuffering h NoBuffering

-- | Parse command line options and arguments
parse :: IO Cmd
parse = customExecParser (prefs showHelpOnEmpty) parser
  where
    parser :: ParserInfo Cmd
    parser = info (helper <*> cmd) $ mconcat
        [ progDesc "Simple utility for converting to/from bech32 byte strings."
        ]

    cmd :: Parser Cmd
    cmd = Cmd <$> optional hrpArgument

-- | Parse a 'HumanReadablePart' as an argument.
hrpArgument :: Parser HumanReadablePart
hrpArgument = argument (eitherReader reader) $ mempty
    <> metavar "PREFIX"
    <> help "A human-readable prefix (e.g. 'addr')."
  where
    reader :: String -> Either String HumanReadablePart
    reader = left show . humanReadablePartFromText . T.pack

-- | Run a Command in IO
run :: Cmd -> IO ()
run Cmd{prefix} = do
    source <- T.decodeUtf8 . B8.filter (/= '\n') <$> B8.hGetContents stdin
    case prefix of
        Nothing  -> runDecode source
        Just hrp -> runEncode hrp source
  where
    runDecode source =
        case Bech32.decodeLenient source of
            Left err ->
                fail (show err)
            Right (_, dataPart) -> do
                let base16 = convertToBase BA.Base16
                B8.putStrLn $ base16 $ fromJust $ dataPartToBytes dataPart

    runEncode hrp source = do
        datapart <- either fail pure $
            case detectEncoding (T.unpack source) of
                Just Base16 -> do
                    let fromBase16 = convertFromBase BA.Base16 . T.encodeUtf8
                    dataPartFromBytes <$> fromBase16 source
                Just Bech32 ->
                    right snd $ left show $ Bech32.decodeLenient source
                Just Base58 -> do
                    let err = "Invalid Base58-encoded string."
                    let fromBase58 = decodeBase58 bitcoinAlphabet . T.encodeUtf8
                    dataPartFromBytes <$> maybeToEither err (fromBase58 source)
                Nothing ->
                    fail "Unable to detect input encoding. Neither Base16, \
                         \Bech32 nor Base58."
        B8.putStrLn $ T.encodeUtf8 $ Bech32.encodeLenient hrp datapart

data Encoding = Base16 | Bech32 | Base58 deriving (Show, Eq)

-- | Try detecting the encoding of a given 'String'
detectEncoding :: String -> Maybe Encoding
detectEncoding str
    | length str < minimalSizeForDetection = Nothing
    | otherwise = resembleBase16 <|> resembleBech32 <|> resembleBase58
  where
    resembleBase16 = do
        guard (all isHexDigit (toLower <$> str))
        guard (even (length str))
        pure Base16

    resembleBech32 = do
        guard (not (null humanpart))
        guard (all Bech32.humanReadableCharIsValid humanpart)
        guard (length datapart >= Bech32.checksumLength)
        guard (all (`elem` Bech32.dataCharList) datapart)
        guard (all isUpper alpha || all isLower alpha)
        guard (Bech32.separatorChar `elem` str)
        pure Bech32
      where
        datapart  = reverse . takeWhile (/= Bech32.separatorChar) . reverse $ str
        humanpart = takeWhile (/= Bech32.separatorChar) str
        alpha = filter isLetter str

    resembleBase58 = do
        guard (all isBase58Digit str)
        pure Base58
      where
        isBase58Digit :: Char -> Bool
        isBase58Digit =
            (`elem` T.unpack (T.decodeUtf8 $ unAlphabet bitcoinAlphabet))

-- NOTE For small string, it can be tricky to tell whether a string is hex
-- or bech32 encoded. Both could potentially be valid. As the length
-- increases, the probability for a string to satisfy all three encoding
-- rules gets smaller and smaller.
--
-- For example, let's consider the probability for the alphabet to match
-- between base16 and base58 (which will be bigger than the actual probability
-- of both encoding to be valid, since there are additional rules on top of
-- the alphabet):
--
--     P_1 = 16/58
--
-- Now, the probability that a base58 string of 8 characters will contain
-- only hexadecimal characters is
--
--     P_8 = P_1 ^ 8 ~ 0.00003
--
-- Which can be considered small enough to not happened too frequently. The
-- probability gets worse with Bech32 which has quite a lot of rules.
minimalSizeForDetection :: Int
minimalSizeForDetection = 8
