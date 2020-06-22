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
    ( isLetter, isLower, isUpper, ord, toLower )
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
          Right (_, dataPart) ->
              B8.putStrLn $ convertToBase BA.Base16 $ fromJust $
                dataPartToBytes dataPart

    runEncode hrp source = do
        datapart <- either fail pure $
            case detectEncoding (T.unpack source) of
                Just Base16 -> do
                    let fromBase16 = convertFromBase BA.Base16 . T.encodeUtf8
                    dataPartFromBytes <$> fromBase16 source
                Just Bech32 ->
                    right snd $ left show $ Bech32.decodeLenient source
                Just Base58 -> do
                    let maybeToEither = maybe (Left "Invalid Base58-encoded string.") Right
                    let fromBase58 = decodeBase58 bitcoinAlphabet . T.encodeUtf8
                    dataPartFromBytes <$> maybeToEither (fromBase58 source)
                Nothing ->
                    fail "Unable to detect input encoding. Neither Base16, Bech32 nor Base58."
        B8.putStrLn $ T.encodeUtf8 $ Bech32.encodeLenient hrp datapart

data Encoding = Base16 | Bech32 | Base58 deriving (Show, Eq)

-- | Try detecting the encoding of a given 'String'
detectEncoding :: String -> Maybe Encoding
detectEncoding str = isBase16 <|> isBech32  <|> isBase58
  where
    isBase16 = do
        guard (all (`elem` "0123456789abcdef") (toLower <$> str))
        guard (even (length str))
        pure Base16

    isBech32 = do
        guard (not (null humanpart))
        guard (all (\c -> ord c >= 33 && ord c <= 126) humanpart)
        guard (length datapart >= 6)
        guard (all (`elem` Bech32.dataCharList) datapart)
        guard (all isUpper alpha || all isLower alpha)
        pure Bech32
      where
        datapart  = reverse . takeWhile (/= '1') . reverse $ str
        humanpart = takeWhile (/= '1') str
        alpha = filter isLetter str

    isBase58 = do
        guard (all (`elem` T.unpack (T.decodeUtf8 $ unAlphabet bitcoinAlphabet)) str)
        pure Base58
