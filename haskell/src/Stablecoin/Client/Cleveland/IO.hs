-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Cleveland.IO
  ( -- * Calling the @stablecoin-client@ executable
    callStablecoinClient
  , encodeMaybeOption

   -- * Parsing the @stablecoin-client@'s output
  , Parser
  , OutputParseError(..)
  , runParser
  , labelled
  , mutezParser
  , naturalParser
  , textParser
  , addressParser
  , addressAndAliasParser
  ) where

import Data.Char (isAlpha, isDigit)
import Fmt (Buildable, pretty, (+|), (|+))
import Morley.Client (Alias, MorleyClientEnv, MorleyClientEnv'(..), mkAlias)
import Morley.Client qualified as MorleyClient
import Morley.Client.TezosClient (TezosClientEnv(..))
import Morley.Tezos.Address (Address, parseAddress)
import Morley.Tezos.Core (Mutez, toMutez, zeroMutez)
import Servant.Client (showBaseUrl)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Text.Megaparsec qualified as P
  (Parsec, customFailure, many, parse, satisfy, skipManyTill, try)
import Text.Megaparsec.Char (newline, printChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ShowErrorComponent(..))

import Stablecoin.Client (AddressAndAlias(..))

----------------------------------------------------------------------------
-- CLI
----------------------------------------------------------------------------

callStablecoinClient :: MorleyClientEnv -> [String] -> IO Text
callStablecoinClient conf args = toText <$> do
  let allArgs = args <> morleyClientOpts conf
  readProcessWithExitCode "stablecoin-client" allArgs "N" >>=
    \case
      (ExitSuccess, output, errOutput) ->
        output <$ putErrLn errOutput
      (ExitFailure code, toText -> output, toText -> errOutput) ->
        throwM $ MorleyClient.UnexpectedClientFailure code output errOutput

-- | Write something to stderr.
putErrLn :: Print a => a -> IO ()
putErrLn = hPutStrLn stderr

morleyClientOpts :: MorleyClientEnv -> [String]
morleyClientOpts (MorleyClientEnv tcEnv _ mbSk _) =
  ["--endpoint", showBaseUrl $ tceEndpointUrl tcEnv]
  <> ["--client-path", tceTezosClientPath tcEnv]
  <> encodeMaybeOption "--data-dir" (tceMbTezosClientDataDir tcEnv)
  <> encodeMaybeOption "--secret-key" mbSk

encodeMaybeOption :: Buildable a => String -> Maybe a -> [String]
encodeMaybeOption _ Nothing = []
encodeMaybeOption optName (Just val) = [optName, pretty val]

----------------------------------------------------------------------------
-- Parse stdout
----------------------------------------------------------------------------

type Parser = P.Parsec OutputParseError Text

data OutputParseError = OutputParseError Text Text
  deriving stock (Eq, Show, Ord)

instance Exception OutputParseError where
  displayException = showErrorComponent

instance ShowErrorComponent OutputParseError where
  showErrorComponent (OutputParseError parserName err) =
    "Failed parsing '" +| parserName |+ "\nError: " +| err |+ ""

runParser :: Text -> Parser a -> IO a
runParser str p =
  case P.parse p "" str of
    Right a -> pure a
    Left err -> throwM err

labelled :: Text -> Parser a -> Parser a
labelled label p =
  P.skipManyTill (printChar <|> newline) (string (label <> ": ")) >> p

mutezParser :: Parser Mutez
mutezParser =
  -- Note: There is a known bug in `formatter` that makes `Mutez 0` be printed
  -- as "-0 μꜩ" instead of "0 μꜩ", so we need to handle this special case here.
  -- https://github.com/chrisdone/formatting/pull/61
  -- https://gitlab.com/morley-framework/morley/-/issues/302#note_386133651
  (string "-0 μꜩ" $> zeroMutez)
  <|>
  (fmap (toMutez @Word63) decimal <* string " μꜩ")

naturalParser :: Parser Natural
naturalParser = decimal

textParser :: Parser Text
textParser = fromString <$> some printChar

addressParser :: Parser Address
addressParser = do
  rawAddr <- P.many (P.satisfy isBase58Char)
  case parseAddress (fromString rawAddr) of
    Left err -> P.customFailure $ OutputParseError "address" $ pretty err
    Right addr -> pure addr
  where
    isBase58Char :: Char -> Bool
    isBase58Char c =
      (isDigit c && c /= '0') || (isAlpha c && c /= 'O' && c /= 'I' && c /= 'l')

aliasParser :: Parser Alias
aliasParser = mkAlias <$> textParser

addressAndAliasParser :: Text -> Parser AddressAndAlias
addressAndAliasParser label =
  AddressAndAlias
    <$> labelled (label <> " address") addressParser
    <*> optional (P.try (labelled (label <> " alias") aliasParser))
