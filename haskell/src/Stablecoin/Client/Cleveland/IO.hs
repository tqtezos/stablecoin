-- SPDX-FileCopyrightText: 2020 tqtezos
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
  , permissionsDescriptorParser
  ) where

import Data.Char (isAlpha, isDigit)
import Fmt (Buildable, pretty, (+|), (|+))
import Michelson.Text (MText, mkMText)
import Morley.Client (Alias)
import qualified Morley.Client as MorleyClient
import Morley.Nettest (MorleyClientConfig(..))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import qualified Text.Megaparsec as P
  (Parsec, customFailure, many, parse, satisfy, skipManyTill, try)
import Text.Megaparsec.Char (newline, printChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ShowErrorComponent(..))
import Tezos.Address (Address, parseAddress)
import Tezos.Core (Mutez, unsafeMkMutez)
import Util.Named ((.!))

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin (OwHook(..), OwHookOptReq(..), PermissionsDescriptor)
import Stablecoin.Client (AddressAndAlias(..))

----------------------------------------------------------------------------
-- CLI
----------------------------------------------------------------------------

callStablecoinClient :: MorleyClientConfig -> [String] -> IO Text
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

morleyClientOpts :: MorleyClientConfig -> [String]
morleyClientOpts (MorleyClientConfig _ap addr port tzPath dataDir useHttps verb key) =
  encodeMaybeOption "--node-url" addr
  <> encodeMaybeOption "--node-port" port
  <> ["--client-path", tzPath]
  <> encodeMaybeOption "--data-dir" dataDir
  <> if useHttps then ["--use-https"] else []
  <> replicate (fromIntegral verb) "-V"
  <> encodeMaybeOption "--secret-key" key

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
  (string "-0 μꜩ" $> unsafeMkMutez 0)
  <|>
  (fmap unsafeMkMutez decimal <* string " μꜩ")

naturalParser :: Parser Natural
naturalParser = decimal

textParser :: Parser Text
textParser = fromString <$> some printChar

mTextParser :: Parser MText
mTextParser =
  textParser <&> mkMText >>= \case
    Left err -> P.customFailure $ OutputParseError "MText" err
    Right mt -> pure mt

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
aliasParser = textParser

addressAndAliasParser :: Text -> Parser AddressAndAlias
addressAndAliasParser label =
  AddressAndAlias
    <$> labelled (label <> " address") addressParser
    <*> optional (P.try (labelled (label <> " alias") aliasParser))

permissionsDescriptorParser :: Parser PermissionsDescriptor
permissionsDescriptorParser = do
  custom <- customPolicyParser
  transferPolicy <- labelled "Transfer policy" transferPolicyParser
  receiver <- labelled "Receiver" ownerTransferModeParser
  sender <- labelled "Sender" ownerTransferModeParser
  pure ((custom, transferPolicy), (receiver, sender))

  where
  customPolicyParser :: Parser (Maybe FA2.CustomPermissionPolicy)
  customPolicyParser =
    optional . P.try $ do
      tag <- labelled "Tag" mTextParser
      configApiMaybe <- optional $ P.try $ labelled "Config API" addressParser
      pure $ (#tag .! tag, #config_api .! configApiMaybe)

  ownerTransferModeParser :: Parser OwHook
  ownerTransferModeParser =
    string "optional_owner_hook" $> OwOptReq OptOH
    <|> string "required_owner_hook" $> OwOptReq ReqOp
    <|> string "owner_no_op" $> OwNoOp

  transferPolicyParser :: Parser FA2.OperatorTransferPolicy
  transferPolicyParser =
    string "owner_transfer" $> FA2.OwnerTransfer (#owner_transfer .! ())
    <|> string "no_transfer" $> FA2.NoTransfer (#no_transfer .! ())
    <|> string "owner_or_operator_transfer" $> FA2.OwnerOrOperatorTransfer (#owner_or_operator_transfer .! ())
