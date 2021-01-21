-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains the TZIP-16 metadata and off-chain-views for the
-- @stablecoin.tz@ contract.
module Lorentz.Contracts.Stablecoin.Metadata
  ( MetadataUri(..)
  , ParsedMetadataUri(..)
  , metadataJSON
  , metadataMap
  , parseMetadataUri
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Data.Version (showVersion)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (string')

import Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface
  (Error(..), License(..), Metadata(..), MetadataMap, Source(..), ViewImplementation(..))
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZ
import Morley.Metadata
  (ViewCode(..), ViewCodeError, compileViewCode, compileViewCodeTH, mkMichelsonStorageView)
import Morley.Micheline (ToExpression(toExpression))
import Tezos.Address (formatAddress, parseAddress)

import Lorentz.Contracts.Stablecoin.Types
import Paths_stablecoin (version)
import Stablecoin.Util (ligoVersion)

jfield :: MText
jfield = [mt|metadataJSON|]

metadataMap :: J.ToJSON metadata => MetadataUri metadata -> MetadataMap BigMap
metadataMap mdata = BigMap $ Map.fromList $
  -- One might reasonable expect that the URI would be stored as packed Michelson strings,
  -- but the TZIP-16 spec is explicit about that not being the case.
  --
  -- > Unless otherwise-specified, the encoding of the values must be the direct stream
  -- > of bytes of the data being stored. (...)
  -- > There is no implicit conversion to Michelson's binary format (PACK) nor
  -- > quoting mechanism.
  --
  -- See: <https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md#contract-storage>
  --
  -- So, instead, we encode it as UTF-8 byte sequences.
  case mdata of
    CurrentContract md includeUri ->
      if includeUri
          then [ (mempty, TZ.encodeURI $ TZ.tezosStorageUri (TZ.ContractHost Nothing) jfield)
               , (jfield, BSL.toStrict (J.encode md))
               ]
          else [ (jfield, BSL.toStrict (J.encode md)) ]
    RemoteContract addr ->
      [ (mempty, TZ.encodeURI $ TZ.tezosStorageUri (TZ.ContractHost (Just $ formatAddress addr)) jfield)
      ]
    Raw uri ->
      [ (mempty, encodeUtf8 uri)
      ]


-- Result after parsing the metadata uri from a TZIP-16 metadata bigmap.
data ParsedMetadataUri
  = InCurrentContractUnderKey Text
  | InRemoteContractUnderKey Address Text
  | RawUri Text
  deriving stock (Eq, Show)

parseMetadataUri :: Text -> Either Text ParsedMetadataUri
parseMetadataUri t = first (fromString . P.errorBundlePretty) $ P.parse metadataUriParser "" t

metadataUriParser :: P.Parsec Void Text ParsedMetadataUri
metadataUriParser
  =   (P.try remoteContractUriParser)
  <|> (P.try currentContractUriParser)
  <|> rawUriParser

remoteContractUriParser :: P.Parsec Void Text ParsedMetadataUri
remoteContractUriParser = do
  _ <- string' (TZ.tezosStorageScheme <> "://")
  addr <- P.manyTill P.anySingle (string' "/")
  key <- P.many P.anySingle
  case parseAddress (toText addr) of
    Right paddr -> pure $ InRemoteContractUnderKey paddr (toText key)
    Left err -> fail $ show err

rawUriParser :: P.Parsec Void Text ParsedMetadataUri
rawUriParser = (RawUri . toText) <$> (P.many (P.satisfy (const True)))

currentContractUriParser :: P.Parsec Void Text ParsedMetadataUri
currentContractUriParser = do
  _ <- string' (TZ.tezosStorageScheme <>  ":")
  key_ <- P.many P.anySingle
  pure $ InCurrentContractUnderKey (toText key_)

data MetadataUri metadata
  = CurrentContract metadata Bool -- ^ Metadata and a flag to denote if URI should be included
  | RemoteContract Address
  | Raw Text

-- | Make the TZIP-16 metadata. We accept a @Maybe@ @FA2.TokenMetadata@
-- as argument here so that we can use this function to create the metadata of the
-- FA1.2 Variant as well.
metadataJSON :: Maybe FA2.TokenMetadata -> Maybe Text -> Either ViewCodeError (Metadata (ToT Storage))
metadataJSON mtmd mbDescription = do
  views <- mkViews
  pure $
    TZ.name  "stablecoin" <>
    TZ.description (fromMaybe defaultDescription mbDescription) <>
    TZ.version (toText $ showVersion version) <>
    TZ.license (License { lName = "MIT", lDetails = Nothing }) <>
    TZ.authors
        [ TZ.author "Serokell" "https://serokell.io/"
        , TZ.author "TQ Tezos" "https://tqtezos.com/"
        ] <>
    TZ.homepage "https://github.com/tqtezos/stablecoin/" <>
    TZ.source Source
      { sLocation = Just $ "https://github.com/tqtezos/stablecoin/tree/v" <> toText (showVersion version) <> "/ligo/stablecoin"
      , sTools = [ "ligo " <> $ligoVersion ]
      } <>
    TZ.interfaces [ TZ.Interface "TZIP-012", TZ.Interface "TZIP-017" ] <>
    TZ.errors [ mkError [mt|FA2_TOKEN_UNDEFINED|]        [mt|All `token_id`s must be 0|]
              , mkError [mt|FA2_INSUFFICIENT_BALANCE|]   [mt|Cannot debit from a wallet because of insufficient amount of tokens|]
              , mkError [mt|FA2_NOT_OPERATOR|]           [mt|You're neither the owner or a permitted operator of one or more wallets from which tokens will be transferred|]
              , mkError [mt|XTZ_RECEIVED|]               [mt|Contract received a non-zero amount of tokens|]
              , mkError [mt|NOT_CONTRACT_OWNER|]         [mt|Operation can only be performed by the contract's owner|]
              , mkError [mt|NOT_PENDING_OWNER|]          [mt|Operation can only be performed by the current contract's pending owner|]
              , mkError [mt|NO_PENDING_OWNER_SET|]       [mt|There's no pending transfer of ownership|]
              , mkError [mt|NOT_PAUSER|]                 [mt|Operation can only be performed by the contract's pauser|]
              , mkError [mt|NOT_MASTER_MINTER|]          [mt|Operation can only be performed by the contract's master minter|]
              , mkError [mt|NOT_MINTER|]                 [mt|Operation can only be performed by registered minters|]
              , mkError [mt|CONTRACT_PAUSED|]            [mt|Operation cannot be performed while the contract is paused|]
              , mkError [mt|CONTRACT_NOT_PAUSED|]        [mt|Operation cannot be performed while the contract is not paused|]
              , mkError [mt|NOT_TOKEN_OWNER|]            [mt|You cannot configure another user's operators|]
              , mkError [mt|CURRENT_ALLOWANCE_REQUIRED|] [mt|The given address is already a minter, you must specify its current minting allowance|]
              , mkError [mt|ALLOWANCE_MISMATCH|]         [mt|The given current minting allowance does not match the minter's actual current minting allowance|]
              , mkError [mt|ADDR_NOT_MINTER|]            [mt|This address is not a registered minter|]
              , mkError [mt|ALLOWANCE_EXCEEDED|]         [mt|The amount of tokens to be minted exceeds your current minting allowance|]
              , mkError [mt|BAD_TRANSFERLIST|]           [mt|The given address is a not a smart contract complying with the transferlist interface|]
              , mkError [mt|MINTER_LIMIT_REACHED|]       [mt|Cannot add new minter because the number of minters is already at the limit|]
              , mkError [mt|MISSIGNED|]                  [mt|This permit's signature is invalid|]
              , mkError [mt|EXPIRED_PERMIT|]             [mt|A permit was found, but it has already expired|]
              , mkError [mt|NOT_PERMIT_ISSUER|]          [mt|You're not the issuer of the given permit|]
              , mkError [mt|DUP_PERMIT|]                 [mt|The given permit already exists|]
              , mkError [mt|EXPIRY_TOO_BIG|]             [mt|The `set_expiry` entrypoint was called with an expiry value that is too big|]
              , mkError [mt|NEGATIVE_TOTAL_SUPPLY|]      [mt|The total_supply value was found to be less than zero after an operation. This indicates a bug in the contract.|]
              ] <>
    TZ.views views

  where
    defaultDescription :: Text
    defaultDescription =
      "Tezos Stablecoin project implements an FA2-compatible token smart contract.\
      \ It draws inspiration from popular permissioned asset contracts like CENTRE Fiat Token and other similar contracts.\
      \ The contract is implemented in the LIGO language."

    mkViews :: Either ViewCodeError [TZ.View (ToT Storage)]
    mkViews =
      case mtmd of
        Nothing ->
          pure
            [ getDefaultExpiryView
            , getCounterView
            ]
        Just tmd -> do
          tokenMetadataView <- mkTokenMetadataView tmd
          pure
            [ getDefaultExpiryView
            , getCounterView
            , getBalanceView
            , getTotalSupplyView
            , getAllTokensView
            , isOperatorView
            , tokenMetadataView
            ]

    mkError :: MText -> MText -> Error
    mkError err expansion =
      TZ.EStatic $ TZ.StaticError
        { seError = toExpression (toVal err)
        , seExpansion = toExpression (toVal expansion)
        , seLanguages = ["en"]
        }

type BalanceViewParam = (Address, Natural)

getBalanceView :: TZ.View (ToT Storage)
getBalanceView =
  TZ.View
    { vName = "get_balance"
    , vDescription = Just "Access the balance of an address"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $
            $$(compileViewCodeTH $ WithParam @BalanceViewParam $
              L.dip (L.toField #sLedger) #
              L.car #
              L.get #
              L.whenNone (L.push 0) -- If there is no ledger entry, return zero.
            )
    }

getTotalSupplyView :: TZ.View (ToT Storage)
getTotalSupplyView =
  TZ.View
    { vName = "total_supply"
    , vDescription = Just "Get the total no of tokens available."
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $
            $$(compileViewCodeTH $ WithParam $
              L.int #
              L.assertEq0 [mt|Unknown TOKEN ID|] #
              L.toField #sTotalSupply
            )
    }

getAllTokensView :: TZ.View (ToT Storage)
getAllTokensView =
  TZ.View
    { vName = "all_tokens"
    , vDescription = Just "Get list of token ids supported."
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage Nothing [] $
            $$(compileViewCodeTH $ WithoutParam $
              L.drop # L.nil # L.push (0 :: Natural) # L.cons
            )
    }

isOperatorView :: TZ.View (ToT Storage)
isOperatorView =
  TZ.View
    { vName = "is_operator"
    , vDescription = Just "Check if the given address is an operator"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Bool Nothing [] $
            $$(compileViewCodeTH $ WithParam @FA2.OperatorParam $
              L.dip (L.toField #sOperators) #

              L.getField #opTokenId # forcedCoerce_ #
              L.int #
              L.assertEq0 [mt|Unknown TOKEN ID|] #

              L.getField #opOwner #
              L.dip (L.toField #opOperator) #
              L.pair #
              L.get #
              L.ifSome (L.drop # L.push True) (L.push False)
            )
    }

mkTokenMetadataView :: FA2.TokenMetadata -> Either ViewCodeError (TZ.View (ToT Storage))
mkTokenMetadataView md = do
  vc <- compileViewCode $ WithParam $
          L.dip L.drop #
          L.int #
          L.assertEq0 [mt|Unknown TOKEN ID|] #
          L.push (0 :: Natural, md)
  pure $ TZ.View
    { vName = "token_metadata"
    , vDescription = Just "Get token metadata for the token id"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @(Natural, Map.Map MText ByteString) Nothing [] vc
    }

getDefaultExpiryView :: TZ.View (ToT Storage)
getDefaultExpiryView =
  TZ.View
    { vName = "GetDefaultExpiry"
    , vDescription = Just "Access the contract's default expiry in seconds"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage  @Natural Nothing [] $
            $$(compileViewCodeTH $ WithoutParam $
              L.toField #sDefaultExpiry
            )
    }

getCounterView :: TZ.View (ToT Storage)
getCounterView =
  TZ.View
    { vName = "GetCounter"
    , vDescription = Just "Access the current permit counter"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $
            $$(compileViewCodeTH $ WithoutParam $
              L.toField #sPermitCounter
            )
    }
