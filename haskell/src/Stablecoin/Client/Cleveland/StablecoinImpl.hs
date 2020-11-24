-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Cleveland.StablecoinImpl
  ( StablecoinImpl(..)
  , stablecoinImplClient
  ) where

import Data.Text (isInfixOf)
import Fmt (Buildable(build), pretty)
import Lorentz (arg)
import Morley.Client (Alias, MorleyClientConfig)
import Morley.Nettest (AddressOrAlias, MorleyClientEnv)
import Morley.Nettest.Client (revealKeyUnlessRevealed)
import Tezos.Address (Address)
import Tezos.Core (Mutez)
import Util.Named ((:!), (.!))

import Stablecoin.Client
  (AddressAndAlias(..), InitialStorageData(..), UpdateOperatorData(AddOperator, RemoveOperator))
import Stablecoin.Client.Cleveland.IO
  (OutputParseError(..), addressAndAliasParser, addressParser, callStablecoinClient,
  encodeMaybeOption, labelled, mutezParser, naturalParser, runParser, textParser)

data StablecoinTestError where
  STEDiff :: forall a. Show a => a -> a -> StablecoinTestError

deriving stock instance Show StablecoinTestError

instance Buildable StablecoinTestError where
  build (STEDiff actual expected) =
    "--- Diff ---\n" <>
    "Expected: " <> show expected <> "\n" <>
    "Actual:   " <> show actual

instance Exception StablecoinTestError where
  displayException = pretty

data StablecoinImpl m = StablecoinImpl
  { siDeploy :: "sender" :! AddressOrAlias -> InitialStorageData AddressOrAlias -> m Address
  , siTransfer
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> AddressOrAlias -> Natural -> m ()
  , siGetBalanceOf
      :: "contract" :! AddressOrAlias
      -> AddressOrAlias -> m Natural
  , siUpdateOperators
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> NonEmpty UpdateOperatorData -> m ()
  , siIsOperator
      :: "contract" :! AddressOrAlias
      -> AddressOrAlias -> AddressOrAlias -> m Bool
  , siPause :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> m ()
  , siUnpause :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> m ()
  , siConfigureMinter
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> Maybe Natural -> Natural -> m ()
  , siRemoveMinter
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> m ()
  , siMint
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> Natural -> m ()
  , siBurn
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> NonEmpty Natural -> m ()
  , siTransferOwnership
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> m ()
  , siAcceptOwnership
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> m ()
  , siChangeMasterMinter
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> m ()
  , siChangePauser
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> AddressOrAlias -> m ()
  , siSetTransferlist
      :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
      -> Maybe AddressOrAlias -> m ()
  , siGetBalance :: "contract" :! AddressOrAlias -> m Mutez
  , siGetPaused :: "contract" :! AddressOrAlias -> m Bool
  , siGetContractOwner :: "contract" :! AddressOrAlias -> m AddressAndAlias
  , siGetPendingContractOwner :: "contract" :! AddressOrAlias -> m (Maybe AddressAndAlias)
  , siGetMasterMinter :: "contract" :! AddressOrAlias -> m AddressAndAlias
  , siGetPauser :: "contract" :! AddressOrAlias -> m AddressAndAlias
  , siGetTransferlist :: "contract" :! AddressOrAlias -> m (Maybe AddressAndAlias)
  , siGetMintingAllowance :: "contract" :! AddressOrAlias -> AddressOrAlias -> m Natural
  , siGetTokenMetadata
      :: "contract" :! AddressOrAlias
      -> m ("symbol" :! Text, "name" :! Text, "decimals" :! Natural)
  , siAssertEq :: forall a. (Eq a, Show a) => a -> a -> m ()
  , siRevealKeyUnlessRevealed :: Alias -> m ()
  }

-- | Implementation of `StablecoinImpl` that defers to `stablecoin-client`.
stablecoinImplClient :: MorleyClientConfig -> MorleyClientEnv -> StablecoinImpl IO
stablecoinImplClient conf env = StablecoinImpl
  { siDeploy = \sender (InitialStorageData {..}) -> do
      output <- callStablecoinClient conf $
        [ "deploy"
        , "--master-minter", pretty isdMasterMinter
        , "--contract-owner", pretty isdContractOwner
        , "--pauser", pretty isdPauser
        , "--transferlist", pretty isdTransferlist
        , "--token-name", pretty isdTokenName
        , "--token-symbol", pretty isdTokenSymbol
        , "--token-decimals", pretty isdTokenDecimals
        , "--default-expiry", pretty isdDefaultExpiry
        , "--replace-alias"
        ] <> mkUserOpt sender
      runParser output (labelled "Contract address" addressParser)
  , siTransfer = \sender contract from to amount ->
      void $ callStablecoinClient conf $
        [ "transfer"
        , "--from", pretty from
        , "--to", pretty to
        , "--amount", pretty amount
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siGetBalanceOf = \contract owner -> do
      output <- callStablecoinClient conf $
        [ "get-balance-of"
        , "--owner", pretty owner
        ] <> mkContractOpt contract
      runParser output (labelled "Current balance" naturalParser)
  , siUpdateOperators = \sender contract updateOperators ->
      void $ callStablecoinClient conf $
        [ "update-operators" ]
        <> (toList updateOperators >>= \case
              AddOperator operator -> ["--add", pretty operator]
              RemoveOperator operator -> ["--remove", pretty operator]
           )
        <> mkUserOpt sender <> mkContractOpt contract
  , siIsOperator = \contract owner operator -> do
      output <- callStablecoinClient conf $
        [ "is-operator"
        , "--owner", pretty owner
        , "--operator", pretty operator
        ] <> mkContractOpt contract
      if | "This account is an operator" `isInfixOf` output -> pure True
         | "This account is not an operator" `isInfixOf` output -> pure False
         | otherwise ->
              throwM $ OutputParseError "is-operator" $
                "Unexpected stablecoin-client output: " <> pretty output
  , siPause = \sender contract ->
      void $ callStablecoinClient conf $
        [ "pause" ]
        <> mkUserOpt sender <> mkContractOpt contract
  , siUnpause = \sender contract ->
      void $ callStablecoinClient conf $
        [ "unpause" ]
        <> mkUserOpt sender <> mkContractOpt contract
  , siConfigureMinter = \sender contract minter currentAllowance newAllowance ->
      void $ callStablecoinClient conf $
        [ "configure-minter"
        , "--minter", pretty minter
        , "--new-minting-allowance", pretty newAllowance
        ] <> encodeMaybeOption "--current-minting-allowance" currentAllowance
          <> mkUserOpt sender <> mkContractOpt contract
  , siRemoveMinter = \sender contract minter ->
      void $ callStablecoinClient conf $
        [ "remove-minter"
        , "--minter", pretty minter
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siMint = \sender contract to amount ->
      void $ callStablecoinClient conf $
        [ "mint"
        , "--to", pretty to
        , "--amount", pretty amount
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siBurn = \sender contract amounts ->
      void $ callStablecoinClient conf $
        [ "burn" ]
        <> (toList amounts >>= \amount -> ["--amount", pretty amount])
        <> mkUserOpt sender <> mkContractOpt contract
  , siTransferOwnership = \sender contract to ->
      void $ callStablecoinClient conf $
        [ "transfer-ownership"
        , "--to", pretty to
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siAcceptOwnership = \sender contract ->
      void $ callStablecoinClient conf $
        [ "accept-ownership"
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siChangeMasterMinter = \sender contract to ->
      void $ callStablecoinClient conf $
        [ "change-master-minter"
        , "--to", pretty to
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siChangePauser = \sender contract to ->
      void $ callStablecoinClient conf $
        [ "change-pauser"
        , "--to", pretty to
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siSetTransferlist = \sender contract transferlist ->
      void $ callStablecoinClient conf $
        [ "set-transferlist"]
        <> encodeMaybeOption "--transferlist" transferlist
        <> mkUserOpt sender <> mkContractOpt contract
  , siGetBalance = \contract -> do
      output <- callStablecoinClient conf $ [ "get-balance" ] <> mkContractOpt contract
      runParser output (labelled "Current balance" mutezParser)
  , siGetPaused = \contract -> do
      output <- callStablecoinClient conf $ [ "get-paused" ] <> mkContractOpt contract
      if | "Contract is paused" `isInfixOf` output -> pure True
         | "Contract is not paused" `isInfixOf` output -> pure False
         | otherwise ->
              throwM $ OutputParseError "get-paused" $
                "Unexpected stablecoin-client output: " <> pretty output
  , siGetContractOwner = \contract -> do
      output <- callStablecoinClient conf $ [ "get-contract-owner" ] <> mkContractOpt contract
      runParser output (addressAndAliasParser "Contract owner")
  , siGetPendingContractOwner = \contract -> do
      output <- callStablecoinClient conf $ [ "get-pending-contract-owner" ] <> mkContractOpt contract
      if "There is no pending contract owner" `isInfixOf` output
        then pure Nothing
        else Just <$> runParser output (addressAndAliasParser "Pending contract owner")
  , siGetMasterMinter = \contract -> do
      output <- callStablecoinClient conf $ [ "get-master-minter" ] <> mkContractOpt contract
      runParser output (addressAndAliasParser "Master minter")
  , siGetPauser = \contract -> do
      output <- callStablecoinClient conf $ [ "get-pauser" ] <> mkContractOpt contract
      runParser output (addressAndAliasParser "Pauser")
  , siGetTransferlist = \contract -> do
      output <- callStablecoinClient conf $ [ "get-transferlist" ] <> mkContractOpt contract
      if "Transferlist contract is not set" `isInfixOf` output
        then pure Nothing
        else Just <$> runParser output (addressAndAliasParser "Transferlist contract")
  , siGetMintingAllowance = \contract minter -> do
      output <- callStablecoinClient conf $
        [ "get-minting-allowance"
        , "--minter", pretty minter
        ] <> mkContractOpt contract
      runParser output (labelled "Minting allowance" naturalParser)
  , siGetTokenMetadata = \contract -> do
      output <- callStablecoinClient conf $ [ "get-token-metadata" ] <> mkContractOpt contract
      runParser output $
        (,,)
        <$> ((#symbol .!) <$> labelled "Token symbol" textParser)
        <*> ((#name .!) <$> labelled "Token name" textParser)
        <*> ((#decimals .!) <$> labelled "Token decimals" naturalParser)
  , siAssertEq = \actual expected ->
      if actual == expected
        then pass
        else throwM $ STEDiff actual expected
  , siRevealKeyUnlessRevealed = revealKeyUnlessRevealed env
  }
  where
    mkUserOpt :: "sender" :! AddressOrAlias -> [String]
    mkUserOpt (arg #sender -> sender) = ["--user", pretty sender]

    mkContractOpt :: "contract" :! AddressOrAlias -> [String]
    mkContractOpt (arg #contract -> contract) = ["--contract", pretty contract]
