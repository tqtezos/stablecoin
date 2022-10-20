-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Cleveland.StablecoinImpl
  ( StablecoinImpl(..)
  , stablecoinImplClient
  ) where

import Data.Text (isInfixOf)
import Fmt (Buildable(build), pretty)
import Morley.Tezos.Address (ContractAddress, ImplicitAddress, L1Address)
import Morley.Tezos.Core (Mutez)
import Morley.Util.Named (pattern (:!), (:!))
import Test.Cleveland (MorleyClientEnv)
import Test.Cleveland.Internal.Client (ClientM, revealKeyUnlessRevealed)

import Stablecoin.Client (AddressAndAlias(..), UpdateOperatorData(AddOperator, RemoveOperator))
import Stablecoin.Client.Cleveland.IO
  (OutputParseError(..), addressAndAliasParser, callStablecoinClient, contractAddressParser,
  encodeMaybeOption, labelled, mutezParser, naturalParser, runParser, textParser)
import Stablecoin.Client.Contract (InitialStorageOptions(..))

data StablecoinTestError where
  STEDiff :: forall a. (Show a, Buildable a) => a -> a -> StablecoinTestError

deriving stock instance Show StablecoinTestError

instance Buildable StablecoinTestError where
  build (STEDiff actual expected) =
    "--- Diff ---\n" <>
    "Expected: " <> pretty expected <> "\n" <>
    "Actual:   " <> pretty actual

instance Exception StablecoinTestError where
  displayException = pretty

data StablecoinImpl m = StablecoinImpl
  { siDeploy :: "sender" :! ImplicitAddress -> InitialStorageOptions -> m ContractAddress
  , siTransfer
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> L1Address -> Natural -> m ()
  , siGetBalanceOf
      :: "contract" :! ContractAddress
      -> L1Address -> m Natural
  , siUpdateOperators
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> NonEmpty UpdateOperatorData -> m ()
  , siIsOperator
      :: "contract" :! ContractAddress
      -> L1Address -> L1Address -> m Bool
  , siPause :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress -> m ()
  , siUnpause :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress -> m ()
  , siConfigureMinter
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> Maybe Natural -> Natural -> m ()
  , siRemoveMinter
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> m ()
  , siMint
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> Natural -> m ()
  , siBurn
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> NonEmpty Natural -> m ()
  , siTransferOwnership
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> m ()
  , siAcceptOwnership
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> m ()
  , siChangeMasterMinter
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> m ()
  , siChangePauser
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> L1Address -> m ()
  , siSetTransferlist
      :: "sender" :! ImplicitAddress -> "contract" :! ContractAddress
      -> Maybe ContractAddress -> m ()
  , siGetBalance :: "contract" :! ContractAddress -> m Mutez
  , siGetPaused :: "contract" :! ContractAddress -> m Bool
  , siGetContractOwner :: "contract" :! ContractAddress -> m AddressAndAlias
  , siGetPendingContractOwner :: "contract" :! ContractAddress -> m (Maybe AddressAndAlias)
  , siGetMasterMinter :: "contract" :! ContractAddress -> m AddressAndAlias
  , siGetPauser :: "contract" :! ContractAddress -> m AddressAndAlias
  , siGetTransferlist :: "contract" :! ContractAddress -> m (Maybe AddressAndAlias)
  , siGetMintingAllowance :: "contract" :! ContractAddress -> L1Address -> m Natural
  , siGetTokenMetadata
      :: "contract" :! ContractAddress
      -> m ("symbol" :! Text, "name" :! Text, "decimals" :! Natural)
  , siAssertEq :: forall a. (Eq a, Show a, Buildable a) => a -> a -> m ()
  , siRevealKeyUnlessRevealed :: ImplicitAddress -> m ()
  }

-- | Implementation of `StablecoinImpl` that defers to `stablecoin-client`.
stablecoinImplClient :: MorleyClientEnv -> StablecoinImpl ClientM
stablecoinImplClient env = StablecoinImpl
  { siDeploy = \sender (InitialStorageOptions {..}) -> liftIO $ do
      output <- liftIO $ callStablecoinClient env $
        [ "deploy"
        , "--master-minter", pretty isoMasterMinter
        , "--contract-owner", pretty isoContractOwner
        , "--pauser", pretty isoPauser
        , "--transferlist", pretty isoTransferlist
        , "--token-name", pretty isoTokenName
        , "--token-symbol", pretty isoTokenSymbol
        , "--token-decimals", pretty isoTokenDecimals
        , "--default-expiry", pretty isoDefaultExpiry
        , "--replace-alias"
        ] <> mkUserOpt sender
      runParser output (labelled "Contract address" contractAddressParser)
  , siTransfer = \sender contract from to amount -> liftIO $
      void $ callStablecoinClient env $
        [ "transfer"
        , "--from", pretty from
        , "--to", pretty to
        , "--amount", pretty amount
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siGetBalanceOf = \contract owner -> liftIO $ do
      output <- callStablecoinClient env $
        [ "get-balance-of"
        , "--owner", pretty owner
        ] <> mkContractOpt contract
      runParser output (labelled "Current balance" naturalParser)
  , siUpdateOperators = \sender contract updateOperators -> liftIO $
      void $ callStablecoinClient env $
        [ "update-operators" ]
        <> (toList updateOperators >>= \case
              AddOperator operator -> ["--add", pretty operator]
              RemoveOperator operator -> ["--remove", pretty operator]
           )
        <> mkUserOpt sender <> mkContractOpt contract
  , siIsOperator = \contract owner operator -> liftIO $ do
      output <- callStablecoinClient env $
        [ "is-operator"
        , "--owner", pretty owner
        , "--operator", pretty operator
        ] <> mkContractOpt contract
      if | "This account is an operator" `isInfixOf` output -> pure True
         | "This account is not an operator" `isInfixOf` output -> pure False
         | otherwise ->
              throwM $ OutputParseError "is-operator" $
                "Unexpected stablecoin-client output: " <> pretty output
  , siPause = \sender contract -> liftIO $
      void $ callStablecoinClient env $
        [ "pause" ]
        <> mkUserOpt sender <> mkContractOpt contract
  , siUnpause = \sender contract -> liftIO $
      void $ callStablecoinClient env $
        [ "unpause" ]
        <> mkUserOpt sender <> mkContractOpt contract
  , siConfigureMinter = \sender contract minter currentAllowance newAllowance ->
      liftIO $ void $ callStablecoinClient env $
        [ "configure-minter"
        , "--minter", pretty minter
        , "--new-minting-allowance", pretty newAllowance
        ] <> encodeMaybeOption "--current-minting-allowance" currentAllowance
          <> mkUserOpt sender <> mkContractOpt contract
  , siRemoveMinter = \sender contract minter -> liftIO $
      void $ callStablecoinClient env $
        [ "remove-minter"
        , "--minter", pretty minter
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siMint = \sender contract to amount -> liftIO $
      void $ callStablecoinClient env $
        [ "mint"
        , "--to", pretty to
        , "--amount", pretty amount
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siBurn = \sender contract amounts -> liftIO $
      void $ callStablecoinClient env $
        [ "burn" ]
        <> (toList amounts >>= \amount -> ["--amount", pretty amount])
        <> mkUserOpt sender <> mkContractOpt contract
  , siTransferOwnership = \sender contract to -> liftIO $
      void $ callStablecoinClient env $
        [ "transfer-ownership"
        , "--to", pretty to
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siAcceptOwnership = \sender contract -> liftIO $
      void $ callStablecoinClient env $
        [ "accept-ownership"
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siChangeMasterMinter = \sender contract to -> liftIO $
      void $ callStablecoinClient env $
        [ "change-master-minter"
        , "--to", pretty to
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siChangePauser = \sender contract to -> liftIO $
      void $ callStablecoinClient env $
        [ "change-pauser"
        , "--to", pretty to
        ] <> mkUserOpt sender <> mkContractOpt contract
  , siSetTransferlist = \sender contract transferlist -> liftIO $
      void $ callStablecoinClient env $
        [ "set-transferlist"]
        <> encodeMaybeOption "--transferlist" transferlist
        <> mkUserOpt sender <> mkContractOpt contract
  , siGetBalance = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-balance" ] <> mkContractOpt contract
      runParser output (labelled "Current balance" mutezParser)
  , siGetPaused = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-paused" ] <> mkContractOpt contract
      if | "Contract is paused" `isInfixOf` output -> pure True
         | "Contract is not paused" `isInfixOf` output -> pure False
         | otherwise ->
              throwM $ OutputParseError "get-paused" $
                "Unexpected stablecoin-client output: " <> pretty output
  , siGetContractOwner = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-contract-owner" ] <> mkContractOpt contract
      runParser output (addressAndAliasParser "Contract owner")
  , siGetPendingContractOwner = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-pending-contract-owner" ] <> mkContractOpt contract
      if "There is no pending contract owner" `isInfixOf` output
        then pure Nothing
        else Just <$> runParser output (addressAndAliasParser "Pending contract owner")
  , siGetMasterMinter = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-master-minter" ] <> mkContractOpt contract
      runParser output (addressAndAliasParser "Master minter")
  , siGetPauser = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-pauser" ] <> mkContractOpt contract
      runParser output (addressAndAliasParser "Pauser")
  , siGetTransferlist = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-transferlist" ] <> mkContractOpt contract
      if "Transferlist contract is not set" `isInfixOf` output
        then pure Nothing
        else Just <$> runParser output (addressAndAliasParser "Transferlist contract")
  , siGetMintingAllowance = \contract minter -> liftIO $ do
      output <- callStablecoinClient env $
        [ "get-minting-allowance"
        , "--minter", pretty minter
        ] <> mkContractOpt contract
      runParser output (labelled "Minting allowance" naturalParser)
  , siGetTokenMetadata = \contract -> liftIO $ do
      output <- callStablecoinClient env $ [ "get-token-metadata" ] <> mkContractOpt contract
      runParser output $
        (,,)
        <$> ((#symbol :!) <$> labelled "Token symbol" textParser)
        <*> ((#name :!) <$> labelled "Token name" textParser)
        <*> ((#decimals :!) <$> labelled "Token decimals" naturalParser)
  , siAssertEq = \actual expected ->
      if actual == expected
        then pass
        else throwM $ STEDiff actual expected
  , siRevealKeyUnlessRevealed = liftIO . revealKeyUnlessRevealed env
  }
  where
    mkUserOpt :: "sender" :! ImplicitAddress -> [String]
    mkUserOpt (_ :! sender) = ["--user", pretty sender]

    mkContractOpt :: "contract" :! ContractAddress -> [String]
    mkContractOpt (_ :! contract) = ["--contract", pretty contract]
