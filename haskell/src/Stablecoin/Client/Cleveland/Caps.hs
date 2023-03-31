-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{-# LANGUAGE InstanceSigs #-}

module Stablecoin.Client.Cleveland.Caps
  ( MonadNetwork
  -- * Operations
  , deploy
  , transfer
  , getBalanceOf
  , updateOperators
  , isOperator
  , pause
  , unpause
  , configureMinter
  , removeMinter
  , mint
  , burn
  , transferOwnership
  , acceptOwnership
  , changeMasterMinter
  , changePauser
  , setTransferlist
  , getBalance
  , getPaused
  , getContractOwner
  , getPendingContractOwner
  , getMasterMinter
  , getPauser
  , getTransferlist
  , getMintingAllowance
  , getTokenMetadata
  , assertEq
  ) where

import Data.Text (isInfixOf)
import Fmt (Buildable(..), pretty)

import Morley.Tezos.Address (ContractAddress, ImplicitAddress, L1Address)
import Morley.Tezos.Core (Mutez)
import Morley.Util.Named (pattern (:!), (:!))
import Test.Cleveland (ToImplicitAddress(..), getMorleyClientEnv, runIO)
import Test.Cleveland.Internal.Abstract (MonadCleveland, MonadNetwork, cmiThrow, getMiscCap)
import Test.Cleveland.Internal.Actions.Helpers (withCap)

import Stablecoin.Client (AddressAndAlias(..), UpdateOperatorData(AddOperator, RemoveOperator))
import Stablecoin.Client.Cleveland.IO
  (OutputParseError(..), addressAndAliasParser, contractAddressParser, encodeMaybeOption, labelled,
  mutezParser, naturalParser, textParser)
import Stablecoin.Client.Cleveland.IO qualified as IO
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

mkUserOpt :: ToImplicitAddress addr => "sender" :! addr -> [String]
mkUserOpt (_ :! sender) = ["--user", pretty $ toImplicitAddress sender]

mkContractOpt :: "contract" :! ContractAddress -> [String]
mkContractOpt (_ :! contract) = ["--contract", pretty contract]


callStablecoinClient :: MonadNetwork caps m => [String] -> m Text
callStablecoinClient x = do
  env <- getMorleyClientEnv
  runIO $ IO.callStablecoinClient env x

runParser :: MonadCleveland caps m => Text -> IO.Parser r -> m r
runParser = runIO ... IO.runParser

deploy
  :: (MonadNetwork caps m, ToImplicitAddress addr)
  => "sender" :! addr -> InitialStorageOptions -> m ContractAddress
deploy sender (InitialStorageOptions {..}) = do
  output <- callStablecoinClient $
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

transfer
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> L1Address -> Natural -> m ()
transfer sender contract from to amount =
  void $ callStablecoinClient $
    [ "transfer"
    , "--from", pretty from
    , "--to", pretty to
    , "--amount", pretty amount
    ] <> mkUserOpt sender <> mkContractOpt contract

getBalanceOf
  :: MonadNetwork caps m
  => "contract" :! ContractAddress
  -> L1Address -> m Natural
getBalanceOf contract owner = do
  output <- callStablecoinClient $
    [ "get-balance-of"
    , "--owner", pretty owner
    ] <> mkContractOpt contract
  runParser output (labelled "Current balance" naturalParser)

updateOperators
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> NonEmpty UpdateOperatorData -> m ()
updateOperators sender contract updateOperators' =
  void $ callStablecoinClient $
    [ "update-operators" ]
    <> (toList updateOperators' >>= \case
          AddOperator operator -> ["--add", pretty operator]
          RemoveOperator operator -> ["--remove", pretty operator]
        )
    <> mkUserOpt sender <> mkContractOpt contract

isOperator
  :: MonadNetwork caps m
  => "contract" :! ContractAddress
  -> L1Address -> L1Address -> m Bool
isOperator contract owner operator = do
  output <- callStablecoinClient $
    [ "is-operator"
    , "--owner", pretty owner
    , "--operator", pretty operator
    ] <> mkContractOpt contract
  if | "This account is an operator" `isInfixOf` output -> pure True
      | "This account is not an operator" `isInfixOf` output -> pure False
      | otherwise ->
          withCap getMiscCap \cap -> cmiThrow cap $ toException $ OutputParseError "is-operator" $
            "Unexpected stablecoin-client output: " <> pretty output

pause :: MonadNetwork caps m => "sender" :! ImplicitAddress -> "contract" :! ContractAddress -> m ()
pause sender contract =
  void $ callStablecoinClient $
    [ "pause" ]
    <> mkUserOpt sender <> mkContractOpt contract

unpause :: MonadNetwork caps m => "sender" :! ImplicitAddress -> "contract" :! ContractAddress -> m ()
unpause sender contract =
  void $ callStablecoinClient $
    [ "unpause" ]
    <> mkUserOpt sender <> mkContractOpt contract

configureMinter
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> Maybe Natural -> Natural -> m ()
configureMinter sender contract minter currentAllowance newAllowance =
  void $ callStablecoinClient $
    [ "configure-minter"
    , "--minter", pretty minter
    , "--new-minting-allowance", pretty newAllowance
    ] <> encodeMaybeOption "--current-minting-allowance" currentAllowance
      <> mkUserOpt sender <> mkContractOpt contract

removeMinter
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
removeMinter sender contract minter =
  void $ callStablecoinClient $
    [ "remove-minter"
    , "--minter", pretty minter
    ] <> mkUserOpt sender <> mkContractOpt contract

mint
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> Natural -> m ()
mint sender contract to amount =
  void $ callStablecoinClient $
    [ "mint"
    , "--to", pretty to
    , "--amount", pretty amount
    ] <> mkUserOpt sender <> mkContractOpt contract

burn
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> NonEmpty Natural -> m ()
burn sender contract amounts =
  void $ callStablecoinClient $
    [ "burn" ]
    <> (toList amounts >>= \amount -> ["--amount", pretty amount])
    <> mkUserOpt sender <> mkContractOpt contract

transferOwnership
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
transferOwnership sender contract to =
  void $ callStablecoinClient $
    [ "transfer-ownership"
    , "--to", pretty to
    ] <> mkUserOpt sender <> mkContractOpt contract

acceptOwnership
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> m ()
acceptOwnership sender contract =
  void $ callStablecoinClient $
    [ "accept-ownership"
    ] <> mkUserOpt sender <> mkContractOpt contract

changeMasterMinter
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
changeMasterMinter sender contract to =
  void $ callStablecoinClient $
    [ "change-master-minter"
    , "--to", pretty to
    ] <> mkUserOpt sender <> mkContractOpt contract

changePauser
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
changePauser sender contract to =
  void $ callStablecoinClient $
    [ "change-pauser"
    , "--to", pretty to
    ] <> mkUserOpt sender <> mkContractOpt contract

setTransferlist
  :: MonadNetwork caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> Maybe ContractAddress -> m ()
setTransferlist sender contract transferlist =
  void $ callStablecoinClient $
    [ "set-transferlist"]
    <> encodeMaybeOption "--transferlist" transferlist
    <> mkUserOpt sender <> mkContractOpt contract

getBalance :: MonadNetwork caps m => "contract" :! ContractAddress -> m Mutez
getBalance contract = do
  output <- callStablecoinClient $ [ "get-balance" ] <> mkContractOpt contract
  runParser output (labelled "Current balance" mutezParser)

getPaused :: MonadNetwork caps m => "contract" :! ContractAddress -> m Bool
getPaused contract = do
  output <- callStablecoinClient $ [ "get-paused" ] <> mkContractOpt contract
  if | "Contract is paused" `isInfixOf` output -> pure True
      | "Contract is not paused" `isInfixOf` output -> pure False
      | otherwise ->
          withCap getMiscCap \cap -> cmiThrow cap $ toException $ OutputParseError "get-paused" $
            "Unexpected stablecoin-client output: " <> pretty output

getContractOwner :: MonadNetwork caps m => "contract" :! ContractAddress -> m AddressAndAlias
getContractOwner contract = do
  output <- callStablecoinClient $ [ "get-contract-owner" ] <> mkContractOpt contract
  runParser output (addressAndAliasParser "Contract owner")

getPendingContractOwner :: MonadNetwork caps m => "contract" :! ContractAddress -> m (Maybe AddressAndAlias)
getPendingContractOwner contract = do
  output <- callStablecoinClient $ [ "get-pending-contract-owner" ] <> mkContractOpt contract
  if "There is no pending contract owner" `isInfixOf` output
    then pure Nothing
    else Just <$> runParser output (addressAndAliasParser "Pending contract owner")

getMasterMinter :: MonadNetwork caps m => "contract" :! ContractAddress -> m AddressAndAlias
getMasterMinter contract = do
  output <- callStablecoinClient $ [ "get-master-minter" ] <> mkContractOpt contract
  runParser output (addressAndAliasParser "Master minter")

getPauser :: MonadNetwork caps m => "contract" :! ContractAddress -> m AddressAndAlias
getPauser contract = do
  output <- callStablecoinClient $ [ "get-pauser" ] <> mkContractOpt contract
  runParser output (addressAndAliasParser "Pauser")

getTransferlist :: MonadNetwork caps m => "contract" :! ContractAddress -> m (Maybe AddressAndAlias)
getTransferlist contract = do
  output <- callStablecoinClient $ [ "get-transferlist" ] <> mkContractOpt contract
  if "Transferlist contract is not set" `isInfixOf` output
    then pure Nothing
    else Just <$> runParser output (addressAndAliasParser "Transferlist contract")

getMintingAllowance :: MonadNetwork caps m => "contract" :! ContractAddress -> L1Address -> m Natural
getMintingAllowance contract minter = do
  output <- callStablecoinClient $
    [ "get-minting-allowance"
    , "--minter", pretty minter
    ] <> mkContractOpt contract
  runParser output (labelled "Minting allowance" naturalParser)

getTokenMetadata
  :: MonadNetwork caps m
  => "contract" :! ContractAddress
  -> m ("symbol" :! Text, "name" :! Text, "decimals" :! Natural)
getTokenMetadata contract = do
  output <- callStablecoinClient $ [ "get-token-metadata" ] <> mkContractOpt contract
  runParser output $
    (,,)
    <$> ((#symbol :!) <$> labelled "Token symbol" textParser)
    <*> ((#name :!) <$> labelled "Token name" textParser)
    <*> ((#decimals :!) <$> labelled "Token decimals" naturalParser)

assertEq :: MonadNetwork caps m => (Eq a, Buildable a, Show a) => a -> a -> m ()
assertEq actual expected =
  if actual == expected
    then pass
    else withCap getMiscCap \cap -> cmiThrow cap $ toException $ STEDiff actual expected
