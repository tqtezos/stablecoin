-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Impl
  ( AddressAndAlias(..)
  , deploy

  -- * FA2 Entrypopints
  , transfer
  , getBalanceOf
  , UpdateOperatorData(..)
  , updateOperators
  , isOperator

  -- * Stablecoin Entrypoints
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

  -- * Query commands
  , getBalance
  , getPaused
  , getContractOwner
  , getPendingContractOwner
  , getMasterMinter
  , getPauser
  , getTransferlist
  , getTotalSupply
  , getMintingAllowance
  , getTokenMetadata
  ) where

import Fmt (Buildable(build), pretty, (+|), (|+))
import Lorentz (EntrypointRef(Call), HasEntrypointArg, arg, useHasEntrypointArg)
import Michelson.Typed (Dict(..), IsoValue, fromVal, toVal)
import qualified Michelson.Typed as T
import Morley.Client
  (AddressOrAlias(..), Alias, BigMapId(..), MorleyClientM, TezosClientError(UnknownAddress),
  getAlias, getContractScript, lTransfer, originateUntypedContract, readBigMapValue,
  readBigMapValueMaybe, resolveAddress)
import qualified Morley.Client as Client
import Morley.Client.RPC (OriginationScript(OriginationScript))
import Morley.Micheline (Expression, FromExpressionError, fromExpression)
import Tezos.Address (Address)
import Tezos.Core (Mutez, unsafeMkMutez)
import Util.Named ((:!), (.!))

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin
  (pattern MasterMinterRole, pattern OwnerRole, Parameter, pattern PauserRole,
  pattern PendingOwnerRole, TokenMetadata)
import Stablecoin.Client.Contract
  (InitialStorageData(..), mkInitialStorage, parseStablecoinContract)

-- | An address and an optional alias, if one is found.
data AddressAndAlias = AddressAndAlias Address (Maybe Alias)
  deriving stock (Show, Eq)

-- | Data needed to add or remove an operator.
data UpdateOperatorData
  = AddOperator AddressOrAlias
  | RemoveOperator AddressOrAlias
  deriving stock Show

-- | Deploy the stablecoin contract.
-- Returns the operation hash and the contract's new address.
--
-- Saves the contract with the given alias.
-- If the given alias already exists, nothing happens.
deploy :: "sender" :! AddressOrAlias -> Text -> InitialStorageData AddressOrAlias -> MorleyClientM (Text, Address)
deploy (arg #sender -> sender) alias initialStorageData = do
  stablecoinContract <- parseStablecoinContract
  masterMinter <- resolveAddress (isdMasterMinter initialStorageData)
  contractOwner <- resolveAddress (isdContractOwner initialStorageData)
  pauser <- resolveAddress (isdPauser initialStorageData)
  transferlist <- traverse resolveAddress (isdTransferlist initialStorageData)

  let initialStorageData' = initialStorageData
        { isdMasterMinter = masterMinter
        , isdContractOwner = contractOwner
        , isdPauser = pauser
        , isdTransferlist = transferlist
        }

  let initialStorage = mkInitialStorage initialStorageData'

  originateUntypedContract
    False
    alias
    sender
    (unsafeMkMutez 0)
    stablecoinContract
    (T.untypeValue $ toVal initialStorage)

transfer
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> Natural -> MorleyClientM ()
transfer sender contract from to amount = do
  fromAddr <- resolveAddress from
  toAddr <- resolveAddress to
  call sender contract (Call @"Transfer")
    [(#from_ .! fromAddr, #txs .! [(#to_ .! toAddr, (#token_id .! 0, #amount .! amount))])]

getBalanceOf
  :: "contract" :! AddressOrAlias
  -> AddressOrAlias -> MorleyClientM Natural
getBalanceOf contract owner = do
  ownerAddr <- resolveAddress owner
  (((arg #ledger -> bigMapId, _ ), _), _) <- getStorage contract
  balanceMaybe <- readBigMapValueMaybe bigMapId ownerAddr
  pure $ fromMaybe 0 balanceMaybe

updateOperators
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> NonEmpty UpdateOperatorData -> MorleyClientM ()
updateOperators (arg #sender -> sender) contract ops = do
  senderAddr <- resolveAddress sender
  -- Note: As per the specification:
  -- "In each update_operator item `owner` MUST be equal to `SENDER`"
  let ownerAddr = senderAddr
  param <- traverse (mkUpdateOperator ownerAddr) (toList ops)
  call (#sender .! AddressResolved senderAddr) contract (Call @"Update_operators") param
  where
    mkUpdateOperator :: Address -> UpdateOperatorData -> MorleyClientM FA2.UpdateOperator
    mkUpdateOperator ownerAddr = \case
      AddOperator op -> FA2.Add_operator <$> mkOperatorParam ownerAddr op
      RemoveOperator op -> FA2.Remove_operator <$> mkOperatorParam ownerAddr op

    mkOperatorParam :: Address -> AddressOrAlias -> MorleyClientM FA2.OperatorParam
    mkOperatorParam ownerAddr operator =
      resolveAddress operator <&> \operatorAddr -> (#owner .! ownerAddr, #operator .! operatorAddr)

isOperator
  :: "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> MorleyClientM Bool
isOperator contract owner operator = do
  ownerAddr <- resolveAddress owner
  operatorAddr <- resolveAddress operator
  ((_, (arg #operators -> operatorsId, _)), _) <- getStorage contract
  isJust @() <$> readBigMapValueMaybe operatorsId (ownerAddr, operatorAddr)

-- | Pauses transferring, burning and minting operations so that they
-- cannot be performed. All other operations remain unaffected.
pause :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> MorleyClientM ()
pause sender contract = call sender contract (Call @"Pause") ()

-- | Unpauses the contract so that transferring, burning and minting
-- operations can be performed by users with corresponding roles.
unpause :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> MorleyClientM ()
unpause sender contract = call sender contract (Call @"Unpause") ()

-- | Adds a minter to the minter list to allow them to mint tokens
-- (if minter is not in the list already).
configureMinter
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> Maybe Natural -> Natural -> MorleyClientM ()
configureMinter sender contract minter currentMintingAllowance newMintingAllowance = do
  minterAddr <- resolveAddress minter
  call sender contract (Call @"Configure_minter")
    ( #minter .! minterAddr
    , ( #current_minting_allowance .! currentMintingAllowance
      , #new_minting_allowance .! newMintingAllowance
      )
    )

-- | Removes a minter from the minter list and sets its minting allowance to 0.
-- Once minter is removed it will no longer be able to mint or burn tokens.
removeMinter
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> MorleyClientM ()
removeMinter sender contract minter = do
  minterAddr <- resolveAddress minter
  call sender contract (Call @"Remove_minter") minterAddr

-- | Produces the given amounts of tokens to the wallets associated
-- with the given addresses.
mint
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> Natural -> MorleyClientM ()
mint sender contract to amount = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Mint") [(#to_ .! toAddr, #amount .! amount)]

-- | Decreases balance for sender and the total supply of tokens by
-- the sum of given amounts.
burn
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> NonEmpty Natural -> MorleyClientM ()
burn sender contract amounts =
  call sender contract (Call @"Burn") (toList amounts)

-- | Initiate transfer of contract ownership to a new address.
transferOwnership
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> MorleyClientM ()
transferOwnership sender contract to = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Transfer_ownership") toAddr

-- | Accept contract ownership privileges.
acceptOwnership
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> MorleyClientM ()
acceptOwnership sender contract =
  call sender contract (Call @"Accept_ownership") ()

-- | Set master minter to a new address.
changeMasterMinter
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> MorleyClientM ()
changeMasterMinter sender contract to = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Change_master_minter") toAddr

-- | Set pauser to a new address.
changePauser
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> MorleyClientM ()
changePauser sender contract to = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Change_pauser") toAddr

-- | Set the stored (optional) transferlist address to the new one.
setTransferlist
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> Maybe AddressOrAlias -> MorleyClientM ()
setTransferlist sender contract transferlist = do
  transferlistAddr <- traverse resolveAddress transferlist
  call sender contract (Call @"Set_transferlist") transferlistAddr

-- | Retrieve the contract's balance.
getBalance :: "contract" :! AddressOrAlias -> MorleyClientM Mutez
getBalance (arg #contract -> contract) = do
  contractAddr <- resolveAddress contract
  Client.getBalance contractAddr

-- | Check whether the contract has been paused.
getPaused :: "contract" :! AddressOrAlias -> MorleyClientM Bool
getPaused contract =
  getStorage contract <&> \case
    ((_, (_, arg #paused -> paused)), _) -> paused

-- | Get the address and optional alias of the current contract owner.
getContractOwner :: "contract" :! AddressOrAlias -> MorleyClientM AddressAndAlias
getContractOwner contract =
  getStorage contract >>= \case
    StorageViewRoles (OwnerRole ownerAddr) -> pairWithAlias ownerAddr

-- | Check if there's an ownership transfer pending acceptance and, if so,
-- get its pending owner's address and optional alias.
getPendingContractOwner :: "contract" :! AddressOrAlias -> MorleyClientM (Maybe AddressAndAlias)
getPendingContractOwner contract =
  getStorage contract >>= \case
    StorageViewRoles (PendingOwnerRole ownerAddr) -> traverse pairWithAlias ownerAddr

-- | Get the address and optional alias of the master minter.
getMasterMinter :: "contract" :! AddressOrAlias -> MorleyClientM AddressAndAlias
getMasterMinter contract =
  getStorage contract >>= \case
    StorageViewRoles (MasterMinterRole masterMinterAddr) -> pairWithAlias masterMinterAddr

-- | Get the address and optional alias of the pauser.
getPauser :: "contract" :! AddressOrAlias -> MorleyClientM AddressAndAlias
getPauser contract =
  getStorage contract >>= \case
    StorageViewRoles (PauserRole pauserAddr) -> pairWithAlias pauserAddr

-- | Check if a transferlist is set and, if so, get its address and optional alias.
getTransferlist :: "contract" :! AddressOrAlias -> MorleyClientM (Maybe AddressAndAlias)
getTransferlist contract =
  getStorage contract >>= \case
    (_, (_, (_, arg #transferlist_contract -> transferlistMb))) -> traverse pairWithAlias transferlistMb

-- | Get the total supply of tokens.
getTotalSupply :: "contract" :! AddressOrAlias -> MorleyClientM Natural
getTotalSupply contract =
  getStorage contract <&> \case
    (_, (_, (arg #total_supply -> totalSupply, _))) -> totalSupply

-- | Get the minting allowance for the given minter.
getMintingAllowance :: "contract" :! AddressOrAlias -> AddressOrAlias -> MorleyClientM Natural
getMintingAllowance contract minter = do
  minterAddr <- resolveAddress minter
  (((_, arg #minting_allowances -> bigMapId), _), _) <- getStorage contract
  allowanceMaybe <- readBigMapValueMaybe bigMapId minterAddr
  pure $ fromMaybe 0 allowanceMaybe

-- | Get the minting allowance for the given minter.
getTokenMetadata :: "contract" :! AddressOrAlias -> MorleyClientM TokenMetadata
getTokenMetadata contract = do
  (_, ((_, arg #token_metadata -> bigMapId), _)) <- getStorage contract
  readBigMapValue bigMapId (0 :: FA2.TokenId)

-- | Check if there's an alias associated with this address and, if so, return both.
pairWithAlias :: Address -> MorleyClientM AddressAndAlias
pairWithAlias addr = do
  aliasMb <- (Just <$> getAlias (AddressResolved addr)) `catch` \case
    UnknownAddress _ -> pure Nothing
    err -> throwM err
  pure $ AddressAndAlias addr aliasMb

-- | Call an entrypoint of the stablecoin contract with the given argument.
call
  :: forall epRef epArg.
     (HasEntrypointArg Parameter epRef epArg, IsoValue epArg, Typeable epArg)
  => "sender" :! AddressOrAlias
  -> "contract" :! AddressOrAlias
  -> epRef
  -> epArg
  -> MorleyClientM ()
call (arg #sender -> sender) (arg #contract -> contract) epRef epArg =
  case useHasEntrypointArg @Parameter @epRef @epArg epRef of
    (Dict, epName) -> do
      senderAddr <- resolveAddress sender
      contractAddr <- resolveAddress contract
      void $ lTransfer
        senderAddr
        contractAddr
        (unsafeMkMutez 0)
        epName
        epArg

-- | Get the contract's storage.
getStorage :: "contract" :! AddressOrAlias -> MorleyClientM StorageView
getStorage (arg #contract -> contract) = do
  contractAddr <- resolveAddress contract
  OriginationScript _ storageExpr <- getContractScript contractAddr
  case fromVal @StorageView <$> fromExpression storageExpr of
    Right storage -> pure storage
    Left err -> throwM $ SCEExpressionParseError storageExpr err

------------------------------------------------------------------
--- Exceptions
------------------------------------------------------------------

data StablecoinClientError
  = SCEExpressionParseError Expression FromExpressionError

deriving stock instance Show StablecoinClientError

instance Buildable StablecoinClientError where
  build (SCEExpressionParseError expr err) =
    "Failed to parse expression:\n" +|
    expr |+ "\n" <>
    "Parse error: " +| err |+ ""

instance Exception StablecoinClientError where
  displayException = pretty

------------------------------------------------------------------
--- Storage
------------------------------------------------------------------

-- | 'StorageView' is very similar to 'Lorentz.Contracts.Stablecoin.Storage',
-- except 'BigMap's have been replaced by 'BigMapId'.
-- This is because, when a contract's storage is queried, the tezos RPC returns
-- big_maps' IDs instead of their contents.
type StorageView =
  (((Ledger, MintingAllowances), (Operators, IsPaused))
   , ((Roles, TokenMetadataBigMap), (TotalSupply, TransferlistContract)))

type Ledger = "ledger" :! BigMapId Address Natural

type Operators = "operators" :! BigMapId (Address, Address) ()
type TotalSupply = "total_supply" :! Natural
type IsPaused = "paused" :! Bool

type MasterMinter = Address
type Owner = Address
type PendingOwner = Maybe Address
type Pauser = Address

type MintingAllowances = "minting_allowances" :! BigMapId Address Natural

type RolesInner = (("master_minter" :! MasterMinter, "owner" :! Owner)
             , ("pauser" :! Pauser, "pending_owner_address" :! PendingOwner))

type Roles = "roles" :! RolesInner

type TransferlistContract = "transferlist_contract" :! (Maybe Address)

type TokenMetadataBigMap = "token_metadata" :! BigMapId FA2.TokenId TokenMetadata

pattern StorageViewRoles :: RolesInner -> StorageView
pattern StorageViewRoles roles <- (_ , ((arg #roles -> roles, _), _))
{-# COMPLETE StorageViewRoles #-}
