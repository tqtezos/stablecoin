-- SPDX-FileCopyrightText: 2020 TQ Tezos
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
  , getMintingAllowance
  , getTokenMetadata
  ) where

import qualified Data.Map as M
import Fmt (Buildable(build), pretty, (+|), (|+))
import Lorentz (EntrypointRef(Call), HasEntrypointArg, arg, useHasEntrypointArg)
import Michelson.Typed (Dict(..), IsoValue, fromVal, toVal)
import Morley.Client
  (AddressOrAlias(..), Alias, AliasHint(..), MorleyClientM, TezosClientError(UnknownAddress),
  getAlias, getContractScript, lTransfer, originateContract, readBigMapValue, readBigMapValueMaybe)
import qualified Morley.Client as Client
import Morley.Client.RPC (OriginationScript(OriginationScript))
import Morley.Client.TezosClient (resolveAddress)
import Morley.Micheline (Expression, FromExpressionError, fromExpression)
import Tezos.Address (Address)
import Tezos.Core (Mutez, unsafeMkMutez)
import Util.Named ((:!), (.!))

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin
  (ConfigureMinterParam(..), MetadataRegistryStorage, MetadataRegistryStorageView, MintParam(..),
  Parameter, Roles(..), Storage'(..), StorageView, mkMetadataRegistryStorage, mrsTokenMetadata,
  registryContract, stablecoinContract)
import Stablecoin.Client.Contract (InitialStorageData(..), mkInitialStorage)

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
deploy :: "sender" :! AddressOrAlias -> AliasHint -> InitialStorageData AddressOrAlias -> MorleyClientM (Text, Address, Address)
deploy (arg #sender -> sender) alias initialStorageData = do
  masterMinter <- resolveAddress (isdMasterMinter initialStorageData)
  contractOwner <- resolveAddress (isdContractOwner initialStorageData)
  pauser <- resolveAddress (isdPauser initialStorageData)
  transferlist <- traverse resolveAddress (isdTransferlist initialStorageData)

  -- deploy metadata registry contract
  metadataRegistry <- case isdTokenMetadataRegistry initialStorageData of
    Just mdr -> resolveAddress mdr
    Nothing -> do
      let registryStorage :: MetadataRegistryStorage = mkMetadataRegistryStorage
            (isdTokenSymbol initialStorageData)
            (isdTokenName initialStorageData)
            (isdTokenDecimals initialStorageData)
      snd <$> originateContract
        False
        "stablecoin-metadata"
        sender
        (unsafeMkMutez 0)
        registryContract
        (toVal registryStorage)

  let initialStorageData' = initialStorageData
        { isdMasterMinter = masterMinter
        , isdContractOwner = contractOwner
        , isdPauser = pauser
        , isdTransferlist = transferlist
        , isdTokenMetadataRegistry = metadataRegistry
        }

  let initialStorage = mkInitialStorage initialStorageData'

  (cName, cAddr) <- originateContract
    False
    alias
    sender
    (unsafeMkMutez 0)
    stablecoinContract
    (toVal initialStorage)
  pure (cName, cAddr, metadataRegistry)

transfer
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> Natural -> MorleyClientM ()
transfer sender contract from to amount = do
  fromAddr <- resolveAddress from
  toAddr <- resolveAddress to
  call sender contract (Call @"Transfer")
    [FA2.TransferParam fromAddr [FA2.TransferDestination toAddr 0 amount]]

getBalanceOf
  :: "contract" :! AddressOrAlias
  -> AddressOrAlias -> MorleyClientM Natural
getBalanceOf contract owner = do
  ownerAddr <- resolveAddress owner
  ledgerId <- sLedger <$> getStorage contract
  balanceMaybe <- readBigMapValueMaybe ledgerId ownerAddr
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
      resolveAddress operator <&> \operatorAddr -> FA2.OperatorParam ownerAddr operatorAddr 0

isOperator
  :: "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> MorleyClientM Bool
isOperator contract owner operator = do
  ownerAddr <- resolveAddress owner
  operatorAddr <- resolveAddress operator
  operatorsId <- sOperators <$> getStorage contract
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
  call sender contract (Call @"Configure_minter") ConfigureMinterParam
    { cmpMinter = minterAddr
    , cmpCurrentMintingAllowance = currentMintingAllowance
    , cmpNewMintingAllowance = newMintingAllowance
    }

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
  call sender contract (Call @"Mint") [MintParam toAddr amount]

-- | Decreases balance for sender by the sum of given amounts.
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
  sIsPaused <$> getStorage contract

-- | Get the address and optional alias of the current contract owner.
getContractOwner :: "contract" :! AddressOrAlias -> MorleyClientM AddressAndAlias
getContractOwner contract = do
  owner <- rOwner . sRoles <$> getStorage contract
  pairWithAlias owner

-- | Check if there's an ownership transfer pending acceptance and, if so,
-- get its pending owner's address and optional alias.
getPendingContractOwner :: "contract" :! AddressOrAlias -> MorleyClientM (Maybe AddressAndAlias)
getPendingContractOwner contract = do
  pendingOwnerMb <- rPendingOwner . sRoles <$> getStorage contract
  traverse pairWithAlias pendingOwnerMb

-- | Get the address and optional alias of the master minter.
getMasterMinter :: "contract" :! AddressOrAlias -> MorleyClientM AddressAndAlias
getMasterMinter contract = do
  masterMinter <- rMasterMinter . sRoles <$> getStorage contract
  pairWithAlias masterMinter

-- | Get the address and optional alias of the pauser.
getPauser :: "contract" :! AddressOrAlias -> MorleyClientM AddressAndAlias
getPauser contract = do
  pauser <- rPauser . sRoles <$> getStorage contract
  pairWithAlias pauser

-- | Check if a transferlist is set and, if so, get its address and optional alias.
getTransferlist :: "contract" :! AddressOrAlias -> MorleyClientM (Maybe AddressAndAlias)
getTransferlist contract = do
  transferlistMb <- sTransferlistContract <$> getStorage contract
  traverse pairWithAlias transferlistMb

-- | Get the minting allowance for the given minter.
getMintingAllowance :: "contract" :! AddressOrAlias -> AddressOrAlias -> MorleyClientM Natural
getMintingAllowance contract minter = do
  minterAddr <- resolveAddress minter
  mintingAllowances <- sMintingAllowances <$> getStorage contract
  let allowanceMaybe = M.lookup minterAddr mintingAllowances
  pure $ fromMaybe 0 allowanceMaybe

-- | Get the token metadata of the contract
getTokenMetadata :: "contract" :! AddressOrAlias -> MorleyClientM FA2.TokenMetadata
getTokenMetadata contract = do
  mdRegistry <- sTokenMetadataRegistry <$> getStorage contract
  bigMapId <- mrsTokenMetadata <$> getRegistryStorage mdRegistry
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
getRegistryStorage :: Address -> MorleyClientM MetadataRegistryStorageView
getRegistryStorage contractAddr = do
  OriginationScript _ storageExpr <- getContractScript contractAddr
  case fromVal @MetadataRegistryStorageView <$> fromExpression storageExpr of
    Right storage -> pure storage
    Left err -> throwM $ SCEExpressionParseError storageExpr err

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
