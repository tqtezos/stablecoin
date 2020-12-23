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

import qualified Data.Aeson as J
import qualified Data.Map as M
import Fmt (Buildable(build), pretty, (+|), (|+))
import Lorentz (EntrypointRef(Call), HasEntrypointArg, ToT, arg, useHasEntrypointArg)
import Lorentz.Contracts.Spec.TZIP16Interface (Metadata(..))
import Michelson.Typed (Dict(..), IsoValue, fromVal, toVal)

import Michelson.Text
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
  (ConfigureMinterParam(..), ParsedMetadataUri(..), MetadataUri(..),
  MintParam(..), Parameter, Roles(..), Storage'(..), type Storage, StorageView, UpdateOperatorData(..),
  contractMetadataContract, metadataJSON, metadataMap, parseMetadataUri, mkContractMetadataRegistryStorage,
  mkFA2TokenMetadata, stablecoinContract)
import Stablecoin.Client.Contract (InitialStorageData(..), mkInitialStorage)
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

-- | An address and an optional alias, if one is found.
data AddressAndAlias = AddressAndAlias Address (Maybe Alias)
  deriving stock (Show, Eq)

-- | Deploy the stablecoin contract.
-- Returns the operation hash and the contract's new address.
-- If a new contract is originated to hold stablecoin's metadata, that contract's address is returned too.
--
-- Saves the contract with the given alias.
-- If the given alias already exists, nothing happens.
deploy :: "sender" :! AddressOrAlias -> AliasHint -> InitialStorageData AddressOrAlias -> MorleyClientM (Text, Address, Maybe Address)
deploy (arg #sender -> sender) alias initialStorageData@InitialStorageData {..} = do
  masterMinter <- resolveAddress isdMasterMinter
  contractOwner <- resolveAddress isdContractOwner
  pauser <- resolveAddress isdPauser
  transferlist <- traverse resolveAddress isdTransferlist

  -- Make the contract metadata bigmap
  contractMetadataUri  <-
    case isdContractMetadataStorage of
      -- User wants to store metadata embedded in the contact
      OpCurrentContract ->
        -- We drop some errors from metadata so that the contract will originate within operation
        -- limits.
        pure $ CurrentContract ((metadataJSON Nothing) { mErrors = [] }) True
      -- User have stored metadata somewhere and just wants to put the raw uri in metadata bigmap
      OpRaw url -> pure $ Raw url
      -- User wants a new contract with metadata to be deployed.
      OpRemoteContract -> do
        let fa2TokenMetadata = mkFA2TokenMetadata isdTokenSymbol isdTokenName isdTokenDecimals
        let mdrStorage = mkContractMetadataRegistryStorage
              (metadataMap $ CurrentContract (metadataJSON $ Just fa2TokenMetadata) False)
        contractMetadataRegistryAddress <- snd <$> originateContract
          False
          "stablecoin-tzip16-metadata"
          sender
          (unsafeMkMutez 0)
          contractMetadataContract
          (toVal mdrStorage)
        pure $ RemoteContract contractMetadataRegistryAddress

  let initialStorageData' = initialStorageData
            { isdMasterMinter = masterMinter
            , isdContractOwner = contractOwner
            , isdPauser = pauser
            , isdTransferlist = transferlist
            , isdContractMetadataStorage = metadataMap contractMetadataUri
            }

  let initialStorage = mkInitialStorage initialStorageData'

  let cmdAddress = case contractMetadataUri of
        RemoteContract addr -> Just addr
        _ -> Nothing

  (cName, cAddr) <- originateContract
    False
    alias
    sender
    (unsafeMkMutez 0)
    stablecoinContract
    (toVal initialStorage)
  pure (cName, cAddr, cmdAddress)

transfer
  :: "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> Natural -> MorleyClientM ()
transfer sender contract from to amount = do
  fromAddr <- resolveAddress from
  toAddr <- resolveAddress to
  call sender contract (Call @"Transfer")
    [FA2.TransferItem fromAddr [FA2.TransferDestination toAddr FA2.theTokenId amount]]

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
      AddOperator op -> FA2.AddOperator <$> mkOperatorParam ownerAddr op
      RemoveOperator op -> FA2.RemoveOperator <$> mkOperatorParam ownerAddr op

    mkOperatorParam :: Address -> AddressOrAlias -> MorleyClientM FA2.OperatorParam
    mkOperatorParam ownerAddr operator =
      resolveAddress operator <&> \operatorAddr ->
        FA2.OperatorParam ownerAddr operatorAddr FA2.theTokenId

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

getTokenMetadata :: "contract" :! AddressOrAlias -> MorleyClientM FA2.TokenMetadata
getTokenMetadata contract = do
  metadata <- getContractMetadata @(Metadata (ToT Storage)) contract
  error "TODO"

-- | Get the token metadata of the contract
getContractMetadata :: (J.FromJSON metadata) => "contract" :! AddressOrAlias -> MorleyClientM metadata
getContractMetadata contract = do
  bigMapId <- sMetadata <$> getStorage contract
  metadataUri <- decodeUtf8 <$> readBigMapValue bigMapId [mt||]
  case parseMetadataUri metadataUri of
    Nothing -> throwM $ SCEMetadataError ("Unparsable URI:" <> metadataUri)
    Just uri -> case uri of
      InCurrentContractUnderKey key -> case mkMText key of
        Right mtKey -> do
          rawMd <- readBigMapValue bigMapId mtKey
          case J.eitherDecodeStrict rawMd of
            Right a -> pure a
            Left err -> throwM $ SCEMetadataError ("Error decoding metadata:" <> show err)
        Left err -> throwM $ SCEMetadataError ("Unexpected key:" <> key)
      InRemoteContractUnderKey addr key -> error "TODO"

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
  | SCEMetadataError Text

deriving stock instance Show StablecoinClientError

instance Buildable StablecoinClientError where
  build (SCEExpressionParseError expr err) =
    "Failed to parse expression:\n" +|
    expr |+ "\n" <>
    "Parse error: " +| err |+ ""

instance Exception StablecoinClientError where
  displayException = pretty
