-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
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

import Data.Aeson qualified as J
import Data.Map qualified as M
import Fmt (Buildable(build), pretty, (+|), (|+))
import Lorentz
  (EntrypointRef(Call), HasEntrypointArg, ToT, toAddress, toMichelsonContract, useHasEntrypointArg)
import Lorentz.Contracts.Spec.TZIP16Interface qualified as TZ
import Morley.Michelson.Typed (BigMapId(..), Dict(..), IsoValue, fromVal, toVal)

import Morley.Client
  (MorleyClientM, TezosClientError(UnknownAddress), getAlias, getContractScript, lTransfer,
  originateContract, readBigMapValue, readBigMapValueMaybe, revealKeyUnlessRevealed)
import Morley.Client qualified as Client
import Morley.Client.RPC (OperationHash, OriginationScript(OriginationScript))
import Morley.Micheline (Expression, FromExpressionError, fromExpression)
import Morley.Michelson.Text
import Morley.Tezos.Address
  (Address, ConstrainedAddress(..), ContractAddress, KindedAddress(..), L1Address, TxRollupAddress)
import Morley.Tezos.Address.Alias (AddressOrAlias(..), Alias(..), ContractAlias, unAlias)
import Morley.Tezos.Core (Mutez, zeroMutez)
import Morley.Util.Default (def)
import Morley.Util.Interpolate (itu)
import Morley.Util.Named (arg, pattern (:!), (:!))

import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Lorentz.Contracts.Stablecoin
  (ConfigureMinterParam(..), MetadataUri(..), MintParam(..), Parameter, ParsedMetadataUri(..),
  Roles(..), Storage(..), StorageRPC(..), UpdateOperatorData(..), contractMetadataContract,
  metadataJSON, metadataMap, mkContractMetadataRegistryStorage, parseMetadataUri,
  stablecoinContract)
import Stablecoin.Client.Contract (InitialStorageOptions(..))
import Stablecoin.Client.L1AddressOrAlias
import Stablecoin.Client.Metadata (ViewParam(..), callOffChainView)
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

-- | An address and an optional alias, if one is found.
data AddressAndAlias = AddressAndAlias Address (Maybe Text)
  deriving stock (Show, Eq)

instance Buildable AddressAndAlias where
  build (AddressAndAlias addr mal) = "Address " +| addr |+
    maybe mempty (\al -> "(alias " +| al |+ ")") mal

-- | Deploy the stablecoin contract.
-- Returns the operation hash and the contract's new address.
-- If a new contract is originated to hold stablecoin's metadata, that contract's address is returned too.
--
-- Saves the contract with the given alias.
-- If the given alias already exists, nothing happens.
deploy :: "sender" :! ImplicitAddressOrAlias -> ContractAlias -> InitialStorageOptions -> MorleyClientM (OperationHash, ContractAddress, Maybe ContractAddress)
deploy (arg #sender -> sender) alias InitialStorageOptions {..} = do
  sender' <- convertImplicitAddressOrAlias sender
  masterMinter <- resolveAddress isoMasterMinter
  contractOwner <- resolveAddress isoContractOwner
  pauser <- resolveAddress isoPauser
  transferlist <- traverse resolveAddress isoTransferlist

  -- Make the metadata bigmap
  contractMetadataUri  <-
    case isoContractMetadataStorage of
      -- User wants to store metadata embedded in the contact
      OpCurrentContract mbDesc -> do
        let metadata = metadataJSON Nothing mbDesc
        -- We drop some errors from metadata so that the contract will originate within operation
        -- limits.
        pure $ CurrentContract (TZ.errors [] <> metadata) True
      -- User have stored metadata somewhere and just wants to put the raw uri in metadata bigmap
      OpRaw url -> pure $ Raw url
      -- User wants a new contract with metadata to be deployed.
      OpRemoteContract mbDesc -> do
        let fa2TokenMetadata = FA2.mkTokenMetadata isoTokenSymbol isoTokenName (show isoTokenDecimals)
        let metadata = metadataJSON (Just fa2TokenMetadata) mbDesc
        let mdrStorage = mkContractMetadataRegistryStorage
              (metadataMap $ CurrentContract metadata False)
        contractMetadataRegistryAddress <- snd <$> originateContract
          False
          (ContractAlias "stablecoin-tzip16-metadata")
          sender'
          zeroMutez
          (toMichelsonContract contractMetadataContract)
          (toVal mdrStorage)
          Nothing
        pure $ RemoteContract contractMetadataRegistryAddress

  let initialStorage =
        Storage
          { sDefaultExpiry = isoDefaultExpiry
          , sLedger = def
          , sMintingAllowances = mempty
          , sOperators = def
          , sPaused = False
          , sPermitCounter = 0
          , sPermits = def
          , sRoles = Roles
              { rMasterMinter = toAddress masterMinter
              , rOwner = toAddress contractOwner
              , rPauser = toAddress pauser
              , rPendingOwner = Nothing
              }
          , sTransferlistContract = toAddress <$> transferlist
          , sMetadata = metadataMap contractMetadataUri
          , sTotalSupply = 0
          }

  let cmdAddress = case contractMetadataUri of
        RemoteContract addr -> Just addr
        _ -> Nothing

  (cName, cAddr) <- originateContract
    False
    alias
    sender'
    zeroMutez
    (toMichelsonContract stablecoinContract)
    (toVal initialStorage)
    Nothing
  pure (cName, cAddr, cmdAddress)

transfer
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> L1AddressOrAlias -> Natural -> MorleyClientM ()
transfer sender contract from to amount = do
  fromAddr <- resolveAddress from
  toAddr <- resolveAddress to
  call sender contract (Call @"Transfer")
    [FA2.TransferItem (toAddress fromAddr) [FA2.TransferDestination (toAddress toAddr) FA2.theTokenId amount]]

getBalanceOf
  :: "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> MorleyClientM Natural
getBalanceOf contract owner = do
  ownerAddr <- resolveAddress owner
  ledgerId <- sLedgerRPC <$> getStorage contract
  balanceMaybe <- readBigMapValueMaybe ledgerId (toAddress ownerAddr)
  pure $ fromMaybe 0 balanceMaybe

updateOperators
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> NonEmpty UpdateOperatorData -> MorleyClientM ()
updateOperators (arg #sender -> sender) contract ops = do
  senderAddr <- resolveAddress sender
  -- Note: As per the specification:
  -- "In each update_operator item `owner` MUST be equal to `SENDER`"
  let ownerAddr = toAddress senderAddr
  param <- traverse (mkUpdateOperator ownerAddr) (toList ops)
  call (#sender :! KAOAAddress senderAddr) contract (Call @"Update_operators") param
  where
    mkUpdateOperator :: Address -> UpdateOperatorData -> MorleyClientM FA2.UpdateOperator
    mkUpdateOperator ownerAddr = \case
      AddOperator op -> FA2.AddOperator <$> mkOperatorParam ownerAddr op
      RemoveOperator op -> FA2.RemoveOperator <$> mkOperatorParam ownerAddr op

    mkOperatorParam :: Address -> L1AddressOrAlias -> MorleyClientM FA2.OperatorParam
    mkOperatorParam ownerAddr operator =
      resolveAddress operator <&> \operatorAddr ->
        FA2.OperatorParam (toAddress ownerAddr) (toAddress operatorAddr) FA2.theTokenId

isOperator
  :: "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> L1AddressOrAlias -> MorleyClientM Bool
isOperator contract owner operator = do
  ownerAddr <- resolveAddress owner
  operatorAddr <- resolveAddress operator
  operatorsId <- sOperatorsRPC <$> getStorage contract
  isJust @() <$> readBigMapValueMaybe operatorsId (toAddress ownerAddr, toAddress operatorAddr)

-- | Pauses transferring, burning and minting operations so that they
-- cannot be performed. All other operations remain unaffected.
pause :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias -> MorleyClientM ()
pause sender contract = call sender contract (Call @"Pause") ()

-- | Unpauses the contract so that transferring, burning and minting
-- operations can be performed by users with corresponding roles.
unpause :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias -> MorleyClientM ()
unpause sender contract = call sender contract (Call @"Unpause") ()

-- | Adds a minter to the minter list to allow them to mint tokens
-- (if minter is not in the list already).
configureMinter
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> Maybe Natural -> Natural -> MorleyClientM ()
configureMinter sender contract minter currentMintingAllowance newMintingAllowance = do
  minterAddr <- resolveAddress minter
  call sender contract (Call @"Configure_minter") ConfigureMinterParam
    { cmpMinter = toAddress minterAddr
    , cmpCurrentMintingAllowance = currentMintingAllowance
    , cmpNewMintingAllowance = newMintingAllowance
    }

-- | Removes a minter from the minter list and sets its minting allowance to 0.
-- Once minter is removed it will no longer be able to mint or burn tokens.
removeMinter
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> MorleyClientM ()
removeMinter sender contract minter = do
  minterAddr <- resolveAddress minter
  call sender contract (Call @"Remove_minter") (toAddress minterAddr)

-- | Produces the given amounts of tokens to the wallets associated
-- with the given addresses.
mint
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> Natural -> MorleyClientM ()
mint sender contract to amount = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Mint") [MintParam (toAddress toAddr) amount]

-- | Decreases balance for sender by the sum of given amounts.
burn
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> NonEmpty Natural -> MorleyClientM ()
burn sender contract amounts =
  call sender contract (Call @"Burn") (toList amounts)

-- | Initiate transfer of contract ownership to a new address.
transferOwnership
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> MorleyClientM ()
transferOwnership sender contract to = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Transfer_ownership") (toAddress toAddr)

-- | Accept contract ownership privileges.
acceptOwnership
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> MorleyClientM ()
acceptOwnership sender contract =
  call sender contract (Call @"Accept_ownership") ()

-- | Set master minter to a new address.
changeMasterMinter
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> MorleyClientM ()
changeMasterMinter sender contract to = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Change_master_minter") (toAddress toAddr)

-- | Set pauser to a new address.
changePauser
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> L1AddressOrAlias -> MorleyClientM ()
changePauser sender contract to = do
  toAddr <- resolveAddress to
  call sender contract (Call @"Change_pauser") (toAddress toAddr)

-- | Set the stored (optional) transferlist address to the new one.
setTransferlist
  :: "sender" :! ImplicitAddressOrAlias -> "contract" :! ContractAddressOrAlias
  -> Maybe ContractAddressOrAlias -> MorleyClientM ()
setTransferlist sender contract transferlist = do
  transferlistAddr <- traverse resolveAddress transferlist
  call sender contract (Call @"Set_transferlist") (toAddress <$> transferlistAddr)

-- | Retrieve the contract's balance.
getBalance :: "contract" :! ContractAddressOrAlias -> MorleyClientM Mutez
getBalance (arg #contract -> contract) = do
  contractAddr <- resolveAddress contract
  Client.getBalance contractAddr

-- | Check whether the contract has been paused.
getPaused :: "contract" :! ContractAddressOrAlias -> MorleyClientM Bool
getPaused contract =
  sPausedRPC <$> getStorage contract

-- | Get the address and optional alias of the current contract owner.
getContractOwner :: "contract" :! ContractAddressOrAlias -> MorleyClientM AddressAndAlias
getContractOwner contract = do
  owner <- rOwner . sRolesRPC <$> getStorage contract
  unsafeAddressToL1Address "owner" owner >>= pairWithAlias

-- | Check if there's an ownership transfer pending acceptance and, if so,
-- get its pending owner's address and optional alias.
getPendingContractOwner :: "contract" :! ContractAddressOrAlias -> MorleyClientM (Maybe AddressAndAlias)
getPendingContractOwner contract = do
  pendingOwnerMb <- rPendingOwner . sRolesRPC <$> getStorage contract
  traverse (unsafeAddressToL1Address "pending_owner" >=> pairWithAlias) pendingOwnerMb

-- | Get the address and optional alias of the master minter.
getMasterMinter :: "contract" :! ContractAddressOrAlias -> MorleyClientM AddressAndAlias
getMasterMinter contract = do
  masterMinter <- rMasterMinter . sRolesRPC <$> getStorage contract
  unsafeAddressToL1Address "master_minter" masterMinter >>= pairWithAlias

-- | Get the address and optional alias of the pauser.
getPauser :: "contract" :! ContractAddressOrAlias -> MorleyClientM AddressAndAlias
getPauser contract = do
  pauser <- rPauser . sRolesRPC <$> getStorage contract
  unsafeAddressToL1Address "pauser" pauser >>= pairWithAlias

-- | Check if a transferlist is set and, if so, get its address and optional alias.
getTransferlist :: "contract" :! ContractAddressOrAlias -> MorleyClientM (Maybe AddressAndAlias)
getTransferlist contract = do
  transferlistMb <- sTransferlistContractRPC <$> getStorage contract
  traverse (unsafeAddressToL1Address "transferlist" >=> pairWithAlias) transferlistMb

-- | Get the minting allowance for the given minter.
getMintingAllowance :: "contract" :! ContractAddressOrAlias -> L1AddressOrAlias -> MorleyClientM Natural
getMintingAllowance contract minter = do
  minterAddr <- resolveAddress minter
  mintingAllowances <- sMintingAllowancesRPC <$> getStorage contract
  let allowanceMaybe = M.lookup (toAddress minterAddr) mintingAllowances
  pure $ fromMaybe 0 allowanceMaybe

getTokenMetadata :: "contract" :! ContractAddressOrAlias -> MorleyClientM FA2.TokenMetadata
getTokenMetadata (arg #contract -> contract) = do
  contractAddr <- resolveAddress contract

  (metadata, storageRPC) <- getContractMetadata $ #contract :! contract
  offChainViews <- throwMdErr (\err -> "Views was not found in metadata:" <> pretty err) $ TZ.getViews metadata

  callOffChainView @(Natural, FA2.TokenMetadata) @_ @Storage
    offChainViews storageRPC contractAddr
    "token_metadata" (ViewParam @Natural 0)
    <&> snd

-- | Get the metadata of the contract
getContractMetadata
  :: "contract" :! ContractAddressOrAlias
  -> MorleyClientM (TZ.Metadata (ToT Storage), StorageRPC)
getContractMetadata contract = do
  storageView <- getStorage contract
  let bigMapId = sMetadataRPC storageView
  metadataUri <- decodeUtf8 <$> readBigMapValue bigMapId mempty
  throwMdErr ("Unparsable URI" <>) (parseMetadataUri metadataUri) >>= \case
    InCurrentContractUnderKey key -> do
      mtKey <- throwMdErr (const $ "Unexpected key:" <> key) $ mkMText key
      rawMd <- readBigMapValue bigMapId mtKey
      throwMdErr (\err -> "Error decoding metadata:" <> pretty err)
        $ (, storageView) <$> J.eitherDecodeStrict rawMd
    InRemoteContractUnderKey addr key -> do
      mtKey <- throwMdErr (const $ "Unexpected key:" <> key) $ mkMText key
      bigMapId_ <- snd <$> getMetadataRegistryStorage (#contract :! addr)
      rawMd <- readBigMapValue bigMapId_ mtKey
      throwMdErr (\err -> "Error decoding metadata:" <> pretty err)
        $ (, storageView) <$> J.eitherDecodeStrict rawMd
    RawUri uri_ -> throwM $ SCEMetadataError ("Unsupported metadata URI:" <> uri_)

throwMdErr :: (a -> Text) -> Either a b -> MorleyClientM b
throwMdErr toMsg f = either (throwM . SCEMetadataError . toMsg) pure f

unsafeAddressToL1Address :: Text -> Address -> MorleyClientM L1Address
unsafeAddressToL1Address fieldName = \case
  MkAddress (ca@ContractAddress{}) -> pure $ MkConstrainedAddress ca
  MkAddress (ia@ImplicitAddress{}) -> pure $ MkConstrainedAddress ia
  MkAddress (tra@TxRollupAddress{}) -> throwM $ SCEUnexpectedTxRollupAddress fieldName tra

-- | Check if there's an alias associated with this address and, if so, return both.
pairWithAlias :: L1Address -> MorleyClientM AddressAndAlias
pairWithAlias addr@(MkConstrainedAddress aa) = do
  aliasMb <- (Just <$> getAlias (AddressResolved aa)) `catch` \case
    UnknownAddress _ -> pure Nothing
    err -> throwM err
  pure $ AddressAndAlias (toAddress addr) (unAlias <$> aliasMb)

-- | Call an entrypoint of the stablecoin contract with the given argument.
call
  :: forall epRef epArg.
     (HasEntrypointArg Parameter epRef epArg, IsoValue epArg, Typeable epArg)
  => "sender" :! ImplicitAddressOrAlias
  -> "contract" :! ContractAddressOrAlias
  -> epRef
  -> epArg
  -> MorleyClientM ()
call (arg #sender -> sender) (arg #contract -> contract) epRef epArg =
  case useHasEntrypointArg @Parameter @epRef @epArg epRef of
    (Dict, epName) -> do
      senderAddr <- resolveAddress sender
      env <- ask
      liftIO $ Client.runMorleyClientM env $ revealKeyUnlessRevealed senderAddr Nothing
      contractAddr <- resolveAddress contract
      void $ lTransfer
        senderAddr
        contractAddr
        zeroMutez
        epName
        epArg
        Nothing

-- | Get the contract's storage.
getStorage :: "contract" :! ContractAddressOrAlias -> MorleyClientM StorageRPC
getStorage (arg #contract -> contract) = do
  contractAddr <- resolveAddress contract
  OriginationScript _ storageExpr <- getContractScript contractAddr
  case fromVal @StorageRPC <$> fromExpression storageExpr of
    Right storage -> pure storage
    Left err -> throwM $ SCEExpressionParseError storageExpr err

-- | Get the contract's storage.
getMetadataRegistryStorage :: "contract" :! ContractAddress -> MorleyClientM ((), (BigMapId MText ByteString))
getMetadataRegistryStorage (arg #contract -> contract) = do
  OriginationScript _ storageExpr <- getContractScript contract
  case fromVal @((), BigMapId MText ByteString) <$> fromExpression storageExpr of
    Right storage -> pure storage
    Left err -> throwM $ SCEExpressionParseError storageExpr err

------------------------------------------------------------------
--- Exceptions
------------------------------------------------------------------

data StablecoinClientError
  = SCEExpressionParseError Expression FromExpressionError
  | SCEMetadataError Text
  | SCEUnexpectedTxRollupAddress
      -- ^ A @txr1@ address was found where a contract or an implicit account address was expected.
      Text -- ^ The name of the storage field where the address was found.
      TxRollupAddress

deriving stock instance Show StablecoinClientError

instance Buildable StablecoinClientError where
  build = \case
    SCEExpressionParseError expr err ->
      [itu|
        Failed to parse expression:
          #{expr}
        Parse error:
          #{err}
        |]
    SCEMetadataError err ->
      [itu|
        There was an error during the processing of metadata:
          #{err}
        |]
    SCEUnexpectedTxRollupAddress storageFieldName addr ->
      [itu|
        Expected storage field '#{storageFieldName}' to be an implicit account or contract address,
        but it was a transaction rollup address:
          #{addr}
        |]

instance Exception StablecoinClientError where
  displayException = pretty
