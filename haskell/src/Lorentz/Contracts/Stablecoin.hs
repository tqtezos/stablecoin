-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Stablecoin
  ( ConfigureMinterParam
  , ChangeMasterMinterParam
  , TokenMetadata
  , TokenMetadataRegistryAddress
  , RemoveMinterParam
  , MetadataRegistryStorage
  , MetadataRegistryStorageView
  , MintParams
  , MintParam
  , BurnParams
  , ParameterC
  , Parameter (..)
  , Storage
  , TransferOwnershipParam
  , OwHook(..)
  , OwHookOptReq(..)
  , minterLimit
  , mkTokenMetadata
  , stablecoinTokenMetadata

  -- We use these patterns only for validation
  , pattern StorageLedger
  , pattern StorageRoles
  , pattern StorageMinters
  , pattern StorageOperators
  , pattern StoragePaused
  , pattern StorageTransferlistContract
  , pattern StorageMetadataRegistery
  , pattern MasterMinterRole
  , pattern OwnerRole
  , pattern PauserRole
  , pattern PendingOwnerRole
  , pattern ConfigureMinterParams
  , pattern RegistryMetadata

  , stablecoinPath
  , metadataRegistryContractPath
  ) where

import Fmt
import qualified Text.Show

import Lorentz
import qualified Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Client (BigMapId(..))
import Util.Named

-- | The path to the compiled stablecoin contract.
stablecoinPath :: FilePath
stablecoinPath = "./test/resources/stablecoin.tz"

-- | The path to the compiled metadata registry.
metadataRegistryContractPath :: FilePath
metadataRegistryContractPath = "./test/resources/metadata.tz"

------------------------------------------------------------------
-- Parameter

type ConfigureMinterParam =
  ( "minter" :! Address
  , ( "current_minting_allowance" :! Maybe Natural
    , "new_minting_allowance" :! Natural
    )
  )

pattern ConfigureMinterParams :: Address -> Maybe Natural -> Natural -> ConfigureMinterParam
pattern ConfigureMinterParams addr cma nma <-
  ( arg #minter -> addr
  , (arg #current_minting_allowance -> cma, arg #new_minting_allowance -> nma))
{-# COMPLETE ConfigureMinterParams #-}

type RemoveMinterParam = Address

type MintParam =
  ( "to_" :! Address
  , "amount" :! Natural
  )

type MintParams = List MintParam

type BurnParams = List Natural

type TransferOwnershipParam = Address
type ChangeMasterMinterParam = Address
type ChangePauserParam = Address
type SetTransferlistParam = Maybe Address

-- | Parameter of Stablecoin contract
data Parameter
  = Call_FA2 FA2.Parameter
  | Pause
  | Unpause
  | Configure_minter ConfigureMinterParam
  | Remove_minter RemoveMinterParam
  | Mint MintParams
  | Burn BurnParams
  | Transfer_ownership TransferOwnershipParam
  | Accept_ownership
  | Change_master_minter ChangeMasterMinterParam
  | Change_pauser ChangePauserParam
  | Set_transferlist SetTransferlistParam
  deriving stock Generic
  deriving anyclass (IsoValue)

instance Buildable Parameter where
  build = genericF

instance Show Parameter where
  show = pretty

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

type ManagementMichelsonEntrypoints =
    [ "Pause" :> ()
    , "Unpause" :> ()
    , "Configure_minter" :> ConfigureMinterParam
    , "Remove_minter" :> RemoveMinterParam
    , "Mint" :> MintParams
    , "Burn" :> BurnParams
    , "Transfer_ownership" :> TransferOwnershipParam
    , "Accept_ownership" :> ()
    , "Change_master_minter" :> ChangeMasterMinterParam
    , "Change_pauser" :> ChangePauserParam
    , "Set_transferlist" :> SetTransferlistParam
    ]

type ParameterC param =
  ( FA2.FA2ParameterC param
  , ParameterContainsEntrypoints param ManagementMichelsonEntrypoints
  )

------------------------------------------------------------------
--- Storage
------------------------------------------------------------------

type OperatorsInner = BigMap (Address, Address) ()

type LedgerInner = Map Address Natural
type Ledger = "ledger" :! (BigMap Address Natural)

type Operators = "operators" :! OperatorsInner
type IsPaused = "paused" :! Bool

type MasterMinter = Address
type Owner = Address
type PendingOwner = Maybe Address
type Pauser = Address

type MintingAllowancesInner = Map Address Natural
type MintingAllowances = "minting_allowances" :! MintingAllowancesInner

type RolesInner = (("master_minter" :! MasterMinter, "owner" :! Owner)
             , ("pauser" :! Pauser, "pending_owner_address" :! PendingOwner))

type Roles = "roles" :! RolesInner

type TransferlistContract = "transferlist_contract" :! (Maybe Address)

type TokenMetadataRegistryAddress = "token_metadata_registry" :! Address

type Storage =
  (((Ledger, MintingAllowances), (Operators, IsPaused))
   , ((Roles, TokenMetadataRegistryAddress), TransferlistContract))

pattern StorageLedger :: LedgerInner -> Storage
pattern StorageLedger ledger <- (((arg #ledger -> (BigMap ledger), _ ), _), _)
{-# COMPLETE StorageLedger #-}

pattern StorageMinters :: MintingAllowancesInner -> Storage
pattern StorageMinters minters <- (((_, arg #minting_allowances -> minters), _), _)
{-# COMPLETE StorageMinters #-}

pattern StorageOperators :: OperatorsInner -> Storage
pattern StorageOperators operators <- ((_, (arg #operators -> operators, _)), _)
{-# COMPLETE StorageOperators #-}

pattern StoragePaused :: Bool -> Storage
pattern StoragePaused paused <- ((_, (_, arg #paused -> paused)), _)
{-# COMPLETE StoragePaused #-}

pattern StorageMetadataRegistery :: Address -> Storage
pattern StorageMetadataRegistery registry <- (_, ((_, arg #token_metadata_registry -> registry), _))
{-# COMPLETE StorageMetadataRegistery #-}

pattern StorageRoles :: RolesInner -> Storage
pattern StorageRoles roles <- (_ , ((arg #roles -> roles, _), _))
{-# COMPLETE StorageRoles #-}

pattern StorageTransferlistContract :: Maybe Address -> Storage
pattern StorageTransferlistContract transferlistContract <- (_, (_, arg #transferlist_contract -> transferlistContract))
{-# COMPLETE StorageTransferlistContract #-}

pattern MasterMinterRole :: MasterMinter -> RolesInner
pattern MasterMinterRole masterMinter <- ((arg #master_minter -> masterMinter, _), _)
{-# COMPLETE MasterMinterRole #-}

pattern OwnerRole :: Owner -> RolesInner
pattern OwnerRole owner <- ((_, arg #owner -> owner), _)
{-# COMPLETE OwnerRole #-}

pattern PauserRole :: Pauser -> RolesInner
pattern PauserRole pauser <- (_, (arg #pauser -> pauser, _))
{-# COMPLETE PauserRole #-}

pattern PendingOwnerRole :: PendingOwner -> RolesInner
pattern PendingOwnerRole pendingOwner <- (_, (_, arg #pending_owner_address -> pendingOwner))
{-# COMPLETE PendingOwnerRole #-}

-- Permissions descriptor
data OwHookOptReq = OptOH | ReqOp
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, L.HasAnnotation)

data OwHook =  OwNoOp | OwOptReq OwHookOptReq
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, L.HasAnnotation)

-- We will hard code stablecoin token metadata here
stablecoinTokenMetadata :: TokenMetadata
stablecoinTokenMetadata = mkTokenMetadata stablecoinTokenMetadataFA2
  where
    stablecoinTokenMetadataFA2 :: FA2.TokenMetadata
    stablecoinTokenMetadataFA2 =
      (#token_id .! 0, #mdr .! ( #symbol .! [mt|USDC|]
                               , #mdr2 .! ( #name .! [mt|USDC|]
                                          , #mdr3 .! ( #decimals .! 8, #extras .! mempty))))

mkTokenMetadata :: FA2.TokenMetadata -> TokenMetadata
mkTokenMetadata
  ( L.arg #token_id -> token_id
  , L.arg #mdr -> (L.arg #symbol -> symbol
  , L.arg #mdr2 -> (L.arg #name -> name
  , L.arg #mdr3 -> (L.arg #decimals -> decimals
  , L.arg #extras -> extras)))) = (token_id, (symbol, (name, (decimals, extras))))

type TokenMetadata = (Natural, (L.MText, (L.MText, (Natural, Map L.MText L.MText))))

-- Currently the contract allows to add upto 12 minters.
minterLimit :: Int
minterLimit = 12

type MetadataRegistryStorage' bt = ("dummy_field" :! (), "token_metadata" :! bt)

type MetadataRegistryStorageView = MetadataRegistryStorage' (BigMapId FA2.TokenId TokenMetadata)
type MetadataRegistryStorage = MetadataRegistryStorage' (BigMap FA2.TokenId TokenMetadata)

pattern RegistryMetadata :: a -> MetadataRegistryStorage' a
pattern RegistryMetadata metadata <- (_, arg #token_metadata -> metadata)
  where
    RegistryMetadata metadata = (#dummy_field .! (), #token_metadata .! metadata)
{-# COMPLETE RegistryMetadata #-}
