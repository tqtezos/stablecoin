-- SPDX-FileCopyrightText: 2020 TQ Tezos
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
  , Expiry
  , PermitHash(..)
  , UserPermits
  , PermitCounter
  , DefaultExpiry
  , PermitParam
  , RevokeParam
  , RevokeParams
  , SetExpiryParam
  , GetDefaultExpiryParam
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
  , pattern StoragePermits

  , stablecoinPath
  , metadataRegistryContractPath
  ) where

import Fmt
import qualified Text.Show

import Lorentz
import qualified Lorentz as L
import Morley.Client (BigMapId(..))
import Util.Named

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2

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

newtype PermitHash = PermitHash ByteString
  deriving stock (Generic, Show)
  deriving newtype (IsoValue, L.HasAnnotation, Eq, Ord)

instance Buildable PermitHash where
  build (PermitHash bs) = base64F bs

type PermitParam =
  ( PublicKey
  , (Signature, PermitHash)
  )

type RevokeParam = (PermitHash, Address)
type RevokeParams = List RevokeParam

type Expiry = Natural
type SetExpiryParam = (Expiry, Maybe (PermitHash, Address))

type GetDefaultExpiryParam = View () Expiry

type GetCounterParam = View () Natural

-- | Parameter of Stablecoin contract
data Parameter
  = Accept_ownership
  | Burn BurnParams
  | Call_FA2 FA2.Parameter
  | Change_master_minter ChangeMasterMinterParam
  | Change_pauser ChangePauserParam
  | Configure_minter ConfigureMinterParam
  | Get_counter GetCounterParam
  | Get_default_expiry GetDefaultExpiryParam
  | Mint MintParams
  | Pause
  | Permit PermitParam
  | Remove_minter RemoveMinterParam
  | Revoke RevokeParams
  | Set_expiry SetExpiryParam
  | Set_transferlist SetTransferlistParam
  | Transfer_ownership TransferOwnershipParam
  | Unpause


-- | In order to be able to construct valid permits,
-- the shape of the @or@ tree generated
-- the Michelson representation of 'Parameter' must be an exact match
-- of the Michelson parameter generated by Ligo in @stablecoin.tz@.
--
-- To see the the Michelson representation of 'Parameter':
--
-- >>> import Data.Singletons (demote)
-- >>> import Michelson.Typed (ToT)
-- >>> import Fmt (pretty)
-- >>>
-- >>> pretty $ demote @(ToT Parameter)
$(customGeneric "Parameter" $ withDepths
    [ cstr @5 []       -- %accept_ownership
    , cstr @5 [fld @0] -- %burn
    , cstr @5 [fld @0] -- %call_FA2
    , cstr @5 [fld @0] -- %change_master_minter
    , cstr @5 [fld @0] -- %change_pauser
    , cstr @5 [fld @0] -- %configure_minter
    , cstr @5 [fld @0] -- %get_counter
    , cstr @5 [fld @0] -- %get_default_expiry
    , cstr @5 [fld @0] -- %mint
    , cstr @5 []       -- %pause
    , cstr @5 [fld @0] -- %permit
    , cstr @5 [fld @0] -- %remove_minter
    , cstr @5 [fld @0] -- %revoke
    , cstr @5 [fld @0] -- %set_expiry
    , cstr @5 [fld @0] -- %set_transferlist
    , cstr @5 [fld @0] -- %transfer_ownership
    , cstr @1 []       -- %unpause
    ]
  )

deriving anyclass instance IsoValue Parameter

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

type PermitEntrypoints =
    [ "Permit" :> PermitParam
    , "Revoke" :> RevokeParams
    , "Set_expiry" :> SetExpiryParam
    , "Get_default_expiry" :> GetDefaultExpiryParam
    ]

type ParameterC param =
  ( FA2.FA2ParameterC param
  , ParameterContainsEntrypoints param ManagementMichelsonEntrypoints
  , ParameterContainsEntrypoints param PermitEntrypoints
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

type PermitCounter = "permit_counter" :! Natural
type DefaultExpiry = "default_expiry" :! Expiry
type Permits = "permits" :! BigMap Address UserPermits

type UserPermits = ("expiry" :! Maybe Expiry, "permits" :! Map PermitHash PermitInfo)
type PermitExpiry = "expiry" :! Maybe Expiry
type PermitCreatedAt = "created_at" :! Timestamp
type PermitInfo = (PermitCreatedAt, PermitExpiry)

type Storage =
  ((((DefaultExpiry, Ledger),
     (MintingAllowances, Operators)),
    ((IsPaused, PermitCounter),
     (Permits, Roles))),
   (TokenMetadataRegistryAddress, TransferlistContract))

pattern StorageLedger :: LedgerInner -> Storage
pattern StorageLedger ledger <- ((((_, arg #ledger -> BigMap ledger), _), _), _)
{-# COMPLETE StorageLedger #-}

pattern StorageMinters :: MintingAllowancesInner -> Storage
pattern StorageMinters minters <- (((_, (arg #minting_allowances -> minters, _)), _), _)
{-# COMPLETE StorageMinters #-}

pattern StorageOperators :: OperatorsInner -> Storage
pattern StorageOperators operators <- (((_, (_, arg #operators -> operators)), _), _)
{-# COMPLETE StorageOperators #-}

pattern StoragePaused :: Bool -> Storage
pattern StoragePaused paused <- ((_, ((arg #paused -> paused, _), _)), _)
{-# COMPLETE StoragePaused #-}

pattern StorageMetadataRegistery :: Address -> Storage
pattern StorageMetadataRegistery registry <- (_, (arg #token_metadata_registry -> registry, _))
{-# COMPLETE StorageMetadataRegistery #-}

pattern StorageRoles :: RolesInner -> Storage
pattern StorageRoles roles <- ((_, (_, (_, arg #roles -> roles))), _)
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

pattern StoragePermits :: Map Address UserPermits -> Storage
pattern StoragePermits permits <- ((_, (_, (arg #permits -> BigMap permits, _))), _)
{-# COMPLETE StoragePermits #-}

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
      (#token_id .! 0, #mdr .! ( #symbol .! [mt|TEST|]
                               , #mdr2 .! ( #name .! [mt|Test|]
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
