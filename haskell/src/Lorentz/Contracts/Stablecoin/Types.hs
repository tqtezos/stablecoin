-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Stablecoin.Types
  ( ConfigureMinterParam(..)
  , MetadataRegistryStorage
  , ChangeMasterMinterParam
  , ChangePauserParam
  , SetTransferlistParam
  , RemoveMinterParam
  , mkContractMetadataRegistryStorage
  , defaultContractMetadataStorage
  , MintParams
  , MintParam(..)
  , BurnParams
  , FA2Parameter(..)
  , ParameterC
  , Parameter (..)
  , Storage(..)
  , StorageRPC(..)
  , UserPermits(..)
  , UpdateOperatorData(..)
  , PermitInfo(..)
  , Roles(..)

  , TransferOwnershipParam
  , Expiry
  , PermitHash(..)
  , mkPermitHash
  , PermitParam(..)
  , SetExpiryParam(..)
  , minterLimit

  ) where

import Fmt
import Text.Show qualified

import Lorentz as L
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import Morley.AsRPC (HasRPCRepr(..), deriveRPCWithStrategy)
import Morley.Michelson.Typed (Notes(..))
import Morley.Michelson.Untyped (noAnn)
import Morley.Tezos.Address.Alias (AddressOrAlias(..))
import Morley.Tezos.Crypto qualified as Hash

------------------------------------------------------------------
-- Parameter

-- | Data needed to add or remove an operator.
data UpdateOperatorData
  = AddOperator AddressOrAlias
  | RemoveOperator AddressOrAlias
  deriving stock Show

data ConfigureMinterParam = ConfigureMinterParam
 { cmpMinter :: Address
 , cmpCurrentMintingAllowance :: Maybe Natural
 , cmpNewMintingAllowance :: Natural
 }
 deriving stock (Generic, Show)
 deriving anyclass (IsoValue, HasAnnotation)

instance Buildable ConfigureMinterParam where
  build = genericF

type RemoveMinterParam = Address

data MintParam = MintParam
  { mpTo :: Address
  , mpAmount :: Natural
  }
  deriving stock (Show, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable MintParam where
  build = genericF

type MintParams = [MintParam]

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

data PermitParam = PermitParam
  { ppKey :: PublicKey
  , ppSignature :: Signature
  , ppPermitHash :: PermitHash
  }
 deriving stock (Generic, Show)
 deriving anyclass (IsoValue, HasAnnotation)

instance Buildable PermitParam where
  build = genericF

type Expiry = Natural

data SetExpiryParam = SetExpiryParam
  { sepOwner :: Address
  , sepExpiry :: Expiry
  , sepPermitHash :: Maybe PermitHash
  }
  deriving stock (Show, Generic)

deriving anyclass instance IsoValue SetExpiryParam
deriving anyclass instance HasAnnotation SetExpiryParam
instance Buildable SetExpiryParam where
  build = genericF

-- | Similar to 'FA2.Parameter' from @morley-ledgers@, but with a layout compatible with
-- the ligo implementation
data FA2Parameter
  = Balance_of FA2.BalanceRequestParams
  | Transfer FA2.TransferParams
  | Update_operators FA2.UpdateOperatorsParam
  deriving stock (Eq, Show)

instance Buildable FA2Parameter where
  build = \case
    Balance_of p -> "<Balance_of:" +| build p |+ ">"
    Transfer p -> "<Transfer:" +| build p |+ ">"
    Update_operators p -> "<Update_operators:" +| build p |+ ">"

customGeneric "FA2Parameter" ligoLayout
deriving anyclass instance IsoValue FA2Parameter

instance HasAnnotation FA2Parameter where
  getAnnotation f =
    NTOr noAnn noAnn noAnn
      (NTOr noAnn noAnn noAnn
        (getAnnotation @FA2.BalanceRequestParams f) (getAnnotation @FA2.TransferParams f))
      (getAnnotation @FA2.UpdateOperatorsParam f)

instance ParameterHasEntrypoints FA2Parameter where
  type ParameterEntrypointsDerivation FA2Parameter = EpdPlain

instance TypeHasDoc FA2Parameter where
  typeDocMdDescription = "Describes the FA2 operations."

-- | Parameter of Stablecoin contract
data Parameter
  = Accept_ownership
  | Burn BurnParams
  | Call_FA2 FA2Parameter
  | Change_master_minter ChangeMasterMinterParam
  | Change_pauser ChangePauserParam
  | Configure_minter ConfigureMinterParam
  | Mint MintParams
  | Pause
  | Permit PermitParam
  | Remove_minter RemoveMinterParam
  | Set_expiry SetExpiryParam
  | Set_transferlist SetTransferlistParam
  | Transfer_ownership TransferOwnershipParam
  | Unpause

customGeneric "Parameter" ligoLayout

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
    , "Set_expiry" :> SetExpiryParam
    ]

type ParameterC param =
  ( FA2.ParameterC param
  , ParameterContainsEntrypoints param ManagementMichelsonEntrypoints
  , ParameterContainsEntrypoints param PermitEntrypoints
  )

-- | Crates a permit that can be issued via the @Permit@ entrypoint, allowing other users
-- to call the contract with the given 'Parameter' value.
mkPermitHash :: Parameter -> PermitHash
mkPermitHash = PermitHash . Hash.blake2b . unPacked .  lPackValue

------------------------------------------------------------------
--- Storage
------------------------------------------------------------------

data UserPermits = UserPermits
  { upExpiry :: Maybe Expiry
  , upPermits :: Map PermitHash PermitInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

data PermitInfo = PermitInfo
  { piCreatedAt :: Timestamp
  , piExpiry :: Maybe Expiry
  }
  deriving stock (Show, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

data Roles = Roles
  { rMasterMinter :: Address
  , rOwner :: Address
  , rPauser :: Address
  , rPendingOwner :: Maybe Address
  }
  deriving stock (Show)

customGeneric "Roles" ligoLayout
deriving anyclass instance IsoValue Roles
deriving anyclass instance HasAnnotation Roles

instance HasRPCRepr Roles where
  type AsRPC Roles = Roles

data Storage = Storage
  { sDefaultExpiry :: Expiry
  , sLedger :: BigMap Address Natural
  , sMetadata :: MetadataMap
  , sMintingAllowances :: Map Address Natural
  , sOperators :: BigMap (Address, Address) ()
  , sPaused :: Bool
  , sPermitCounter :: Natural
  , sPermits :: BigMap Address UserPermits
  , sRoles :: Roles
  , sTotalSupply :: Natural
  , sTransferlistContract :: Maybe Address
  }

customGeneric "Storage" ligoLayout
deriveRPCWithStrategy "Storage" ligoLayout

deriving stock instance Show Storage
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage

-- | Represents a storage value retrieved using the Tezos RPC.
--
-- 'StorageView' is very similar to 'Storage',
-- except 'BigMap's have been replaced by 'BigMapId'.
-- This is because, when a contract's storage is queried, the Tezos RPC returns
-- big_maps' IDs instead of their contents.
type StorageView = StorageRPC
deriving stock instance Show StorageView

-- Currently the contract allows to add up to 12 minters.
minterLimit :: Int
minterLimit = 12

data MetadataRegistryStorage = MetadataRegistryStorage
  { cmrsDummyField :: () -- Dummy field to match the ligo contract's storage.
  , cmrsMetadata:: MetadataMap
  }
  deriving stock Generic

deriving anyclass instance IsoValue MetadataRegistryStorage

-- | Construct the storage for the Metadata contract.
mkContractMetadataRegistryStorage
  :: MetadataMap
  -> MetadataRegistryStorage
mkContractMetadataRegistryStorage m = MetadataRegistryStorage
  { cmrsDummyField = ()
  , cmrsMetadata = m
  }

-- The default storage of metadata contract with some hardcoded token metadata.
-- Useful for testing.
defaultContractMetadataStorage :: MetadataRegistryStorage
defaultContractMetadataStorage =
  mkContractMetadataRegistryStorage
    (mkBigMap [([mt|TEST|], "TEST"), ([mt|Test|], "Test")])
