-- SPDX-FileCopyrightText: 2020 TQ Tezos
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
  , Storage'(..)
  , Storage
  , StorageView
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

  -- * Embedded LIGO contracts
  , contractMetadataContract
  , stablecoinContract
  ) where

import Data.FileEmbed (embedStringFile)
import qualified Data.Map as Map
import Fmt
import qualified Text.Show

import Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import Michelson.Test.Import (readContract)
import Michelson.Typed (Notes(..))
import qualified Michelson.Typed as T
import Michelson.Untyped (noAnn)
import Morley.Client (AddressOrAlias(..), BigMapId(..))
import qualified Tezos.Crypto as Hash

import Lorentz.Contracts.StablecoinPath (metadataRegistryContractPath, stablecoinPath)

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

instance HasAnnotation FA2Parameter where
  getAnnotation f =
    NTOr noAnn noAnn noAnn
      (NTOr noAnn noAnn noAnn
        (getAnnotation @FA2.BalanceRequestParams f) (getAnnotation @FA2.TransferParams f))
      (getAnnotation @FA2.UpdateOperatorsParam f)

customGeneric "FA2Parameter" ligoLayout

deriving anyclass instance IsoValue FA2Parameter

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

deriving anyclass instance IsoValue Roles
deriving anyclass instance HasAnnotation Roles
customGeneric "Roles" ligoLayout

data Storage' big_map = Storage
  { sDefaultExpiry :: Expiry
  , sLedger :: big_map Address Natural
  , sMetadata :: MetadataMap big_map
  , sMintingAllowances :: Map Address Natural
  , sOperators :: big_map (Address, Address) ()
  , sPaused :: Bool
  , sPermitCounter :: Natural
  , sPermits :: big_map Address UserPermits
  , sRoles :: Roles
  , sTotalSupply :: Natural
  , sTransferlistContract :: Maybe Address
  }

customGeneric "Storage'" ligoLayout

type Storage = Storage' BigMap
deriving stock instance Show Storage
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage

-- | Represents a storage value retrieved using the Tezos RPC.
--
-- 'StorageView' is very similar to 'Storage',
-- except 'BigMap's have been replaced by 'BigMapId'.
-- This is because, when a contract's storage is queried, the Tezos RPC returns
-- big_maps' IDs instead of their contents.
type StorageView = Storage' BigMapId
deriving stock instance Show StorageView
deriving anyclass instance IsoValue StorageView

-- Currently the contract allows to add up to 12 minters.
minterLimit :: Int
minterLimit = 12

data MetadataRegistryStorage' big_map = MetadataRegistryStorage
  { cmrsDummyField :: () -- Dummy field to match the ligo contract's storage.
  , cmrsMetadata:: MetadataMap big_map
  }
  deriving stock Generic

type MetadataRegistryStorage = MetadataRegistryStorage' BigMap

deriving anyclass instance IsoValue MetadataRegistryStorage

-- | Construct the storage for the Metadata contract.
mkContractMetadataRegistryStorage
  :: MetadataMap BigMap
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
    (BigMap $ Map.fromList [([mt|TEST|], "TEST"), ([mt|Test|], "Test")])

-- This empty splice lets us workaround the GHC stage restriction, and refer to `Storage`
-- in the TH splices below.
$(pure [])

stablecoinContract :: T.Contract (ToT Parameter) (ToT Storage)
stablecoinContract =
  -- This TemplateHaskell splice is here to ensure the Michelson representation
  -- (i.e., the generated tree of `or` and `pair`) of these Haskell data
  -- types matches exactly the Michelson representation of the Ligo data types.
  --
  -- For example, if a Ligo data type generates a Michelson balanced tuple like
  -- ((a, b), (c, d)), but the corresponding Haskell data type generates a Michelson
  -- tuple like (a, (b, (c, d))), compilation should fail.
  --
  -- The reason they need to match is because of the way permits work.
  -- If we issue a permit for an entrypoint and the tree of `or`s is incorrect,
  -- then the permit will be unusable.
  -- See TZIP-017 for more info.
  --
  -- The splice attempts to parse the stablecoin.tz contract at compile-time.
  -- If, for example, the Michelson representation of Haskell's
  -- `Lorentz.Contracts.Stablecoin.Parameter` is different from Ligo's `parameter`,
  -- then this splice will raise a compilation error.
  --
  -- This can usually be fixed by writing a custom instance of Generic using `customGeneric`,
  -- and making sure the data type's layout matches the layout in stablecoin.tz.
  -- See examples in this module.
  $(case readContract @(ToT Parameter) @(ToT Storage) stablecoinPath $(embedStringFile stablecoinPath) of
      Left e ->
        -- Emit a compiler error if the contract cannot be read.
        fail (pretty e)
      Right _ ->
        -- Emit a haskell expression that reads the contract.
        [|
          -- Note: it's ok to use `error` here, because we just proved that the contract
          -- can be parsed+typechecked.
          either (error . pretty) snd $
            readContract
              stablecoinPath
              $(embedStringFile stablecoinPath)
        |]
  )

-- | Parse the contract-metadata contract.
contractMetadataContract :: T.Contract (ToT ()) (ToT MetadataRegistryStorage)
contractMetadataContract =
  $(case readContract @(ToT ()) @(ToT MetadataRegistryStorage) metadataRegistryContractPath $(embedStringFile metadataRegistryContractPath) of
      Left e -> fail (pretty e)
      Right _ ->
        [|
          -- Note: it's ok to use `error` here, because we just proved that the contract
          -- can be parsed+typechecked.
          either (error . pretty) snd $
            readContract
              metadataRegistryContractPath
              $(embedStringFile metadataRegistryContractPath)
        |]
  )
