-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Stablecoin
  ( ParameterC
  , Parameter (..)
  , Storage
  , mkPermissionDescriptor
  , stablecoinPermissionsDescriptor
  , stablecoinTokenMetadata

  -- We use these patterns only for validation
  , pattern StorageLedger
  , pattern StorageRoles
  , pattern StorageMinters
  , pattern StorageOperators
  , pattern StoragePaused
  , pattern StorageSafelistContract
  , pattern MasterMinterRole
  , pattern OwnerRole
  , pattern PauserRole
  , pattern PendingOwnerRole
  ) where

import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.FA2Interface
  (OperatorTransferPolicy(..), OwnerTransferMode(..))
import qualified Lorentz as L
import Util.Named


------------------------------------------------------------------
-- Parameter

type ConfigureMinterParam =
  ( "minter" :! Address
  , ( "current_minting_allowance" :! Maybe Natural
    , "new_minting_allowance" :! Natural
    )
  )

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
type SetSafelistParam = Maybe Address

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
  | Set_safelist SetSafelistParam
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdRecursive

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
    , "Set_safelist" :> SetSafelistParam
    ]

type ParameterC param =
  ( FA2.FA2ParameterC param
  , ParameterContainsEntryPoints param ManagementMichelsonEntrypoints
  )

------------------------------------------------------------------
--- Storage
------------------------------------------------------------------

type OperatorsInner = BigMap (Address, Address) ()

type LedgerInner = Map Address Natural
type Ledger = "ledger" :! (BigMap Address Natural)

type Operators = "operators" :! OperatorsInner
type TotalSupply = "total_supply" :! Natural
type IsPaused = "paused" :! Bool

type MintingAllowancesInner = Map Address Natural
type MintingAllowances = "minting_allowances" :! (BigMap Address Natural)

type RolesInner = (("master_minter" :! Address, "owner" :! Address)
             , ("pauser" :! Address, "pending_owner_address" :! Maybe Address))

type Roles = "roles" :! RolesInner

type SafelistContract = "safelist_contract" :! (Maybe Address)

type Storage =
  (((Ledger, MintingAllowances), (Operators, IsPaused))
   , ((Roles, SafelistContract), TotalSupply))

pattern StorageLedger :: LedgerInner -> Storage
pattern StorageLedger ledger <- (((arg #ledger -> (BigMap ledger), _ ), _), _)

pattern StorageMinters :: MintingAllowancesInner -> Storage
pattern StorageMinters minters <- (((_, arg #minting_allowances -> (BigMap minters)), _), _)

pattern StorageOperators :: OperatorsInner -> Storage
pattern StorageOperators operators <- ((_, (arg #operators -> operators, _)), _)

pattern StoragePaused :: Bool -> Storage
pattern StoragePaused paused <- ((_, (_, arg #paused -> paused)), _)

pattern StorageRoles :: RolesInner -> Storage
pattern StorageRoles roles <- (_ , ((arg #roles -> roles, _), _))

pattern StorageSafelistContract :: Maybe Address -> Storage
pattern StorageSafelistContract safelistContract <- (_ , ((_, arg #safelist_contract -> safelistContract), _))

------------------------------------------------------------------

type MasterMinter = Address
type Owner = Address
type PendingOwner = Maybe Address
type Pauser = Address

pattern MasterMinterRole :: MasterMinter -> RolesInner
pattern MasterMinterRole masterMinter <- ((arg #master_minter -> masterMinter, _), _)

pattern OwnerRole :: Owner -> RolesInner
pattern OwnerRole owner <- ((_, arg #owner -> owner), _)

pattern PauserRole :: Pauser -> RolesInner
pattern PauserRole pauser <- (_, (arg #pauser -> pauser, _))

pattern PendingOwnerRole :: PendingOwner -> RolesInner
pattern PendingOwnerRole pendingOwner <- (_, (_, arg #pending_owner_address -> pendingOwner))

-- Permissions descriptor
data OwHookOptReq = OptOH | ReqOp
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, L.HasTypeAnn)

data OwHook =  OwNoOp | OwOptReq OwHookOptReq
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, L.HasTypeAnn)

type PermissionsDescriptor =
  ((Maybe FA2.CustomPermissionPolicy, FA2.OperatorTransferPolicy), (OwHook, OwHook))

-- We will hard code permissions descriptor of Stablecoin contract here
stablecoinPermissionsDescriptor :: PermissionsDescriptor
stablecoinPermissionsDescriptor = mkPermissionDescriptor stablecoinPermissionsDescriptorFA2
  where
  stablecoinPermissionsDescriptorFA2 :: FA2.PermissionsDescriptor
  stablecoinPermissionsDescriptorFA2 =
    ( #operator .! OwnerOrOperatorTransfer (#owner_or_operator_transfer .! ())
    , #pdr2 .! ( #receiver .! OptionalOwnerHook (#optional_owner_hook .! ())
               , #pdr3 .! (#sender .! (OptionalOwnerHook (#optional_owner_hook .! ())), #custom .! Nothing )))

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
  , L.arg #extras -> extras)))) = (((decimals, extras), (name, symbol)), token_id)

mkPermissionDescriptor :: FA2.PermissionsDescriptor -> PermissionsDescriptor
mkPermissionDescriptor
    (L.arg #operator -> ot
     , L.arg #pdr2 -> (L.arg #receiver -> owtmr
            , L.arg #pdr3 -> (L.arg #sender -> otms, L.arg #custom -> cst))) = let
  custom = case cst of
    Just (L.arg #tag -> tag, L.arg #config_api -> config_api) -> Just (#tag .! tag, #config_api .! config_api)
    Nothing -> Nothing

  owHookRec = case owtmr of
    FA2.OptionalOwnerHook _ -> OwOptReq OptOH
    FA2.OwnerNoHook _ -> OwNoOp
    FA2.RequiredOwnerHook _ -> OwOptReq ReqOp

  owHookSend = case otms of
    FA2.OptionalOwnerHook _ -> OwOptReq OptOH
    FA2.OwnerNoHook _ -> OwNoOp
    FA2.RequiredOwnerHook _ -> OwOptReq ReqOp

  in ((custom, ot), (owHookRec, owHookSend))

-- Metadata
type TokenMetadataDecimalsAndExtra = (Natural, Map L.MText L.MText)
type TokenMetadataNameAndSymbol = (L.MText, L.MText)
type TokenMetadata = ((TokenMetadataDecimalsAndExtra, TokenMetadataNameAndSymbol), FA2.TokenId)
