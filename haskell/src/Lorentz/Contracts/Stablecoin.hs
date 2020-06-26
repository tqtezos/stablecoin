-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Stablecoin
  ( ConfigureMinterParam
  , ChangeMasterMinterParam
  , TokenMetadata
  , TokenMetadataBigMap
  , RemoveMinterParam
  , MintParams
  , MintParam
  , BurnParams
  , ParameterC
  , Parameter (..)
  , Storage
  , TransferOwnershipParam
  , mkPermissionDescriptor
  , mkTokenMetadata
  , stablecoinPermissionsDescriptor
  , stablecoinTokenMetadata

  -- We use these patterns only for validation
  , pattern StorageLedger
  , pattern StorageRoles
  , pattern StorageMinters
  , pattern StorageOperators
  , pattern StoragePaused
  , pattern StorageSafelistContract
  , pattern StorageMetadataBigMap
  , pattern StorageTotalSupply
  , pattern MasterMinterRole
  , pattern OwnerRole
  , pattern PauserRole
  , pattern PendingOwnerRole
  , pattern ConfigureMinterParams
  ) where

import Fmt
import qualified Text.Show

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
  deriving stock Generic
  deriving anyclass (IsoValue)

instance Buildable Parameter where
  build = genericF

instance Show Parameter where
  show = pretty

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

type MasterMinter = Address
type Owner = Address
type PendingOwner = Maybe Address
type Pauser = Address

type MintingAllowancesInner = Map Address Natural
type MintingAllowances = "minting_allowances" :! (BigMap Address Natural)

type RolesInner = (("master_minter" :! MasterMinter, "owner" :! Owner)
             , ("pauser" :! Pauser, "pending_owner_address" :! PendingOwner))

type Roles = "roles" :! RolesInner

type SafelistContract = "safelist_contract" :! (Maybe Address)

type TokenMetadataBigMap = "token_metadata" :! (BigMap FA2.TokenId TokenMetadata)

type Storage =
  (((Ledger, MintingAllowances), (Operators, IsPaused))
   , ((Roles, SafelistContract), (TokenMetadataBigMap, TotalSupply)))

pattern StorageLedger :: LedgerInner -> Storage
pattern StorageLedger ledger <- (((arg #ledger -> (BigMap ledger), _ ), _), _)
{-# COMPLETE StorageLedger #-}

pattern StorageMinters :: MintingAllowancesInner -> Storage
pattern StorageMinters minters <- (((_, arg #minting_allowances -> (BigMap minters)), _), _)
{-# COMPLETE StorageMinters #-}

pattern StorageOperators :: OperatorsInner -> Storage
pattern StorageOperators operators <- ((_, (arg #operators -> operators, _)), _)
{-# COMPLETE StorageOperators #-}

pattern StoragePaused :: Bool -> Storage
pattern StoragePaused paused <- ((_, (_, arg #paused -> paused)), _)
{-# COMPLETE StoragePaused #-}

pattern StorageMetadataBigMap :: BigMap FA2.TokenId TokenMetadata -> Storage
pattern StorageMetadataBigMap bigMap <- (_, (_, (arg #token_metadata -> bigMap, _)))
{-# COMPLETE StorageMetadataBigMap #-}

pattern StorageTotalSupply :: Natural -> Storage
pattern StorageTotalSupply totalSupply <- (_, (_, (_, arg #total_supply -> totalSupply)))
{-# COMPLETE StorageTotalSupply #-}

pattern StorageRoles :: RolesInner -> Storage
pattern StorageRoles roles <- (_ , ((arg #roles -> roles, _), _))
{-# COMPLETE StorageRoles #-}

pattern StorageSafelistContract :: Maybe Address -> Storage
pattern StorageSafelistContract safelistContract <- (_ , ((_, arg #safelist_contract -> safelistContract), _))
{-# COMPLETE StorageSafelistContract #-}

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
  , L.arg #extras -> extras)))) = (token_id, (symbol, (name, (decimals, extras))))

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



type TokenMetadata = (Natural, (L.MText, (L.MText, (Natural, Map L.MText L.MText))))
