-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains the Haskell bindings for working with the
-- @stablecoin.fa1.2.tz@ contract.
module Lorentz.Contracts.StablecoinFA1_2.Types
  ( Storage(..)
  , Parameter(..)
  , stablecoinFA1_2Contract
  ) where


import Lorentz as L
import Lorentz.Contracts.Spec.ApprovableLedgerInterface qualified as AL
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import Morley.Michelson.Typed qualified as T
import Test.Cleveland.Michelson.Import (embedContract)

import Lorentz.Contracts.Stablecoin qualified as S
import Lorentz.Contracts.StablecoinPath (stablecoinFA1_2Path)

data Storage = Storage
  { sDefaultExpiry :: S.Expiry
  , sLedger :: BigMap Address Natural
  , sMetadata :: MetadataMap
  , sMintingAllowances :: Map Address Natural
  , sPaused :: Bool
  , sPermitCounter :: Natural
  , sPermits :: BigMap Address S.UserPermits
  , sRoles :: S.Roles
  , sSpenderAllowances :: BigMap (Address, Address) Natural
  , sTotalSupply :: Natural
  , sTransferlistContract :: Maybe Address
  }

customGeneric "Storage" ligoLayout
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage

-- | Similar to 'AL.Parameter' from @morley-ledgers@, but with a layout compatible with
-- the ligo implementation
data FA1_2Parameter
  = Transfer       AL.TransferParams
  | Approve        AL.ApproveParams
  | GetAllowance   AL.GetAllowanceArg
  | GetBalance     AL.GetBalanceArg
  | GetTotalSupply AL.GetTotalSupplyArg
customGeneric "FA1_2Parameter" ligoLayout
deriving anyclass instance IsoValue FA1_2Parameter

data Parameter
  = Accept_ownership
  | Burn S.BurnParams
  | Change_master_minter S.ChangeMasterMinterParam
  | Change_pauser S.ChangePauserParam
  | Configure_minter S.ConfigureMinterParam
  | Mint S.MintParams
  | Pause
  | Permit S.PermitParam
  | Remove_minter S.RemoveMinterParam
  | Set_expiry S.SetExpiryParam
  | Set_transferlist S.SetTransferlistParam
  | Transfer_ownership S.TransferOwnershipParam
  | Unpause
  | Call_FA1_2 FA1_2Parameter

customGeneric "Parameter" ligoLayout
deriving anyclass instance IsoValue Parameter

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

-- | Parse the FA1.2 variant of the stablecoin contract.
stablecoinFA1_2Contract :: T.Contract (ToT Parameter) (ToT Storage)
stablecoinFA1_2Contract =
  $$(embedContract @(ToT Parameter) @(ToT Storage) stablecoinFA1_2Path)
