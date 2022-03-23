-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains the Haskell bindings for working with the
-- @stablecoin.fa1.2.tz@ contract.
module Lorentz.Contracts.StablecoinFA1_2.Types
  ( Storage(..)
  , Parameter(..)
  , stablecoinFA1_2Contract

  , GetCounterParam
  , GetDefaultExpiryParam
  ) where

import Fmt (pretty)

import Lorentz as L
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import qualified Morley.Michelson.Untyped as U
import Test.Cleveland.Michelson.Import (embedTextFile, readUntypedContract)
import Morley.Michelson.Parser.Types (MichelsonSource(MSFile))

import qualified Lorentz.Contracts.Stablecoin as S
import Lorentz.Contracts.StablecoinPath (stablecoinFA1_2Path)

data Storage = Storage
  { sDefaultExpiry :: S.Expiry
  , sLedger :: BigMap Address Natural
  , sMetadata :: MetadataMap BigMap
  , sMintingAllowances :: Map Address Natural
  , sPaused :: Bool
  , sPermitCounter :: Natural
  , sPermits :: BigMap Address S.UserPermits
  , sRoles :: S.Roles
  , sSpenderAllowances :: BigMap (Address, Address) Natural
  , sTotalSupply :: Natural
  , sTransferlistContract :: Maybe Address
  }

deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
customGeneric "Storage" ligoLayout

data Parameter
  = Accept_ownership
  | Burn S.BurnParams
  | Change_master_minter S.ChangeMasterMinterParam
  | Change_pauser S.ChangePauserParam
  | Configure_minter S.ConfigureMinterParam
  | Get_counter GetCounterParam
  | Get_default_expiry GetDefaultExpiryParam
  | Mint S.MintParams
  | Pause
  | Permit S.PermitParam
  | Remove_minter S.RemoveMinterParam
  | Set_expiry S.SetExpiryParam
  | Set_transferlist S.SetTransferlistParam
  | Transfer_ownership S.TransferOwnershipParam
  | Unpause
  | Call_FA1_2 AL.Parameter
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

type GetCounterParam = View_ () Natural

type GetDefaultExpiryParam = View_ () S.Expiry

-- | Parse the metadata registry contract.
stablecoinFA1_2Contract :: U.Contract
stablecoinFA1_2Contract = $$(do
  text <- embedTextFile stablecoinFA1_2Path
  case readUntypedContract (MSFile stablecoinFA1_2Path) text of
    Left err -> fail $ pretty err
    -- Note: it's ok to use `error` here, because we just proved that the contract
    -- can be parsed+typechecked.
    Right _ -> [|| either (error . pretty) id $ readUntypedContract (MSFile stablecoinFA1_2Path) text ||]
    )
