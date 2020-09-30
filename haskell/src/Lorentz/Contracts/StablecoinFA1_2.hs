-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains the Haskell bindings for working with the
-- @stablecoin.fa1.2.tz@ contract.
module Lorentz.Contracts.StablecoinFA1_2
 ( Storage(..)
 , Parameter(..)
 , stablecoinFA1_2Contract
 ) where

import Data.FileEmbed (embedStringFile)
import Fmt (pretty)

import Lorentz
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Michelson.Runtime (parseExpandContract)
import qualified Michelson.Untyped as U

import qualified Lorentz.Contracts.Stablecoin as S
import Lorentz.Contracts.StablecoinPath (stablecoinFA1_2Path)

data Storage = Storage
  { sDefaultExpiry :: S.Expiry
  , sLedger :: BigMap Address Natural
  , sMintingAllowances :: Map Address Natural
  , sIsPaused :: Bool
  , sPermitCounter :: Natural
  , sPermits :: BigMap Address S.UserPermits
  , sRoles :: S.Roles
  , sSpenderAllowances :: BigMap (Address, Address) Natural
  , sTotalSupply :: Natural
  , sTransferlistContract :: Maybe Address
  }

deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage
$(customGeneric "Storage" $ withDepths
    [ cstr @0
      [ fld @4 -- sDefaultExpiry
      , fld @4 -- sLedger
      , fld @4 -- sMintingAllowances
      , fld @4 -- sIsPaused
      , fld @4 -- sPermitCounter
      , fld @4 -- sPermits
      , fld @4 -- sRoles
      , fld @4 -- sSpenderAllowances
      , fld @2 -- sTotalSupply
      , fld @2 -- sTransferlistContract
      ]
    ]
  )

data Parameter
  = Accept_ownership
  | Burn S.BurnParams
  | Change_master_minter S.ChangeMasterMinterParam
  | Change_pauser S.ChangePauserParam
  | Configure_minter S.ConfigureMinterParam
  | Get_counter S.GetCounterParam
  | Get_default_expiry S.GetDefaultExpiryParam
  | Mint S.MintParams
  | Pause
  | Permit S.PermitParam
  | Remove_minter S.RemoveMinterParam
  | Revoke S.RevokeParams
  | Set_expiry S.SetExpiryParam
  | Set_transferlist S.SetTransferlistParam
  | Transfer_ownership S.TransferOwnershipParam
  | Unpause
  | Call_FA1_2 AL.Parameter
  deriving stock (Generic)
  deriving anyclass (IsoValue)

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

-- | Parse the metadata registry contract.
stablecoinFA1_2Contract :: U.Contract
stablecoinFA1_2Contract =
  $(case parseExpandContract (Just stablecoinFA1_2Path) $(embedStringFile stablecoinFA1_2Path) of
    Left e -> fail (pretty e)
    Right _ ->
      [|
        -- Note: it's ok to use `error` here, because we just proved that the contract
        -- can be parsed+typechecked.
        either (error . pretty) id $
          parseExpandContract
            (Just stablecoinFA1_2Path)
            $(embedStringFile stablecoinFA1_2Path)
      |]
    )
