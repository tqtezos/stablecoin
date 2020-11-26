-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains the Haskell bindings for working with the
-- @stablecoin.fa1.2.tz@ contract.
module Lorentz.Contracts.StablecoinFA1_2
  ( Storage(..)
  , Parameter(..)
  , stablecoinFA1_2Contract

  -- * TZIP-16
  , metadataMap
  , metadataJSON
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (embedStringFile)
import qualified Data.Map as Map
import Fmt (pretty)

import Lorentz as L
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Michelson.Runtime (parseExpandContract)
import qualified Michelson.Untyped as U

import Lorentz.Contracts.Spec.TZIP16Interface
  (Metadata(..), MetadataMap, SomeMichelsonStorageView(..), ViewImplementation(..),
  mkMichelsonStorageView)
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZ (View(..))
import qualified Lorentz.Contracts.Stablecoin as S
import Lorentz.Contracts.StablecoinPath (stablecoinFA1_2Path)

data Storage = Storage
  { sDefaultExpiry :: S.Expiry
  , sLedger :: BigMap Address Natural
  , sMetadata :: MetadataMap BigMap
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
      , fld @4 -- sMetadata
      , fld @4 -- sMintingAllowances
      , fld @4 -- sIsPaused
      , fld @4 -- sPermitCounter
      , fld @4 -- sPermits
      , fld @4 -- sRoles
      , fld @3 -- sSpenderAllowances
      , fld @3 -- sTotalSupply
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

metadataMap :: MetadataMap BigMap
metadataMap = BigMap $ Map.fromList
  [ (mempty, encodeUtf8 @Text "tezos-storage:metadataJSON")
  , ([mt|metadataJSON|], BSL.toStrict (J.encode metadataJSON))
  ]

metadataJSON :: Metadata (ToT Storage)
metadataJSON =
  S.metadataJSON
    { mName = Just "stablecoin FA1.2"
    , mInterfaces = [ "TZIP-7", "TZIP-17" ]
    , mViews =
        [ getDefaultExpiryView
        , getCounterView
        ]

    -- NOTE: Because we're storing the metadata in the contract's storage,
    -- the storage may now exceed the storage size limit.
    -- See: https://buildkite.com/serokell/stablecoin/builds/1150#eb990412-157b-4a96-92d8-3fd39d954369/65-261
    --
    -- As a temporary measure, we're removing the "homepage" and "source"
    -- info to make the storage a little bit smaller, just enough to make it
    -- possible to originate the contract.
    --
    -- TODO: once we move the metadata to its own contract, we can
    -- add this info back to the metadata.
    , mHomepage = Nothing
    , mSource = Nothing
    -- , mSource = Just Source
    --     { sLocation = "https://github.com/tqtezos/stablecoin/tree/v" <> toText (showVersion version) <> "/ligo/stablecoin/fa1.2"
    --     , sTools = [ "ligo " ]
    --     }
    }

getDefaultExpiryView :: TZ.View (ToT Storage)
getDefaultExpiryView =
  TZ.View
    { vName = "GetDefaultExpiry"
    , vDescription = Just "Access the contract's default expiry in seconds"
    , vPure = True
    , vImplementations = one $
        VIMichelsonStorageView $ SomeMichelsonStorageView $
          mkMichelsonStorageView @Storage @() [] $
            L.car # L.toField #sDefaultExpiry
    }

getCounterView :: TZ.View (ToT Storage)
getCounterView =
  TZ.View
    { vName = "GetCounter"
    , vDescription = Just "Access the current permit counter"
    , vPure = True
    , vImplementations = one $
        VIMichelsonStorageView $ SomeMichelsonStorageView $
          mkMichelsonStorageView @Storage @() [] $
            L.car # L.toField #sPermitCounter
    }
