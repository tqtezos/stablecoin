-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module FA1_2Comparison
  ( fa1_2ComparisonScenario
  ) where

import qualified Data.Map as Map

import Lorentz (BigMap(..), EntrypointRef(Call), TAddress(..), toVal)
import Michelson.Typed (convertContract, untypeValue)
import Morley.Nettest
import Util.Named ((.!))

import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SFA2
import Lorentz.Contracts.StablecoinFA1_2 as SFA1_2

import Lorentz.Contracts.Test.Common (nettestOriginateMetadataRegistry)

-- | This test originates both the FA1.2 and FA2 versions of stablecoin,
-- and performs a single @transfer@ operation.
--
-- It does not actually make any assertions (other than that the contract doesn't fail).
-- Rather, its main purpose is to automate these operations so we can easily
-- measure and compare the gas/transaction costs.
--
-- Whenever a new version of the contract is released, we should
-- run this test, collect the gas/transaction costs, and update
-- the table in the README.
fa1_2ComparisonScenario :: NettestScenario m
fa1_2ComparisonScenario = uncapsNettest $ do
  comment "-- FA1.2 vs FA2 comparison tests --"

  comment "Creating accounts"
  nettest <- resolveNettestAddress
  owner1 <- newAddress "Steve"
  owner2 <- newAddress "Megan"

  comment "Originating Stablecoin FA1.2 contract"
  let balances = Map.fromList
        [ (owner1, 10)
        , (owner2, 10)
        ]
  let roles = Roles
        { rMasterMinter = nettest
        , rOwner = nettest
        , rPauser = nettest
        , rPendingOwner = Nothing
        }

  let fa1_2Storage = SFA1_2.Storage
        { sDefaultExpiry = 1000
        , sLedger = BigMap balances
        , sMintingAllowances = mempty
        , sIsPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = roles
        , sTransferlistContract = Nothing
        , sTotalSupply = sum balances
        , sSpenderAllowances = mempty
        , sMetadata = SFA1_2.metadataMap
        }

  fa1_2ContractAddr <- TAddress @SFA1_2.Parameter <$>
    originateUntypedSimple "Stablecoin FA1.2" (untypeValue $ toVal fa1_2Storage) stablecoinFA1_2Contract

  comment "Calling transfer"
  callFrom (AddressResolved owner1) fa1_2ContractAddr (Call @"Transfer") (#from .! owner1, #to .! owner2, #value .! 2)

  mrAddress <- nettestOriginateMetadataRegistry
  let fa2Storage = SFA2.Storage
        { sDefaultExpiry = 1000
        , sLedger = BigMap balances
        , sMintingAllowances = mempty
        , sIsPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = roles
        , sTransferlistContract = Nothing
        , sOperators = mempty
        , sTokenMetadataRegistry = mrAddress
        , sMetadata = SFA2.metadataMap
        }

  comment "Originating Stablecoin FA2 contract"
  fa2ContractAddr <- TAddress @FA2.Parameter <$>
    originateUntypedSimple "Stablecoin FA2" (untypeValue $ toVal fa2Storage) (convertContract stablecoinContract)

  comment "Calling transfer"
  callFrom (AddressResolved owner1) fa2ContractAddr (Call @"Transfer") [FA2.TransferParam
    { tpFrom = owner1
    , tpTxs =
        [ TransferDestination { tdTo = owner2, tdTokenId = 0, tdAmount = 2 }

        ]
    }]
