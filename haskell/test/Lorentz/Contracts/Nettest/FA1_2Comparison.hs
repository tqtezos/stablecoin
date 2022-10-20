-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Nettest.FA1_2Comparison
  ( test_fa1_2ComparisonScenario
  ) where

import Data.Map qualified as Map
import Test.Tasty (TestTree)

import Morley.Michelson.Typed (mkBigMap)
import Test.Cleveland
import Test.Cleveland.Lorentz (toContractAddress)

import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SFA2
import Lorentz.Contracts.StablecoinFA1_2 as SFA1_2

import Lorentz.Contracts.Test.Common (nettestOriginateContractMetadataContract)

test_fa1_2ComparisonScenario :: TestTree
test_fa1_2ComparisonScenario = testScenario "fa1_2ComparisonScenario" fa1_2ComparisonScenario

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
fa1_2ComparisonScenario :: Scenario m
fa1_2ComparisonScenario = scenario do
  comment "-- FA1.2 vs FA2 comparison tests --"

  comment "Creating accounts"
  nettest <- refillable $ newAddress "minter"
  owner1 <- newAddress auto
  owner2 <- newAddress auto

  comment "Originating Stablecoin FA1.2 contract"
  let balances =
        [ (toAddress owner1, 10)
        , (toAddress owner2, 10)
        ]
  let roles = Roles
        { rMasterMinter = toAddress nettest
        , rOwner = toAddress nettest
        , rPauser = toAddress nettest
        , rPendingOwner = Nothing
        }

  let metadata = SFA2.metadataJSON Nothing Nothing

  cmrFA1_2Address <- nettestOriginateContractMetadataContract metadata
  let fa1_2Storage = SFA1_2.Storage
        { sDefaultExpiry = 1000
        , sLedger = mkBigMap balances
        , sMintingAllowances = mempty
        , sPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = roles
        , sTransferlistContract = Nothing
        , sTotalSupply = sum $ Map.fromList balances
        , sSpenderAllowances = mempty
        , sMetadata = metadataMap @(()) (RemoteContract $ toContractAddress cmrFA1_2Address)
        }

  fa1_2ContractAddr <-
    originate
      "Stablecoin FA1.2"
      fa1_2Storage
      stablecoinFA1_2Contract

  comment "Calling transfer"
  withSender owner1 $
    transfer fa1_2ContractAddr $ calling (ep @"Transfer")
      (#from :! toAddress owner1, #to :! toAddress owner2, #value :! 2)

  cmrAddress <- nettestOriginateContractMetadataContract metadata
  let fa2Storage = SFA2.Storage
        { sDefaultExpiry = 1000
        , sLedger = mkBigMap balances
        , sMintingAllowances = mempty
        , sPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = roles
        , sTransferlistContract = Nothing
        , sOperators = mempty
        , sMetadata = SFA2.metadataMap @(()) (RemoteContract $ toContractAddress cmrAddress)
        , sTotalSupply = 0
        }

  comment "Originating Stablecoin FA2 contract"
  fa2ContractAddr <- originate "Stablecoin FA2" fa2Storage stablecoinContract

  comment "Calling transfer"
  withSender owner1 $ transfer fa2ContractAddr $ calling (ep @"Transfer") [FA2.TransferItem
    { tiFrom = toAddress owner1
    , tiTxs =
        [ TransferDestination
          { tdTo = toAddress owner2
          , tdTokenId = FA2.theTokenId
          , tdAmount = 2
          }
        ]
    }]
