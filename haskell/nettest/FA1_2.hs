-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module FA1_2
  ( fa1_2Scenario
  ) where

import qualified Data.Map as Map

import Lorentz (BigMap(..), EntrypointRef(Call), TAddress(..), mkView, toVal)
import Lorentz.Test.Consumer (contractConsumer)
import Michelson.Typed (untypeValue)
import qualified Michelson.Untyped as U
import Morley.Nettest
import Util.Named ((.!))

import Lorentz.Contracts.StablecoinFA1_2 (Parameter, Storage(..))

-- | This test runs each FA1.2 operation once.
--
-- It does not actually make any assertions (other than that the contract doesn't fail).
-- Rather, its main purpose is to automate these operations so we can easily
-- the gas/transaction costs.
fa1_2Scenario :: U.Contract -> NettestScenario m
fa1_2Scenario contract = uncapsNettest $ do
  comment "-- FA1.2 tests --"

  comment "Creating accounts"
  nettest <- resolveNettestAddress
  natConsumer <- originateSimple "natConsumer" [] (contractConsumer @Natural)
  owner1 <- newAddress "Steve"
  owner2 <- newAddress "Megan"

  let balances = Map.fromList
        [ (owner1, 10)
        , (owner2, 10)
        ]

  let storage = Storage
        { sDefaultExpiry = 1000
        , sLedger = BigMap balances
        , sMintingAllowances = mempty
        , sSpenderAllowances = mempty
        , sIsPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = #roles .!
            ( ( #master_minter .! nettest
              , #owner .! nettest
              )
            , ( #pauser .! nettest
              , #pending_owner_address .! Nothing
              )
            )
        , sTransferlistContract = Nothing
        , sTotalSupply = sum balances
        }

  comment "Originating FA1.2 contract"
  contractAddr <- TAddress @Parameter <$>
    originateUntypedSimple "Stablecoin FA1.2" (untypeValue $ toVal storage) contract

  comment "Calling approve"
  callFrom (AddressResolved owner1) contractAddr (Call @"Approve") (#spender .! owner2, #value .! 2)

  comment "Calling getAllowance"
  call contractAddr (Call @"GetAllowance") (mkView (#owner .! owner1, #spender .! owner2) natConsumer)

  comment "Calling getBalance"
  call contractAddr (Call @"GetBalance") (mkView (#owner .! owner1) natConsumer)

  comment "Calling getTotalSupply"
  call contractAddr (Call @"GetTotalSupply") (mkView () natConsumer)

  comment "Calling transfer"
  callFrom (AddressResolved owner1) contractAddr (Call @"Transfer") (#from .! owner1, #to .! owner2, #value .! 2)

  comment "Calling pre-approved transfer"
  callFrom (AddressResolved owner2) contractAddr (Call @"Transfer") (#from .! owner1, #to .! owner2, #value .! 2)
