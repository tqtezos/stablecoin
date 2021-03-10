-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | Tests for FA1.2 interface.
-- https://gitlab.com/tzip/tzip/-/blob/667f925471728bb8e81fbd30b93a171e401b6dc1/proposals/tzip-7/tzip-7.md

module Lorentz.Contracts.Test.FA1_2
  ( test_fa1_2
  ) where

import Test.Tasty (TestTree)

import Lorentz (Address, mkBigMap, toVal)
import Lorentz.Contracts.Test.ApprovableLedger (AlSettings(..), approvableLedgerGenericTest)
import Morley.Nettest
import Michelson.Typed (untypeValue)

import Lorentz.Contracts.Stablecoin (MetadataUri(..), Roles(..), metadataMap)
import Lorentz.Contracts.StablecoinFA1_2
  (Parameter, Storage(..), metadataJSON, stablecoinFA1_2Contract)

alOriginationFunction
  :: MonadNettest caps base m
  => Address -> AlSettings -> m (ContractHandler Parameter Storage)
alOriginationFunction adminAddr (AlInitAddresses addrBalances) = do
  let storage = Storage
        { sDefaultExpiry = 1000
        , sLedger = mkBigMap $ filter ((/= 0) . snd) addrBalances
        , sMintingAllowances = mempty
        , sSpenderAllowances = mempty
        , sPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = Roles
            { rMasterMinter = adminAddr
            , rOwner = adminAddr
            , rPauser = adminAddr
            , rPendingOwner = Nothing
            }
        , sTransferlistContract = Nothing
        , sTotalSupply = sum $ snd <$> addrBalances
        , sMetadata = metadataMap (CurrentContract metadataJSON True)
        }

  ContractHandler "stablecoinFA1_2Contract" <$>
    originateUntypedSimple "" (untypeValue $ toVal storage) stablecoinFA1_2Contract

test_fa1_2 :: TestTree
test_fa1_2 = approvableLedgerGenericTest @Parameter @Storage alOriginationFunction
