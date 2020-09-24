-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | Tests for FA1.2 interface.
-- https://gitlab.com/tzip/tzip/-/blob/667f925471728bb8e81fbd30b93a171e401b6dc1/proposals/tzip-7/tzip-7.md

module Lorentz.Contracts.Test.FA1_2
  ( spec_fa1_2
  ) where

import qualified Data.Map as Map
import Fmt (pretty)
import Test.Hspec (Spec, describe)

import Lorentz (Address, BigMap(..), TAddress(..), toMutez, toVal)
import Lorentz.Contracts.Test.ApprovableLedger (AlSettings(..), approvableLedgerSpec)
import Michelson.Test.Integrational
import Michelson.Typed (untypeValue)
import Util.Named ((.!))

import Lorentz.Contracts.StablecoinFA1_2 (Parameter, Storage(..), parseStablecoinContract)

alOriginationFunction :: Address -> AlSettings -> IntegrationalScenarioM (TAddress Parameter)
alOriginationFunction adminAddr (AlInitAddresses addrBalances) = do
  stablecoionContract <-
    case parseStablecoinContract of
      Right c -> pure c
      Left err -> integrationalFail . CustomTestError $ pretty err

  let storage = Storage
        { sDefaultExpiry = 1000
        , sLedger = BigMap $ Map.filter (/= 0) $ Map.fromList $ addrBalances
        , sMintingAllowances = mempty
        , sSpenderAllowances = mempty
        , sIsPaused = False
        , sPermitCounter = 0
        , sPermits = mempty
        , sRoles = #roles .!
            ( ( #master_minter .! adminAddr
              , #owner .! adminAddr
              )
            , ( #pauser .! adminAddr
              , #pending_owner_address .! Nothing
              )
            )
        , sTransferlistContract = Nothing
        , sTotalSupply = sum $ snd <$> addrBalances
        }

  TAddress @Parameter <$> originate stablecoionContract "" (untypeValue $ toVal storage) (toMutez 0)

spec_fa1_2 :: Spec
spec_fa1_2 =
  describe "FA1.2 SMT" $
    approvableLedgerSpec alOriginationFunction
