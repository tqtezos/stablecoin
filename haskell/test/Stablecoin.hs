-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_FA2
  , spec_Management
  , spec_Permit
  , test_SMT
  ) where

import Hedgehog (withTests)
import Test.Hspec (Spec)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Lorentz (TAddress(..))
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Contracts.Test.Common
import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Test.Management
import Lorentz.Contracts.Test.Permit (permitSpec)
import Michelson.Test.Integrational
import Michelson.Typed hiding (TAddress)
import SMT
import Tezos.Core

origination :: OriginationFn SC.Parameter
origination originationParams = do
  mrAddress <- originateMetadataRegistry
  TAddress @SC.Parameter <$> tOriginate
    stablecoinContract
    "Stablecoin contract"
    (toVal (mkInitialStorage originationParams mrAddress))
    (toMutez 0)

spec_FA2 :: Spec
spec_FA2 =
  fa2Spec origination

test_SMT :: [TestTree]
test_SMT =
  [ testProperty "have the same state as the model after running the inputs"
    (withTests 20 $ smtProperty)
  ]

spec_Management :: Spec
spec_Management =
  managementSpec origination

spec_Permit :: Spec
spec_Permit =
  permitSpec origination
