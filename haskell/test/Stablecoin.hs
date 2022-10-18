-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( test_FA2
  , test_Management
  , test_Permit
  , test_SMT
  ) where

import Hedgehog (withTests)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Test.Management
import Lorentz.Contracts.Test.Permit (permitSpec)
import SMT

test_FA2 :: [TestTree]
test_FA2 =
  fa2Spec

test_SMT :: [TestTree]
test_SMT =
  [ testProperty "have the same state as the model after running the inputs"
    (withTests 20 $ smtProperty)
  ]

test_Management :: [TestTree]
test_Management =
  managementSpec

test_Permit :: [TestTree]
test_Permit =
  permitSpec
