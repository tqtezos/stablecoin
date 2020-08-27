-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_FA2
  , spec_Management
  , test_SMT
  ) where

import Test.Hspec (Spec)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (runIO)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

import Lorentz (TAddress(..))
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Contracts.Test.Common
import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Test.Management
import Michelson.Test.Integrational
import Michelson.Typed hiding (TAddress)
import qualified Michelson.Untyped as U
import SMT
import Tezos.Core

import Stablecoin.Client.Contract (parseStablecoinContract)

origination :: forall param . U.Contract -> OriginationFn param
origination contract (mkInitialStorage -> Just storageVal) = pure . TAddress @param <$>
    originate contract "Stablecoin contract" (untypeValue $ toVal storageVal) (unsafeMkMutez 0)
origination _ _ = pure Nothing

spec_FA2 :: Spec
spec_FA2 =
  runIO parseStablecoinContract >>= fa2Spec . origination @FA2.Parameter

test_SMT :: IO [TestTree]
test_SMT = do
  contract <- parseStablecoinContract
  pure
    [ testProperty "have the same state as the model after running the inputs"
      (withMaxSuccess 20 $ smtProperty contract)
    ]

spec_Management :: Spec
spec_Management =
  runIO parseStablecoinContract >>= managementSpec . origination @SC.Parameter
