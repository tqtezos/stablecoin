-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_FA2
  , spec_Management
  , spec_Permit
  , test_SMT
  ) where

import Test.Hspec (Spec)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (runIO)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

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

import qualified Michelson.Typed as T
import Stablecoin.Client.Contract (parseStablecoinContract)

origination :: T.Contract (ToT SC.Parameter) (ToT Storage) -> OriginationFn SC.Parameter
origination contract (mkInitialStorage -> Just storageVal) = pure . TAddress @SC.Parameter <$>
    tOriginate contract "Stablecoin contract" (toVal storageVal) (unsafeMkMutez 0)
origination _ _ = pure Nothing

spec_FA2 :: Spec
spec_FA2 =
  runIO parseStablecoinContract >>= fa2Spec . origination

test_SMT :: IO [TestTree]
test_SMT = do
  contract <- parseStablecoinContract
  pure
    [ testProperty "have the same state as the model after running the inputs"
      (withMaxSuccess 20 $ smtProperty contract)
    ]

spec_Management :: Spec
spec_Management =
  runIO parseStablecoinContract >>= managementSpec . origination

spec_Permit :: Spec
spec_Permit =
  runIO parseStablecoinContract >>= permitSpec . origination
