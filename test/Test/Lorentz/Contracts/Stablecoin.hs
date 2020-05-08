-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Test.Lorentz.Contracts.Stablecoin
  ( test_stablecoin_contract
  , test_setPauseAndSetTransferable
  ) where

import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Lorentz.Test
import Michelson.Test
import qualified Michelson.Typed as T
import Tezos.Core

import Lorentz.Contracts.Stablecoin

initialStorage :: Storage
initialStorage = ()

test_stablecoin_contract :: IO [TestTree]
test_stablecoin_contract =
  testTreesWithTypedContract "contract.tz" $
    \(stablecoinContract :: T.FullContract (T.ToT Parameter) (T.ToT Storage)) -> pure
  [ testCase "transfer" $ integrationalTestExpectation $ do
      validate $ Right expectAnySuccess
  ]

test_setPauseAndSetTransferable :: IO [TestTree]
test_setPauseAndSetTransferable =
  testTreesWithTypedContract "contract.tz" $
    \(stablecoinContract :: T.FullContract (T.ToT Parameter) (T.ToT Storage)) -> pure
  [
    testCase "Call SetPause with pauser rights" $
    integrationalTestExpectation $ do
      contract <- tOriginate stablecoinContract "stablecoinContract" (T.toVal initialStorage) (toMutez 0)
      withSender genesisAddress1 $ lCallDef @Parameter contract Pause
      validate $ Right expectAnySuccess
  , testCase "Call SetPause without pauser rights" $
    integrationalTestExpectation $ do
      contract <- tOriginate stablecoinContract "stablecoinContract" (T.toVal initialStorage) (toMutez 0)
      withSender genesisAddress1 $ lCallDef @Parameter contract Pause
      validate . Left $ lExpectCustomError_ #senderIsNotAdmin
  ]
