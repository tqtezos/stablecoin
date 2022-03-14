-- SPDX-FileCopyrightText: 2022 TQ Tezos
-- SPDX-License-Identifier: MIT

module Tests
  ( test_permitScenario
  , test_fa1_2ComparisonScenario
  , test_stablecoinClientScenario
  , test_scenarioWithExternalTransferlist
  , test_scenarioWithInternalTransferlist
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import qualified Data.Text.IO.Utf8 as Utf8
import qualified Indigo.Contracts.Transferlist.External as External
import qualified Indigo.Contracts.Transferlist.Internal as Internal
import Lorentz (ToT, toVal)
import Morley.Michelson.Runtime (parseExpandContract)
import Morley.Michelson.Typed (untypeValue)
import qualified Morley.Michelson.Untyped as U (Contract)
import Test.Cleveland
import Test.Cleveland.Tasty.Internal (whenNetworkEnabled)
import Morley.Michelson.Parser.Types
import Morley.Tezos.Address

import FA1_2Comparison (fa1_2ComparisonScenario)
import Nettest (TransferlistType(External, Internal), scNettestScenario)
import Permit (permitScenario)
import Stablecoin.Client.Cleveland.Caps (runStablecoinClient)
import StablecoinClientTest (stablecoinClientScenario)

externalTransferlistContractPath :: FilePath
externalTransferlistContractPath = "test/resources/transferlist.tz"

originateTransferlistInternal :: MonadOps f => Set (Address, Address) -> Set Address -> f Address
originateTransferlistInternal transfers receivers = chAddress <$>
  originateSimple
    "nettest.transferlist_internal"
    (Internal.Storage transfers receivers)
    (Internal.transferlistContract)

originateTransferlistExternal
  :: MonadCleveland caps m
  => U.Contract -> Set (Address, Address) -> Set Address -> m Address
originateTransferlistExternal externalTransferlistContract transfers receivers = do
  testOwner <- newAddress "testOwner"
  originateUntypedSimple
    "nettest.transferlist_internal"
    (untypeValue @(ToT External.Storage) . toVal
      $ External.convertToExternalStorage
          (Internal.Storage transfers receivers)
          testOwner -- issuer
          testOwner -- admin
          )
    externalTransferlistContract

test_scenarioWithInternalTransferlist :: TestTree
test_scenarioWithInternalTransferlist =
  testScenario "Stablecoin contract nettest scenarioWithInternalTransferlist" $
    scNettestScenario
      originateTransferlistInternal
      Internal

test_scenarioWithExternalTransferlist :: IO TestTree
test_scenarioWithExternalTransferlist = do
  externalTransferlistFile <- Utf8.readFile externalTransferlistContractPath
  externalTransferlistContract <- either (error "cannot parse transferlist contract") pure $
    parseExpandContract (MSFile externalTransferlistContractPath) externalTransferlistFile

  pure $ testScenario "Stablecoin contract nettest scenarioWithExternalTransferlist" $
    scNettestScenario
      (originateTransferlistExternal externalTransferlistContract)
      External

test_permitScenario :: TestTree
test_permitScenario = testScenario "permitScenario" permitScenario

test_fa1_2ComparisonScenario :: TestTree
test_fa1_2ComparisonScenario = testScenario "fa1_2ComparisonScenario" fa1_2ComparisonScenario

test_stablecoinClientScenario :: TestTree
test_stablecoinClientScenario =
  whenNetworkEnabled $ \withEnv ->
    testCase "stablecoinClientScenario" do
      withEnv (`runStablecoinClient` stablecoinClientScenario)
