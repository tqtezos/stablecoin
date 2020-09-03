-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT
module Main
  ( main
  ) where

import Fmt (pretty)
import Options.Applicative (execParser)

import qualified Data.Text.IO.Utf8 as Utf8
import qualified Indigo.Contracts.Transferlist.External as External
import qualified Indigo.Contracts.Transferlist.Internal as Internal
import Lorentz (ToT, compileLorentzContract, toVal)
import Lorentz.Contracts.Test.Common
import Michelson.Runtime (parseExpandContract)
import Michelson.Typed (untypeValue)
import Michelson.Typed.Convert (convertContract)
import Morley.Nettest
import Tezos.Address
import Util.Named

import FA1_2 (fa1_2Scenario)
import qualified Lorentz.Contracts.StablecoinFA1_2 as FA1_2
import Nettest (TransferlistType(External, Internal), scNettestScenario)
import Permit (permitScenario)
import Stablecoin.Client.Cleveland.Caps (runStablecoinClient)
import Stablecoin.Client.Contract (parseStablecoinContract)
import StablecoinClientTest (stablecoinClientScenario)

externalTransferlistContractPath :: FilePath
externalTransferlistContractPath = "test/resources/transferlist.tz"

main :: IO ()
main = do
  let aliasPrefix = "nettest.Stablecoin"
  let clientParser = nettestConfigParser . pure $ Just aliasPrefix
  parsedConfig <- execParser $
    parserInfo
      (#usage .! mempty)
      (#description .! "Stablecoin nettest scenarioWithInternalTransferlist")
      (#header .! "ManagedLedger nettest")
      (#parser .! clientParser)
  stablecoinContract <- parseStablecoinContract
  externalTransferlistFile <- Utf8.readFile externalTransferlistContractPath
  externalTransferlistContract <- either (error "cannot parse transferlist contract") pure $
    parseExpandContract (Just externalTransferlistContractPath) externalTransferlistFile
  let
    originateTransferlistInternal transfers receivers =
      originateUntypedSimple
        "nettest.transferlist_internal"
        (untypeValue . toVal $ Internal.Storage transfers receivers)
        (convertContract $ compileLorentzContract Internal.transferlistContract)

    originateTransferlistExternal :: forall m capsM. (Monad m, capsM ~ NettestT m) => Set (Address, Address) -> Set Address -> capsM Address
    originateTransferlistExternal transfers receivers =
      originateUntypedSimple
        "nettest.transferlist_internal"
        (untypeValue @(ToT External.Storage) . toVal
          $ External.convertToExternalStorage
              (Internal.Storage transfers receivers)
              testOwner -- issuer
              testOwner -- admin
              )
        externalTransferlistContract

    scenarioWithInternalTransferlist :: NettestScenario m
    scenarioWithInternalTransferlist impl = do
      niComment impl "Stablecoin contract nettest scenarioWithInternalTransferlist"
      scNettestScenario
        mkInitialStorage
        stablecoinContract
        originateTransferlistInternal
        Internal
        impl

    scenarioWithExternalTransferlist :: forall m capsM. (Monad m, capsM ~ NettestT m) => NettestImpl m -> m ()
    scenarioWithExternalTransferlist impl = do
      niComment impl "Stablecoin contract nettest scenarioWithExternalTransferlist"
      scNettestScenario
        mkInitialStorage
        stablecoinContract
        (originateTransferlistExternal @m @capsM)
        External
        impl

  env <- mkNettestEnv parsedConfig
  runNettestViaIntegrational scenarioWithInternalTransferlist
  runNettestViaIntegrational scenarioWithExternalTransferlist
  runNettestViaIntegrational (permitScenario stablecoinContract)

  runNettestClient env scenarioWithInternalTransferlist
  runNettestClient env scenarioWithExternalTransferlist
  runNettestClient env (permitScenario stablecoinContract)

  runStablecoinClient
    (ncMorleyClientConfig parsedConfig)
    (neMorleyClientEnv env)
    stablecoinClientScenario

  -- Test the FA1.2 version of the stablecoin contract
  stablecoinContractFA1_2 <-
    case FA1_2.parseStablecoinContract of
      Right c -> pure c
      Left err -> fail $ pretty err
  runNettestClient env (fa1_2Scenario stablecoinContractFA1_2)
