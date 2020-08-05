-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT
module Main
  ( main
  ) where

import Options.Applicative (execParser)
import qualified Unsafe (fromJust)

import qualified Indigo.Contracts.Transferlist.External as External
import qualified Indigo.Contracts.Transferlist.Internal as Internal
import Lorentz (ToT, compileLorentzContract, toVal)
import Lorentz.Contracts.Test.Common
import Michelson.Runtime (parseExpandContract)
import Michelson.Typed (untypeValue)
import Michelson.Typed.Convert (convertContract)
import Morley.Nettest
import Tezos.Address
import Util.IO
import Util.Named

import Nettest (scNettestScenario)
import Stablecoin.Client.Cleveland.Caps (runStablecoinClient)
import Stablecoin.Client.Contract (parseStablecoinContract)
import StablecoinClientTest (stablecoinClientScenario)

externalTransferlistContractPath :: FilePath
externalTransferlistContractPath = "test/resources/transferlist.tz"

main :: IO ()
main = do
  let aliasPrefix = "nettest.Stablecoin"
  let clientParser = clientConfigParser . pure $ Just aliasPrefix
  parsedConfig <- execParser $
    parserInfo
      (#usage .! mempty)
      (#description .! "Stablecoin nettest scenarioWithInternalTransferlist")
      (#header .! "ManagedLedger nettest")
      (#parser .! clientParser)
  stablecoinContract <- parseStablecoinContract
  externalTransferlistFile <- readFileUtf8 externalTransferlistContractPath
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

    scenarioWithInternalTransferlist :: NettestScenario
    scenarioWithInternalTransferlist impl = do
      commentAction impl "Stablecoin contract nettest scenarioWithInternalTransferlist"
      scNettestScenario
        (Unsafe.fromJust . mkInitialStorage) stablecoinContract originateTransferlistInternal impl

    scenarioWithExternalTransferlist :: forall m capsM. (Monad m, capsM ~ NettestT m) => NettestImpl m -> m ()
    scenarioWithExternalTransferlist impl = do
      commentAction impl "Stablecoin contract nettest scenarioWithExternalTransferlist"
      scNettestScenario
        (Unsafe.fromJust . mkInitialStorage) stablecoinContract (originateTransferlistExternal @m @capsM) impl

  env <- mkMorleyClientEnv parsedConfig
  runNettestViaIntegrational scenarioWithInternalTransferlist
  runNettestViaIntegrational scenarioWithExternalTransferlist

  runNettestClient env scenarioWithInternalTransferlist
  runNettestClient env scenarioWithExternalTransferlist

  runStablecoinClient parsedConfig env (stablecoinClientScenario aliasPrefix)
