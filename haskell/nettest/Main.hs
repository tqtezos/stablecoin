-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT
module Main
  ( main
  ) where

import Options.Applicative (execParser)
import qualified Unsafe (fromJust)

import Lorentz (compileLorentzContract)
import Michelson.Runtime (prepareContract)
import Michelson.Typed.Convert (convertContract)
import Morley.Nettest
import Util.Named

import Indigo.Contracts.Safelist (safelistContract)
import Lorentz.Contracts.Test.Common
import Nettest (scTransferScenario)

main :: IO ()
main = do
  let clientParser = clientConfigParser . pure $ Just "nettest.Stablecoin"
  parsedConfig <- execParser $
    parserInfo
      (#usage .! mempty)
      (#description .! "Stablecoin nettest scenario")
      (#header .! "ManagedLedger nettest")
      (#parser .! clientParser)
  stablecoinContract <- prepareContract $ Just "test/resources/stablecoin.tz"
  let safelistContractCompiled = convertContract $ compileLorentzContract safelistContract
  let
    scenario :: NettestScenario
    scenario impl = do
      commentAction impl "Stablecoin contract nettest scenario"
      scTransferScenario
        (Unsafe.fromJust . mkInitialStorage) stablecoinContract safelistContractCompiled impl

  env <- mkMorleyClientEnv parsedConfig
  runNettestViaIntegrational scenario
  runNettestClient env scenario
