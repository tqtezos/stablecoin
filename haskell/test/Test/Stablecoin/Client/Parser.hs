-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Test.Stablecoin.Client.Parser
  ( test_deployCmd
  ) where

import Options.Applicative (ParserResult(Failure, Success), defaultPrefs, execParserPure)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import Stablecoin.Client.Parser
  (ClientArgs(..), ClientArgsRaw(..), ContractMetadataOptions(..),
  DeployContractOptions(dcoContractMetadata), stablecoinClientInfo)

test_deployCmd :: TestTree
test_deployCmd =
  testGroup "deploy command parser"
    [ testGroup "metadata options"
      [ testCase "stores metadata in remote contract by default" $ do
          parseDeployCmd [] \dco ->
            dcoContractMetadata dco @?= OpRemoteContract Nothing

      , testCase "parses description" $ do
          parseDeployCmd ["--description", "d"] \dco ->
            dcoContractMetadata dco @?= OpRemoteContract (Just "d")

      , testCase "parses '--contract-metadata-in-place' option" $ do
          parseDeployCmd ["--contract-metadata-in-place"] \dco ->
            dcoContractMetadata dco @?= OpCurrentContract Nothing

      , testCase "parses '--contract-metadata-in-place' option with description" $ do
          parseDeployCmd ["--contract-metadata-in-place", "--description", "d"] \dco ->
            dcoContractMetadata dco @?= OpCurrentContract (Just "d")

      , testCase "parses '--contract-metadata-off-chain' option" $ do
          parseDeployCmd ["--contract-metadata-off-chain", "uri"] \dco ->
            dcoContractMetadata dco @?= OpRaw "uri"

      , testCase "fails when both '--contract-metadata-off-chain' and '--description' are present" $ do
          failsToParseDeployCmd ["--contract-metadata-off-chain", "uri", "--description", "d"]

      , testCase "fails when no uri is present for '--contract-metadata-off-chain'" $ do
          failsToParseDeployCmd ["--contract-metadata-off-chain"]
      ]
    ]
  where
    parseDeployCmd :: [String] -> (DeployContractOptions -> Assertion) -> Assertion
    parseDeployCmd additionalOpts test = do
      case execParserPure defaultPrefs stablecoinClientInfo (basicDeployOpts <> additionalOpts) of
        Success (ClientArgs _ _ (CmdDeployContract dco)) -> test dco
        Success (ClientArgs _ _ otherCmd) ->
          assertFailure $ toString $ unlines
            [ "Expected parser to succeed with a 'deploy' command, but it parsed this instead:"
            , show otherCmd
            ]
        res ->
          assertFailure $ toString $ unlines
            [ "Expected parser to succeed, but it failed:"
            , show res
            ]

    failsToParseDeployCmd :: [String] -> Assertion
    failsToParseDeployCmd additionalOpts = do
      case execParserPure defaultPrefs stablecoinClientInfo (basicDeployOpts <> additionalOpts) of
        Failure _ -> pass
        res ->
          assertFailure $ toString $ unlines
            [ "Expected parser to failed, but the result was:"
            , show res
            ]

    basicDeployOpts :: [String]
    basicDeployOpts =
      [ "deploy"
      , "--master-minter", "mm"
      , "--contract-owner", "co"
      , "--pauser", "p"
      , "--default-expiry", "123"
      ]
