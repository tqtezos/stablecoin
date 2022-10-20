-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Test.TZIP16
  ( test_TZIP16_uri_parser
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Morley.Tezos.Address (formatAddress, ta)

import Lorentz.Contracts.Stablecoin (ParsedMetadataUri(..), parseMetadataUri)

test_TZIP16_uri_parser :: TestTree
test_TZIP16_uri_parser =
  testGroup "uri parser"
    [ testCase "can parse embedded uri" $ do
        let uri = "tezos-storage:hello"

        let actual = InCurrentContractUnderKey "hello"
        expected <- case parseMetadataUri uri of
          Right parsedUri -> pure parsedUri
          _ -> fail "Parsing metadata uri failed"

        actual @?= expected

    , testCase "can parse remote uri" $ do
        let addr = [ta|KT1Wy6Ks9Zxx9cmhDbGWbFhsVwFDASD3hDA3|]
        let uri = "tezos-storage://" <> formatAddress addr <>"/foo"

        let actual = InRemoteContractUnderKey addr "foo"
        expected <- case parseMetadataUri uri of
          Right parsedUri -> pure parsedUri
          _ -> fail "Parsing remote metadata uri failed"

        actual @?= expected
    ]
