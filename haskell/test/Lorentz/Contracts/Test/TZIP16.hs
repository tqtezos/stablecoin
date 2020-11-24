-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Test.TZIP16
  ( test_TZIP16
  ) where

import qualified Data.Aeson as J
import Data.String.Interpolate (i)
import Data.Version (showVersion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Michelson.Typed (ToT)

import Lorentz.Contracts.Spec.TZIP16Interface (Metadata)
import Lorentz.Contracts.Stablecoin (Storage, metadataJSON)
import Paths_stablecoin (version)

test_TZIP16 :: TestTree
test_TZIP16 =
  testGroup "json serializers/deserializers"
    [ testCase "serializes to the expected json format" $ do
        let actual = J.toJSON metadataJSON

        expected <-
          case J.eitherDecode' @J.Value (encodeUtf8 expectedMetadataJSON) of
            Right value -> pure value
            Left err -> fail err

        actual @?= expected

    , testCase "roundtrip" $ do
        let first_ = J.toJSON metadataJSON

        parsed <-
          case J.eitherDecode' @(Metadata (ToT Storage)) (encodeUtf8 expectedMetadataJSON) of
            Right value -> pure value
            Left err -> fail err

        let second_ = J.toJSON parsed

        first_ @?= second_
    ]

expectedMetadataJSON :: String
expectedMetadataJSON =
  [i|
  {
    "name": "stablecoin",
    "homepage": "https://github.com/tqtezos/stablecoin/",
    "version": "#{showVersion version}",
    "interfaces": [
      "TZIP-12",
      "TZIP-17"
    ],
    "authors": [
      "Serokell <https://serokell.io/>",
      "TQ Tezos <https://tqtezos.com/>"
    ],
    "source": {
      "location": "https://github.com/tqtezos/stablecoin/tree/v#{showVersion version}/ligo/stablecoin",
      "tools": [
        "ligo "
      ]
    },
    "license": {
      "name": "MIT"
    },
    "views": [
      {
        "name": "GetDefaultExpiry",
        "pure": true,
        "description": "Access the contract's default expiry in seconds",
        "implementations": [
          {
            "michelson-storage-view": {
              "annotations": [],
              "return-type": {
                "args": [],
                "prim": "nat",
                "annots": []
              },
              "code": [
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                }
              ],
              "parameter": {
                "args": [],
                "prim": "unit",
                "annots": []
              }
            }
          }
        ]
      },
      {
        "name": "GetCounter",
        "pure": true,
        "description": "Access the current permit counter",
        "implementations": [
          {
            "michelson-storage-view": {
              "annotations": [],
              "return-type": {
                "args": [],
                "prim": "nat",
                "annots": []
              },
              "code": [
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CDR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CDR",
                  "annots": []
                },
                {
                  "args": [],
                  "prim": "CAR",
                  "annots": []
                }
              ],
              "parameter": {
                "args": [],
                "prim": "unit",
                "annots": []
              }
            }
          }
        ]
      }
    ],
    "errors": [
      {
        "error": { "string": "FA2_TOKEN_UNDEFINED" },
        "expansion": { "string": "One of the specified `token_id`s is not defined (i.e. not zero)" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "FA2_INSUFFICIENT_BALANCE" },
        "expansion": { "string": "Cannot debit from a wallet because of excessive amount of tokens" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "FA2_NOT_OPERATOR" },
        "expansion": { "string": "A transfer is initiated neither by the token owner nor a permitted operato" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "XTZ_RECEIVED" },
        "expansion": { "string": "Contract received a non-zero amount of tokens and should not proceed any further" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_CONTRACT_OWNER" },
        "expansion": { "string": "Authorized sender is not contract owner" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_PENDING_OWNER" },
        "expansion": { "string": "Authorized sender is not current contract pending owner" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NO_PENDING_OWNER_SET" },
        "expansion": { "string": "Thrown when trying to authorize as pending owner whilst is not set for a contract" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_PAUSER" },
        "expansion": { "string": "Authorized sender is not contract pauser" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_MASTER_MINTER" },
        "expansion": { "string": "Authorized sender is not master minter" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_MINTER" },
        "expansion": { "string": "Sender is not registered as minter" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "CONTRACT_PAUSED" },
        "expansion": { "string": "Operation cannot be performed during contract pause" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "CONTRACT_NOT_PAUSED" },
        "expansion": { "string": "Operation cannot be peformed if the contract is not paused" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_TOKEN_OWNER" },
        "expansion": { "string": "Trying to configure operators for a different wallet which sender does not own" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "CURRENT_ALLOWANCE_REQUIRED" },
        "expansion": { "string": "In `configure_minter` the caller wrongly expects the address to be not a minter" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "ALLOWANCE_MISMATCH" },
        "expansion": { "string": "In `configure_minter` both allowances are not `None`, but different" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "ADDR_NOT_MINTER" },
        "expansion": { "string": "An attempt is made to modify minter data of an address that's not a minter" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "ALLOWANCE_EXCEEDED" },
        "expansion": { "string": "Thrown when trying to mint tokens more than currently allowed for an address" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "BAD_TRANSFERLIST" },
        "expansion": { "string": "Given address is a not a smart contract complying with the transferlist interface" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "MINTER_LIMIT_REACHED" },
        "expansion": { "string": "Cannot add new minter because the number of minters is already at the limit." },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "MISSIGNED" },
        "expansion": { "string": "The signature used to create a permit was invalid." },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "EXPIRED_PERMIT" },
        "expansion": { "string": "The sender tried to access an entrypoint for which a permit was found, but it was expired." },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_PERMIT_ISSUER" },
        "expansion": { "string": "The sender tried to revoke a permit that wasn't theirs and no permit was issued to allow this call." },
        "languages": [ "en" ]
      }
    ]
  }
  |]
