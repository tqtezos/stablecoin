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
        "expansion": { "string": "All `token_id`s must be 0" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "FA2_INSUFFICIENT_BALANCE" },
        "expansion": { "string": "Cannot debit from a wallet because of insufficient amount of tokens" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "FA2_NOT_OPERATOR" },
        "expansion": { "string": "You're neither the owner or a permitted operator of one or more wallets from which tokens will be transferred" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "XTZ_RECEIVED" },
        "expansion": { "string": "Contract received a non-zero amount of tokens" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_CONTRACT_OWNER" },
        "expansion": { "string": "Operation can only be performed by the contract's owner" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_PENDING_OWNER" },
        "expansion": { "string": "Operation can only be performed by the current contract's pending owner" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NO_PENDING_OWNER_SET" },
        "expansion": { "string": "There's no pending transfer of ownership" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_PAUSER" },
        "expansion": { "string": "Operation can only be performed by the contract's pauser" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_MASTER_MINTER" },
        "expansion": { "string": "Operation can only be performed by the contract's master minter" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_MINTER" },
        "expansion": { "string": "Operation can only be performed by registered minters" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "CONTRACT_PAUSED" },
        "expansion": { "string": "Operation cannot be performed while the contract is paused" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "CONTRACT_NOT_PAUSED" },
        "expansion": { "string": "Operation cannot be performed while the contract is not paused" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "NOT_TOKEN_OWNER" },
        "expansion": { "string": "You cannot configure another user's operators" },
        "languages": [ "en" ]
      },
      {
        "error": { "string": "CURRENT_ALLOWANCE_REQUIRED" },
        "expansion": { "string": "The given address is already a minter, you must specify its current minting allowance" },
        "languages": [ "en" ]
      }
    ]
  }
  |]

-- TODO: Include this in the above list of errors
--{
--  "error": { "string": "ALLOWANCE_MISMATCH" },
--  "expansion": { "string": "The given current minting allowance does not match the minter's actual current minting allowance" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "ADDR_NOT_MINTER" },
--  "expansion": { "string": "This address is not a registered minter" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "ALLOWANCE_EXCEEDED" },
--  "expansion": { "string": "The amount of tokens to be minted exceeds your current minting allowance" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "BAD_TRANSFERLIST" },
--  "expansion": { "string": "The given address is a not a smart contract complying with the transferlist interface" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "MINTER_LIMIT_REACHED" },
--  "expansion": { "string": "Cannot add new minter because the number of minters is already at the limit" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "MISSIGNED" },
--  "expansion": { "string": "This permit's signature is invalid" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "EXPIRED_PERMIT" },
--  "expansion": { "string": "A permit was found, but it has already expired" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "NOT_PERMIT_ISSUER" },
--  "expansion": { "string": "You're not the issuer of the given permit" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "DUP_PERMIT" },
--  "expansion": { "string": "The given permit already exists" },
--  "languages": [ "en" ]
--},
--{
--  "error": { "string": "EXPIRY_TOO_BIG" },
--  "expansion": { "string": "The `set_expiry` entrypoint was called with an expiry value that is too big" },
--  "languages": [ "en" ]
--}
