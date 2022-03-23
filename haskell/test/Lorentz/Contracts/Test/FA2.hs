-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{- | Tests for FA2 interface.
 https://gitlab.com/tezos/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md
-}
module Lorentz.Contracts.Test.FA2
  ( OriginationParams (..)
  , addAccount
  , defaultOriginationParams
  , fa2Spec
  , fa2NotOperator
  , fa2NotOwner
  ) where

import Data.Map as M (fromList)
import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.Spec.FA2Interface as FA2 hiding (ParameterC)
import Lorentz.Contracts.Stablecoin (ParameterC, Storage, sLedger, sOperators)
import Lorentz.Contracts.Test.Common

import Lorentz.Value
import Morley.Metadata (ViewParam(..))
import Morley.Util.Named
import Test.Cleveland
import Test.Cleveland.Lorentz.Consumer
import Test.Morley.Metadata

import Lorentz.Contracts.Test.AsRPC ()

fa2TokenUndefined
  , fa2InsufficientBalance
  , fa2NotOwner
  , fa2NotOperator
  :: MonadCleveland caps m => m () -> m ()
fa2TokenUndefined = expectFailedWith [mt|FA2_TOKEN_UNDEFINED|]
fa2InsufficientBalance = expectFailedWith [mt|FA2_INSUFFICIENT_BALANCE|]
fa2NotOwner = expectFailedWith [mt|NOT_TOKEN_OWNER|]
fa2NotOperator = expectFailedWith [mt|FA2_NOT_OPERATOR|]

{- | Test suite for an FA2-specific entrypoints for stablecoin smart-contract which:

 1. Supports a single token type.
 2. Does not have an external permission checking transfer hook.
-}
fa2Spec
  :: forall param st.
     (IsoValue st, ParameterC param)
  => (forall caps m. MonadCleveland caps m => OriginationFn param st m)
  -> [TestTree]
fa2Spec fa2Originate =
  [ testGroup
      "Operator Transfer"
      -- Transfer tests or tests for core transfer behavior, as per FA2
      [ testScenario
          "is allowed to transfer and \
          \executes transfers in the given order"
          $ scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            wallet5 <- newAddress "wallet5"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            -- Tests transactions are applied in order
            -- Update balances exactly
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 0)) $
                      addAccount (wallet3, (commonOperators, 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            let transfers =
                  constructTransfers
                    [ (#from_ :! wallet1, [(#to_ :! wallet2, #amount :! 10)])
                    , (#from_ :! wallet2, [(#to_ :! wallet3, #amount :! 10)])
                    , (#from_ :! wallet3, [(#to_ :! wallet4, #amount :! 10)])
                    , (#from_ :! wallet4, [(#to_ :! wallet5, #amount :! 10)])
                    ]

            withSender commonOperator $ call fa2contract (Call @"Transfer") transfers

            consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])
            let balanceRequestItems =
                  [ BalanceRequestItem {briOwner = wallet1, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = wallet2, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = wallet3, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = wallet4, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = wallet5, briTokenId = theTokenId}
                  ]
                balanceRequest = mkFA2View balanceRequestItems consumer
                balanceExpected =
                  [ BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = wallet1, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = wallet2, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = wallet3, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = wallet4, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = wallet5, briTokenId = theTokenId}
                      , briBalance = 10
                      }
                  ]

            call fa2contract (Call @"Balance_of") balanceRequest
            getStorage consumer @@== [balanceExpected]
      , testScenario "allows zero transfer from non-existent accounts" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let originationParams =
                  addOperator (wallet1, commonOperator) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 0)

              withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to low balance)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            wallet5 <- newAddress "wallet5"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 0)) $
                      addAccount (wallet3, (commonOperators, 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructTransfersFromSender
                      (#from_ :! wallet1)
                      [ (#to_ :! wallet2, #amount :! 10)
                      , (#to_ :! wallet3, #amount :! 10)
                      , (#to_ :! wallet3, #amount :! 10)
                      , (#to_ :! wallet5, #amount :! 10)
                      ]

              fa2InsufficientBalance $
                withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to non existent source account)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addOperator (wallet2, commonOperator) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet2)
                      (#to_ :! wallet1)
                      (#amount :! 1)

              fa2InsufficientBalance $
                withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to bad operator)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            wallet5 <- newAddress "wallet5"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 0)) $
                      addAccount (wallet3, ([], 10)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers1 =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 1)

                  transfers2 =
                    constructTransfersFromSender
                      (#from_ :! wallet3)
                      [ (#to_ :! wallet1, #amount :! 1)
                      , (#to_ :! wallet4, #amount :! 1)
                      , (#to_ :! wallet5, #amount :! 1)
                      ]
                  transfers = transfers1 <> transfers2

              fa2NotOperator $
                withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to bad operator for zero transfer)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 0)) $
                      addAccount (wallet3, ([], 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers1 =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 1)

                  transfers2 =
                    constructSingleTransfer
                      (#from_ :! wallet3)
                      (#to_ :! wallet2)
                      (#amount :! 0)

                  transfers = transfers1 <> transfers2

              fa2NotOperator $
                withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "accepts an empty list of transfers" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers = [TransferItem wallet1 []]

              withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "validates token id" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    [ TransferItem
                        { tiFrom = wallet1
                        , tiTxs =
                            [ TransferDestination wallet2 theTokenId 5
                            , TransferDestination wallet3 oneTokenId 1
                            , TransferDestination wallet4 theTokenId 4
                            ]
                        }
                    ]

              fa2TokenUndefined $
                withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "cannot transfer foreign money" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet2, ([], 10)) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet2)
                      (#to_ :! wallet1)
                      (#amount :! 1)

              fa2NotOperator $
                withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "will create target account if it does not already exist" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 5)

              withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
              consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])

              let balanceRequestItem = BalanceRequestItem {briOwner = wallet2, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected = [BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 5}]

              call fa2contract (Call @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      , testScenario "removes zero balance accounts after transfer" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet3, (commonOperators, 0)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)

            stablecoinContract <- fa2Originate originationParams
            do
              let transfer1 =
                    constructTransfersFromSender
                      (#from_ :! wallet1)
                      [ (#to_ :! wallet2, #amount :! 10)
                      ]

              withSender commonOperator $ call stablecoinContract (Call @"Transfer") transfer1

              storage <- getStorage @Storage (chAddress stablecoinContract)
              val <- getBigMapValueMaybe (sLedger storage) wallet1
              unless (isNothing val) $
                failure "Zero balance account was not removed"
      ]
  , testGroup "Self transfer" $
      [ testScenario "Cannot transfer foreign money" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  ( addAccount (wallet1, (commonOperators, 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                  )
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 1)

              fa2NotOperator $
                withSender wallet2 $ call fa2contract (Call @"Transfer") transfers
      , testScenario "is permitted" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])

            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 5)

              withSender wallet1 $ call fa2contract (Call @"Transfer") transfers
              let balanceRequestItem = BalanceRequestItem {briOwner = wallet2, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected = [BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 5}]
              call fa2contract (Call @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      , testScenario "validates token id" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    [ TransferItem
                        { tiFrom = wallet1
                        , tiTxs =
                            [ TransferDestination
                                { tdTo = wallet2
                                , tdTokenId = oneTokenId
                                , tdAmount = 10
                                }
                            ]
                        }
                    ]

              fa2TokenUndefined $
                withSender wallet1 $ call fa2contract (Call @"Transfer") transfers
      , testScenario "allows zero transfer from non-existent accounts" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            -- Tests transactions are applied in order
            -- Update balances exactly
            fa2contract <-
              fa2Originate $
                defaultOriginationParams
                  (#owner :! testOwner)
                  (#pauser :! testPauser)
                  (#masterMinter :! testMasterMinter)
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 0)

              withSender wallet1 $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to low source balance)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            wallet5 <- newAddress "wallet5"
            let originationParams =
                  addAccount (wallet1, ([], 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            -- Tests transactions are applied atomically
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructTransfersFromSender
                      (#from_ :! wallet1)
                      [ (#to_ :! wallet2, #amount :! 4)
                      , (#to_ :! wallet3, #amount :! 4)
                      , (#to_ :! wallet4, #amount :! 4) -- Should fail
                      , (#to_ :! wallet5, #amount :! 2)
                      ]

              fa2InsufficientBalance $ withSender wallet1 $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to non existent source for non-zero transfer)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet3 <- newAddress "wallet3"
            commonOperator <- newAddress "commonOperator"
            let originationParams =
                  addOperator (wallet3, commonOperator) $
                    addAccount (wallet1, ([], 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
            -- Tests transactions are applied atomically
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet3)
                      (#to_ :! wallet1)
                      (#amount :! 2)

              fa2InsufficientBalance $ withSender commonOperator $ call fa2contract (Call @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to bad token id)" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            wallet5 <- newAddress "wallet5"
            let originationParams =
                  addAccount (wallet1, ([], 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            -- Tests transactions are applied atomically
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    [ TransferItem
                        { tiFrom = wallet1
                        , tiTxs =
                            [ TransferDestination {tdTo = wallet2, tdTokenId = theTokenId, tdAmount = 1}
                            , TransferDestination {tdTo = wallet3, tdTokenId = theTokenId, tdAmount = 1}
                            , TransferDestination {tdTo = wallet4, tdTokenId = oneTokenId, tdAmount = 1} -- Should fail
                            , TransferDestination {tdTo = wallet5, tdTokenId = theTokenId, tdAmount = 1}
                            ]
                        }
                    ]

              fa2TokenUndefined $ withSender wallet1 $ call fa2contract (Call @"Transfer") transfers
      , testScenario "will create target account if it does not already exist" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let originationParams =
                  addAccount (wallet1, ([], 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! wallet2)
                      (#amount :! 5)

              withSender wallet1 $ call fa2contract (Call @"Transfer") transfers
              consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])

              let balanceRequestItem = BalanceRequestItem {briOwner = wallet2, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected = [BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 5}]

              call fa2contract (Call @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      ]
  , -- Balance_of tests
    testGroup "Balance_of entrypoint" $
      [ testScenario "returns results in the expected order and without de-duplication" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            wallet5 <- newAddress "wallet5"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 20)) $
                      addAccount (wallet3, (commonOperators, 30)) $
                        addAccount (wallet4, (commonOperators, 40)) $
                          addAccount (wallet5, (commonOperators, 50)) $
                            defaultOriginationParams
                              (#owner :! testOwner)
                              (#pauser :! testPauser)
                              (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItems =
                    [ BalanceRequestItem {briOwner = wallet1, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = wallet4, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = wallet3, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = wallet5, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = wallet2, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = wallet3, briTokenId = theTokenId}
                    ]
                  balanceRequest = mkFA2View balanceRequestItems consumer
                  balanceExpected =
                    [ BalanceResponseItem {briRequest = BalanceRequestItem wallet1 theTokenId, briBalance = 10}
                    , BalanceResponseItem {briRequest = BalanceRequestItem wallet4 theTokenId, briBalance = 40}
                    , BalanceResponseItem {briRequest = BalanceRequestItem wallet3 theTokenId, briBalance = 30}
                    , BalanceResponseItem {briRequest = BalanceRequestItem wallet5 theTokenId, briBalance = 50}
                    , BalanceResponseItem {briRequest = BalanceRequestItem wallet2 theTokenId, briBalance = 20}
                    , BalanceResponseItem {briRequest = BalanceRequestItem wallet3 theTokenId, briBalance = 30}
                    ]

              call fa2contract (Call @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      , testScenario "validates token id" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItem = BalanceRequestItem {briOwner = wallet1, briTokenId = oneTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer

              fa2TokenUndefined $ call fa2contract (Call @"Balance_of") balanceRequest
      , testScenario "returns zero if the account does not exist" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            let originationParams =
                  defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItem = BalanceRequestItem {briOwner = wallet1, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected =
                    [ BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 0}
                    ]

              call fa2contract (Call @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]
      , testScenario "accepts an empty list" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            let originationParams =
                  defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              consumer <- originateSimple "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItems = []
                  balanceRequest = mkFA2View balanceRequestItems consumer
                  balanceExpected = []

              call fa2contract (Call @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]
      ]
  , -- These tests require permission descriptor to be configured that allows for operator transfer.
    -- We have such a configuration set by default in$  defaultOriginationParams.
    let checkForOperator stablecoinContract owner operator expectation = do
          storage <- getStorage @Storage (chAddress stablecoinContract)
          val <- getBigMapValueMaybe (sOperators storage) (owner, operator)
          let found = isJust val
          if found /= expectation
            then
              failure $
                "Unexpected operator status. Expected: "
                  <> show expectation
                  <> " Found: "
                  <> show found
            else pass
     in testGroup "Configure operators entrypoint" $
          [ testScenario "add operator call adds operator as expected" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                wallet2 <- newAddress "wallet2"
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId}

                    let addOperatorParam = AddOperator operatorParam
                    call fa2contract (Call @"Update_operators") [addOperatorParam]

                    checkForOperator fa2contract wallet1 wallet2 True
          , testScenario "remove operator call removes operator as expected" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId}

                    let removeOperatorParam = RemoveOperator operatorParam
                    call fa2contract (Call @"Update_operators") [removeOperatorParam]

                    checkForOperator fa2contract wallet1 commonOperator False
          , testScenario "retains the last operation in case of conflicting operations - Expect removal" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                wallet2 <- newAddress "wallet2"
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId}

                    call fa2contract (Call @"Update_operators") [AddOperator operatorParam, RemoveOperator operatorParam]

                    checkForOperator fa2contract wallet1 wallet2 False
          , testScenario "retains the last operation in case of conflicting operations - Expect addition" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                wallet2 <- newAddress "wallet2"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId}

                    call fa2contract (Call @"Update_operators") [RemoveOperator operatorParam, AddOperator operatorParam]

                    checkForOperator fa2contract wallet1 wallet2 True
          , testScenario "add operator call validates token id" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = oneTokenId}

                    fa2TokenUndefined $
                      call
                        fa2contract
                        (Call @"Update_operators")
                        [AddOperator operatorParam]
          , testScenario "remove operator call validates token id" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = oneTokenId}

                    fa2TokenUndefined $
                      call
                        fa2contract
                        (Call @"Update_operators")
                        [RemoveOperator operatorParam]
          , testScenario "add operator call can be paused" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender testPauser $ call fa2contract (Call @"Pause") ()

                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId}

                    mgmContractPaused $
                      call
                        fa2contract
                        (Call @"Update_operators")
                        [AddOperator operatorParam]
          , testScenario "remove operator call can be paused" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- fa2Originate originationParams
                do
                  withSender testPauser $ call fa2contract (Call @"Pause") ()

                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId}

                    mgmContractPaused $
                      call
                        fa2contract
                        (Call @"Update_operators")
                        [RemoveOperator operatorParam]
          ]
  , -- Check whether "update operator", "remove operator" operations are executed only by contract owner.
    testGroup "Configure operators entrypoint" $
      [ testScenario "denies addOperator call for non-owners" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- fa2Originate originationParams
            do
              withSender wallet2 $ do
                let operatorParam = OperatorParam {opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId}

                let addOperatorParam = AddOperator operatorParam
                fa2NotOwner $ call stablecoinContract (Call @"Update_operators") [addOperatorParam]
      , testScenario "denies removeOperator call for non-owners" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- fa2Originate originationParams
            do
              withSender wallet2 $ do
                let operatorParam = OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId}

                let removeOperatorParam = RemoveOperator operatorParam
                fa2NotOwner $ call stablecoinContract (Call @"Update_operators") [removeOperatorParam]
      , testScenario "denies addOperator for operators" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- fa2Originate originationParams
            do
              withSender commonOperator $ do
                let operatorParam = OperatorParam {opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId}

                let addOperatorParam = AddOperator operatorParam
                fa2NotOwner $ call stablecoinContract (Call @"Update_operators") [addOperatorParam]
      , testScenario "denies removeOperator for operators" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- fa2Originate originationParams
            do
              withSender commonOperator $ do
                let operatorParam = OperatorParam {opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId}

                let removeOperatorParam = RemoveOperator operatorParam
                fa2NotOwner $ call stablecoinContract (Call @"Update_operators") [removeOperatorParam]
      ]
  , ---- Owner hook tests
    --
    ---- The contract implements the default (as per FA2) permission descriptor and thus does not
    ---- call the hooks in either sender or receiver addresses.
    testGroup "Owner hook behavior on transfer" $
      [ testScenario "does not call sender's transfer hook on transfer" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            senderWithHook <- originateSimple "Sender hook consumer" [] $ contractConsumer @FA2OwnerHook
            let originationParams =
                  addAccount (chAddress senderWithHook, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! chAddress senderWithHook)
                      (#to_ :! wallet2)
                      (#amount :! 10)

              withSender commonOperator $ call fa2contract (Call @"Transfer") transfers

              getStorage @[FA2OwnerHook] senderWithHook @@== []
      , testScenario "does not call receiver's transfer hook on transfer" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            receiverWithHook <- chAddress <$> originateSimple @FA2OwnerHook "Receiver hook consumer" [] contractConsumer
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! wallet1)
                      (#to_ :! receiverWithHook)
                      (#amount :! 10)

              withSender commonOperator $ call fa2contract (Call @"Transfer") transfers

              getStorage @[FA2OwnerHook] receiverWithHook @@== []
      ]
  , testGroup
      "Off-chain storage view getBalance"
      [ testScenarioOnEmulator "computes the balance of the address correctly" $
          scenarioEmulated do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 0)) $
                      addAccount (wallet3, (commonOperators, 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              callOffChainView fa2contract "get_balance" (ViewParam (wallet1, 0 :: Natural)) @@== (10 :: Natural)
      ]
  , testGroup "Off-chain storage view getTotalSupply" $
      [ testScenarioOnEmulator "Returns the total supply value correctly" $
          scenarioEmulated do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 25)) $
                      addAccount (wallet3, (commonOperators, 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              callOffChainView fa2contract "total_supply" (ViewParam (0 :: Natural)) @@== (35 :: Natural)
      ]
  , testGroup "Off-chain storage view getAllTokens" $
      [ testScenarioOnEmulator "Returns only token id of Zero" $
          scenarioEmulated do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, (commonOperators, 25)) $
                      addAccount (wallet3, (commonOperators, 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              callOffChainView fa2contract "all_tokens" NoParam @@== [0 :: Natural]
      ]
  , testGroup "Off-chain storage view isOperator" $
      [ testScenarioOnEmulator "Returns the status of operator" $
          scenarioEmulated do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, ([], 25)) $
                      addAccount (wallet3, ([], 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              callOffChainView fa2contract "is_operator" (ViewParam (wallet1, (commonOperator, 0 :: Natural))) @@== True
      ]
  , testGroup "Off-chain storage view GetTokenMetadata" $
      [ testScenarioOnEmulator "Returns the metadata of token" $
          scenarioEmulated do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            wallet4 <- newAddress "wallet4"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet2, ([], 25)) $
                      addAccount (wallet3, ([], 0)) $
                        addAccount (wallet4, (commonOperators, 0)) $
                          defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
            fa2contract <- fa2Originate originationParams
            do
              Showing <$> (callOffChainView fa2contract "token_metadata" (ViewParam (0 :: Natural)))
                @@== Showing
                  ( 0 :: Natural
                  , M.fromList
                    [ ([mt|decimals|], "3" :: ByteString)
                    , ([mt|name|], "TEST")
                    , ([mt|symbol|], "TEST")
                    ]
                  )
      ]
  ]
