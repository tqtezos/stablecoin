-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{- | Tests for FA2 interface.
 https://gitlab.com/tezos/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md
-}
module Lorentz.Contracts.Test.FA2
  ( test_FA2
  , fa2NotOperator
  , fa2NotOwner
  ) where

import Data.Map as M (fromList)
import Fmt (pretty)
import Test.Tasty (TestTree, testGroup)

import Lorentz.Contracts.Spec.FA2Interface as FA2 hiding (Parameter, ParameterC)
import Lorentz.Contracts.Stablecoin (Parameter, Storage, sLedgerRPC, sOperatorsRPC)
import Lorentz.Contracts.Test.Common

import Lorentz.Value
import Morley.Util.Named
import Morley.Util.SizedList qualified as SL
import Morley.Util.SizedList.Types
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

data TestAddresses = TestAddresses
  { testOwner        :: ImplicitAddressWithAlias
  , testPauser       :: ImplicitAddressWithAlias
  , testMasterMinter :: ImplicitAddressWithAlias
  , wallet1          :: ImplicitAddressWithAlias
  , wallet2          :: ImplicitAddressWithAlias
  , wallet3          :: ImplicitAddressWithAlias
  , wallet4          :: ImplicitAddressWithAlias
  , wallet5          :: ImplicitAddressWithAlias
  , commonOperator   :: ImplicitAddressWithAlias
  }

setupAddresses
  :: forall m caps.
     ( MonadFail m
     , MonadCleveland caps m)
  => m TestAddresses
setupAddresses = do
  testOwner
    ::< testPauser
    ::< testMasterMinter
    ::< commonOperator
    ::< wallet1
    ::< wallet2
    ::< wallet3
    ::< wallet4
    ::< wallet5
    ::< Nil
    <- newAddresses $ "testOwner"
      :< "testPauser"
      :< "testMasterMinter"
      :< "commonOperator"
      :< SL.generate @5 (\n -> fromString $ "wallet" <> show (n + 1))
  pure TestAddresses{..}

{- | Test suite for an FA2-specific entrypoints for stablecoin smart-contract which:

 1. Supports a single token type.
 2. Does not have an external permission checking transfer hook.
-}
test_FA2 :: [TestTree]
test_FA2 =
  [ testGroup
      "Operator Transfer"
      -- Transfer tests or tests for core transfer behavior, as per FA2
      [ testScenario
          "is allowed to transfer and executes transfers in the given order"
          $ scenario do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            let transfers =
                  constructTransfers
                    [ (#from_ :! wallet1, [(#to_ :! wallet2, #amount :! 10)])
                    , (#from_ :! wallet2, [(#to_ :! wallet3, #amount :! 10)])
                    , (#from_ :! wallet3, [(#to_ :! wallet4, #amount :! 10)])
                    , (#from_ :! wallet4, [(#to_ :! wallet5, #amount :! 10)])
                    ]

            withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers

            consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])
            let balanceRequestItems =
                  [ BalanceRequestItem {briOwner = toAddress wallet1, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = toAddress wallet2, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = toAddress wallet3, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = toAddress wallet4, briTokenId = theTokenId}
                  , BalanceRequestItem {briOwner = toAddress wallet5, briTokenId = theTokenId}
                  ]
                balanceRequest = mkFA2View balanceRequestItems consumer
                balanceExpected =
                  [ BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = toAddress wallet1, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = toAddress wallet2, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = toAddress wallet3, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = toAddress wallet4, briTokenId = theTokenId}
                      , briBalance = 0
                      }
                  , BalanceResponseItem
                      { briRequest = BalanceRequestItem {briOwner = toAddress wallet5, briTokenId = theTokenId}
                      , briBalance = 10
                      }
                  ]

            transfer fa2contract $ calling (ep @"Balance_of") balanceRequest
            getStorage consumer @@== [balanceExpected]
      , testScenario "allows zero transfer from non-existent accounts" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  addOperator (wallet1, commonOperator) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 0)

              withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to low balance)" $
          scenario do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
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
                withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to non existent source account)" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addOperator (wallet2, commonOperator) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet2)
                      (#to_ :! toAddress wallet1)
                      (#amount :! 1)

              fa2InsufficientBalance $
                withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to bad operator)" $
          scenario do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              let transfers1 =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
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
                withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to bad operator for zero transfer)" $
          scenario do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              let transfers1 =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 1)

                  transfers2 =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet3)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 0)

                  transfers = transfers1 <> transfers2

              fa2NotOperator $
                withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "accepts an empty list of transfers" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers = [TransferItem (toAddress wallet1) []]

              withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "validates token id" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    [ TransferItem
                        { tiFrom = toAddress wallet1
                        , tiTxs =
                            [ TransferDestination (toAddress wallet2) theTokenId 5
                            , TransferDestination (toAddress wallet3) oneTokenId 1
                            , TransferDestination (toAddress wallet4) theTokenId 4
                            ]
                        }
                    ]

              fa2TokenUndefined $
                withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "cannot transfer foreign money" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet2, ([], 10)) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet2)
                      (#to_ :! toAddress wallet1)
                      (#amount :! 1)

              fa2NotOperator $
                withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "will create target account if it does not already exist" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 5)

              withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
              consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])

              let balanceRequestItem = BalanceRequestItem {briOwner = toAddress wallet2, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected = [BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 5}]

              transfer fa2contract $ calling (ep @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      , testScenario "removes zero balance accounts after transfer" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addAccount (wallet3, (commonOperators, 0)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)

            stablecoinContract <- originateStablecoin originationParams
            do
              let transfer1 =
                    constructTransfersFromSender
                      (#from_ :! wallet1)
                      [ (#to_ :! wallet2, #amount :! 10)
                      ]

              withSender commonOperator $ transfer stablecoinContract $ calling (ep @"Transfer") transfer1

              storage <- getStorage stablecoinContract
              val <- getBigMapValueMaybe (sLedgerRPC storage) (toAddress wallet1)
              unless (isNothing val) $
                failure "Zero balance account was not removed"
      ]
  , testGroup "Self transfer" $
      [ testScenario "Cannot transfer foreign money" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  ( addAccount (wallet1, (commonOperators, 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                  )
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 1)

              fa2NotOperator $
                withSender wallet2 $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "is permitted" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])

            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 5)

              withSender wallet1 $ transfer fa2contract $ calling (ep @"Transfer") transfers
              let balanceRequestItem = BalanceRequestItem {briOwner = toAddress wallet2, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected = [BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 5}]
              transfer fa2contract $ calling (ep @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      , testScenario "validates token id" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    [ TransferItem
                        { tiFrom = toAddress wallet1
                        , tiTxs =
                            [ TransferDestination
                                { tdTo = toAddress wallet2
                                , tdTokenId = oneTokenId
                                , tdAmount = 10
                                }
                            ]
                        }
                    ]

              fa2TokenUndefined $
                withSender wallet1 $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "allows zero transfer from non-existent accounts" $
          scenario do
            TestAddresses{..} <- setupAddresses
            -- Tests transactions are applied in order
            -- Update balances exactly
            fa2contract <-
              originateStablecoin $
                defaultOriginationParams
                  (#owner :! testOwner)
                  (#pauser :! testPauser)
                  (#masterMinter :! testMasterMinter)
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 0)

              withSender wallet1 $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to low source balance)" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  addAccount (wallet1, ([], 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            -- Tests transactions are applied atomically
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructTransfersFromSender
                      (#from_ :! wallet1)
                      [ (#to_ :! wallet2, #amount :! 4)
                      , (#to_ :! wallet3, #amount :! 4)
                      , (#to_ :! wallet4, #amount :! 4) -- Should fail
                      , (#to_ :! wallet5, #amount :! 2)
                      ]

              fa2InsufficientBalance $ withSender wallet1 $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to non existent source for non-zero transfer)" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  addOperator (wallet3, commonOperator) $
                    addAccount (wallet1, ([], 10)) $
                      defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
            -- Tests transactions are applied atomically
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet3)
                      (#to_ :! toAddress wallet1)
                      (#amount :! 2)

              fa2InsufficientBalance $ withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "aborts if there is a failure (due to bad token id)" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  addAccount (wallet1, ([], 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            -- Tests transactions are applied atomically
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    [ TransferItem
                        { tiFrom = toAddress wallet1
                        , tiTxs =
                            [ TransferDestination {tdTo = toAddress wallet2, tdTokenId = theTokenId, tdAmount = 1}
                            , TransferDestination {tdTo = toAddress wallet3, tdTokenId = theTokenId, tdAmount = 1}
                            , TransferDestination {tdTo = toAddress wallet4, tdTokenId = oneTokenId, tdAmount = 1} -- Should fail
                            , TransferDestination {tdTo = toAddress wallet5, tdTokenId = theTokenId, tdAmount = 1}
                            ]
                        }
                    ]

              fa2TokenUndefined $ withSender wallet1 $ transfer fa2contract $ calling (ep @"Transfer") transfers
      , testScenario "will create target account if it does not already exist" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  addAccount (wallet1, ([], 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 5)

              withSender wallet1 $ transfer fa2contract $ calling (ep @"Transfer") transfers
              consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])

              let balanceRequestItem = BalanceRequestItem {briOwner = toAddress wallet2, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected = [BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 5}]

              transfer fa2contract $ calling (ep @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      ]
  , -- Balance_of tests
    testGroup "Balance_of entrypoint" $
      [ testScenario "returns results in the expected order and without de-duplication" $
          scenario do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItems =
                    [ BalanceRequestItem {briOwner = toAddress wallet1, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = toAddress wallet4, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = toAddress wallet3, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = toAddress wallet5, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = toAddress wallet2, briTokenId = theTokenId}
                    , BalanceRequestItem {briOwner = toAddress wallet3, briTokenId = theTokenId}
                    ]
                  balanceRequest = mkFA2View balanceRequestItems consumer
                  balanceExpected =
                    [ BalanceResponseItem {briRequest = BalanceRequestItem (toAddress wallet1) theTokenId, briBalance = 10}
                    , BalanceResponseItem {briRequest = BalanceRequestItem (toAddress wallet4) theTokenId, briBalance = 40}
                    , BalanceResponseItem {briRequest = BalanceRequestItem (toAddress wallet3) theTokenId, briBalance = 30}
                    , BalanceResponseItem {briRequest = BalanceRequestItem (toAddress wallet5) theTokenId, briBalance = 50}
                    , BalanceResponseItem {briRequest = BalanceRequestItem (toAddress wallet2) theTokenId, briBalance = 20}
                    , BalanceResponseItem {briRequest = BalanceRequestItem (toAddress wallet3) theTokenId, briBalance = 30}
                    ]

              transfer fa2contract $ calling (ep @"Balance_of") balanceRequest
              getStorage consumer @@== [balanceExpected]
      , testScenario "validates token id" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItem = BalanceRequestItem {briOwner = toAddress wallet1, briTokenId = oneTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer

              fa2TokenUndefined $ transfer fa2contract $ calling (ep @"Balance_of") balanceRequest
      , testScenario "returns zero if the account does not exist" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItem = BalanceRequestItem {briOwner = toAddress wallet1, briTokenId = theTokenId}
                  balanceRequest = mkFA2View [balanceRequestItem] consumer
                  balanceExpected =
                    [ BalanceResponseItem {briRequest = balanceRequestItem, briBalance = 0}
                    ]

              transfer fa2contract $ calling (ep @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]
      , testScenario "accepts an empty list" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let originationParams =
                  defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              consumer <- originate "consumer" [] (contractConsumer @[BalanceResponseItem])
              let balanceRequestItems = []
                  balanceRequest = mkFA2View balanceRequestItems consumer
                  balanceExpected = []

              transfer fa2contract $ calling (ep @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]
      ]
  , -- These tests require permission descriptor to be configured that allows for operator transfer.
    -- We have such a configuration set by default in$  defaultOriginationParams.
    let
      checkForOperator
        :: MonadCleveland caps m => ContractHandle Parameter Storage ()
        -> ImplicitAddressWithAlias
        -> ImplicitAddressWithAlias
        -> Bool
        -> m ()
      checkForOperator stablecoinContract owner operator expectation = do
          storage <- getStorage stablecoinContract
          val <- getBigMapValueMaybe (sOperatorsRPC storage) (toAddress owner, toAddress operator)
          let found = isJust val
          if found /= expectation
            then
              failure $
                "Unexpected operator status. Expected: "
                  <> pretty expectation
                  <> " Found: "
                  <> pretty found
            else pass
     in testGroup "Configure operators entrypoint" $
          [ testScenario "add operator call adds operator as expected" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress wallet2, opTokenId = theTokenId}

                    let addOperatorParam = AddOperator operatorParam
                    transfer fa2contract $ calling (ep @"Update_operators") [addOperatorParam]

                    checkForOperator fa2contract wallet1 wallet2 True
          , testScenario "remove operator call removes operator as expected" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = theTokenId}

                    let removeOperatorParam = RemoveOperator operatorParam
                    transfer fa2contract $ calling (ep @"Update_operators") [removeOperatorParam]

                    checkForOperator fa2contract wallet1 commonOperator False
          , testScenario "retains the last operation in case of conflicting operations - Expect removal" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress wallet2, opTokenId = theTokenId}

                    transfer fa2contract $ calling (ep @"Update_operators") [AddOperator operatorParam, RemoveOperator operatorParam]

                    checkForOperator fa2contract wallet1 wallet2 False
          , testScenario "retains the last operation in case of conflicting operations - Expect addition" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress wallet2, opTokenId = theTokenId}

                    transfer fa2contract $ calling (ep @"Update_operators") [RemoveOperator operatorParam, AddOperator operatorParam]

                    checkForOperator fa2contract wallet1 wallet2 True
          , testScenario "add operator call validates token id" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = oneTokenId}

                    fa2TokenUndefined $
                      transfer fa2contract $
                        calling (ep @"Update_operators") [AddOperator operatorParam]
          , testScenario "remove operator call validates token id" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = oneTokenId}

                    fa2TokenUndefined $
                      transfer fa2contract $
                        calling (ep @"Update_operators") [RemoveOperator operatorParam]
          , testScenario "add operator call can be paused" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let originationParams =
                      addAccount (wallet1, ([], 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender testPauser $ transfer fa2contract $ calling (ep @"Pause") ()

                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = theTokenId}

                    mgmContractPaused $
                      transfer fa2contract $
                        calling (ep @"Update_operators") [AddOperator operatorParam]
          , testScenario "remove operator call can be paused" $
              scenario do
                TestAddresses{..} <- setupAddresses
                let commonOperators = [commonOperator]
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                fa2contract <- originateStablecoin originationParams
                do
                  withSender testPauser $ transfer fa2contract $ calling (ep @"Pause") ()

                  withSender wallet1 $ do
                    let operatorParam =
                          OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = theTokenId}

                    mgmContractPaused $
                      transfer fa2contract $
                        calling (ep @"Update_operators") [RemoveOperator operatorParam]
          ]
  , -- Check whether "update operator", "remove operator" operations are executed only by contract owner.
    testGroup "Configure operators entrypoint" $
      [ testScenario "denies addOperator call for non-owners" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- originateStablecoin originationParams
            do
              withSender wallet2 $ do
                let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress wallet2, opTokenId = theTokenId}

                let addOperatorParam = AddOperator operatorParam
                fa2NotOwner $ transfer stablecoinContract $ calling (ep @"Update_operators") [addOperatorParam]
      , testScenario "denies removeOperator call for non-owners" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- originateStablecoin originationParams
            do
              withSender wallet2 $ do
                let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = theTokenId}

                let removeOperatorParam = RemoveOperator operatorParam
                fa2NotOwner $ transfer stablecoinContract $ calling (ep @"Update_operators") [removeOperatorParam]
      , testScenario "denies addOperator for operators" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- originateStablecoin originationParams
            do
              withSender commonOperator $ do
                let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress wallet2, opTokenId = theTokenId}

                let addOperatorParam = AddOperator operatorParam
                fa2NotOwner $ transfer stablecoinContract $ calling (ep @"Update_operators") [addOperatorParam]
      , testScenario "denies removeOperator for operators" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            stablecoinContract <- originateStablecoin originationParams
            do
              withSender commonOperator $ do
                let operatorParam = OperatorParam {opOwner = toAddress wallet1, opOperator = toAddress commonOperator, opTokenId = theTokenId}

                let removeOperatorParam = RemoveOperator operatorParam
                fa2NotOwner $ transfer stablecoinContract $ calling (ep @"Update_operators") [removeOperatorParam]
      ]
  , ---- Owner hook tests
    --
    ---- The contract implements the default (as per FA2) permission descriptor and thus does not
    ---- call the hooks in either sender or receiver addresses.
    testGroup "Owner hook behavior on transfer" $
      [ testScenario "does not call sender's transfer hook on transfer" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            senderWithHook <- originate "Sender hook consumer" [] $ contractConsumer @FA2OwnerHook
            let originationParams =
                  addAccount (toContractAddress senderWithHook, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress senderWithHook)
                      (#to_ :! toAddress wallet2)
                      (#amount :! 10)

              withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers

              getStorage senderWithHook @@== []
      , testScenario "does not call receiver's transfer hook on transfer" $
          scenario do
            TestAddresses{..} <- setupAddresses
            let commonOperators = [commonOperator]
            receiverWithHook <- originate "Receiver hook consumer" [] (contractConsumer @FA2OwnerHook)
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
            fa2contract <- originateStablecoin originationParams
            do
              let transfers =
                    constructSingleTransfer
                      (#from_ :! toAddress wallet1)
                      (#to_ :! toAddress receiverWithHook)
                      (#amount :! 10)

              withSender commonOperator $ transfer fa2contract $ calling (ep @"Transfer") transfers

              getStorage receiverWithHook @@== []
      ]
  , testGroup
      "Off-chain storage view getBalance"
      [ testScenarioOnEmulator "computes the balance of the address correctly" $
          scenarioEmulated do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              callOffChainView @Natural fa2contract "get_balance" (ViewParam (toAddress wallet1, 0 :: Natural)) @@== 10
      ]
  , testGroup "Off-chain storage view getTotalSupply" $
      [ testScenarioOnEmulator "Returns the total supply value correctly" $
          scenarioEmulated do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              callOffChainView @Natural fa2contract "total_supply" (ViewParam (0 :: Natural)) @@== 35
      ]
  , testGroup "Off-chain storage view getAllTokens" $
      [ testScenarioOnEmulator "Returns only token id of Zero" $
          scenarioEmulated do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              callOffChainView @[Natural] fa2contract "all_tokens" NoParam @@== [0]
      ]
  , testGroup "Off-chain storage view isOperator" $
      [ testScenarioOnEmulator "Returns the status of operator" $
          scenarioEmulated do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              callOffChainView @Bool fa2contract "is_operator" (ViewParam (toAddress wallet1, (toAddress commonOperator, 0 :: Natural))) @@== True
      ]
  , testGroup "Off-chain storage view GetTokenMetadata" $
      [ testScenarioOnEmulator "Returns the metadata of token" $
          scenarioEmulated do
            TestAddresses{..} <- setupAddresses
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
            fa2contract <- originateStablecoin originationParams
            do
              Showing <$> (callOffChainView @(Natural, TokenMetadata) fa2contract "token_metadata" (ViewParam (0 :: Natural)))
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
