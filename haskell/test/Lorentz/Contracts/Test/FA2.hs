-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | Tests for FA2 interface.
-- https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md

module Lorentz.Contracts.Test.FA2
  ( OriginationParams (..)
  , addAccount
  , defaultOriginationParams
  , fa2Spec
  , fa2NotOperator
  , fa2NotOwner
  ) where

import Data.Map as M (fromList, lookup)
import Test.Hspec (Spec, describe, it)

import Lorentz.Contracts.Spec.FA2Interface as FA2 hiding (ParameterC)
import Lorentz.Contracts.Stablecoin (ParameterC, sLedger, sOperators)
import Lorentz.Contracts.Test.Common
import Lorentz.Test
import Lorentz.Value
import Michelson.Runtime (ExecutorError)
import Morley.Metadata (ViewParam(..))
import Util.Named

-- | Test suite for an FA2-specific entrypoints for stablecoin smart-contract which:
--
-- 1. Supports a single token type.
-- 2. Does not have an external permission checking transfer hook.
fa2Spec
  :: forall param. ParameterC param
  => OriginationFn param
  -> Spec
fa2Spec fa2Originate = do
  describe "Operator Transfer" $ do
  -- Transfer tests or tests for core transfer behavior, as per FA2
    it "is allowed to transfer and \
       \executes transfers in the given order" $ integrationalTestExpectation $ do
      -- Tests transactions are applied in order
      -- Update balances exactly
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, (commonOperators, 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructTransfers
              [ (#from_ .! wallet1, [(#to_ .! wallet2, #amount .! 10)])
              , (#from_ .! wallet2, [(#to_ .! wallet3, #amount .! 10)])
              , (#from_ .! wallet3, [(#to_ .! wallet4, #amount .! 10)])
              , (#from_ .! wallet4, [(#to_ .! wallet5, #amount .! 10)])
              ]

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
          let
            balanceRequestItems =
              [ BalanceRequestItem { briOwner = wallet1, briTokenId = theTokenId }
              , BalanceRequestItem { briOwner = wallet2, briTokenId = theTokenId }
              , BalanceRequestItem { briOwner = wallet3, briTokenId = theTokenId }
              , BalanceRequestItem { briOwner = wallet4, briTokenId = theTokenId }
              , BalanceRequestItem { briOwner = wallet5, briTokenId = theTokenId }
              ]
            balanceRequest = mkFA2View balanceRequestItems consumer
            balanceExpected =
              [ BalanceResponseItem
                  { briRequest = BalanceRequestItem { briOwner = wallet1, briTokenId = theTokenId }
                  , briBalance = 0
                  }
              , BalanceResponseItem
                  { briRequest = BalanceRequestItem { briOwner = wallet2, briTokenId = theTokenId }
                  , briBalance = 0
                  }
              , BalanceResponseItem
                  { briRequest = BalanceRequestItem { briOwner = wallet3, briTokenId = theTokenId }
                  , briBalance = 0
                  }
              , BalanceResponseItem
                  { briRequest = BalanceRequestItem { briOwner = wallet4, briTokenId = theTokenId }
                  , briBalance = 0
                  }
              , BalanceResponseItem
                  { briRequest = BalanceRequestItem { briOwner = wallet5, briTokenId = theTokenId }
                  , briBalance = 10
                  }
              ]

          lCallEP fa2contract (Call @"Balance_of") balanceRequest
          lExpectViewConsumerStorage consumer [balanceExpected]

    it "allows zero transfer from non-existent accounts" $ integrationalTestExpectation $ do
        let originationParams = addOperator (wallet1, commonOperator) $ defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet1)
              (#to_ .! wallet2)
              (#amount .! 0)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers


    it "aborts if there is a failure (due to low balance)" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, (commonOperators, 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructTransfersFromSender (#from_ .! wallet1)
              [ (#to_ .! wallet2, #amount .! 10)
              , (#to_ .! wallet3, #amount .! 10)
              , (#to_ .! wallet3, #amount .! 10)
              , (#to_ .! wallet5, #amount .! 10)
              ]

          err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          fa2InsufficientBalance err

    it "aborts if there is a failure (due to non existent source account)" $ integrationalTestExpectation $ do
        let
          originationParams =
              addOperator (wallet2, commonOperator)
            $ addAccount (wallet1, (commonOperators, 10))
            $ defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet2)
              (#to_ .! wallet1)
              (#amount .! 1)

          err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          fa2InsufficientBalance err

    it "aborts if there is a failure (due to bad operator)" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, ([], 10))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers1 = constructSingleTransfer
                          (#from_ .! wallet1) (#to_ .! wallet2) (#amount .! 1)

            transfers2 = constructTransfersFromSender (#from_ .! wallet3)
              [ (#to_ .! wallet1, #amount .! 1)
              , (#to_ .! wallet4, #amount .! 1)
              , (#to_ .! wallet5, #amount .! 1)
              ]
            transfers = transfers1 <> transfers2

          err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          fa2NotOperator err

    it "aborts if there is a failure (due to bad operator for zero transfer)" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, ([], 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers1 = constructSingleTransfer
              (#from_ .! wallet1) (#to_ .! wallet2) (#amount .! 1)

            transfers2 = constructSingleTransfer
              (#from_ .! wallet3) (#to_ .! wallet2) (#amount .! 0)

            transfers = transfers1 <> transfers2

          err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          fa2NotOperator err

    it "accepts an empty list of transfers" $ do
      integrationalTestExpectation $ do
        let
          originationParams =
              addAccount (wallet1, (commonOperators, 10))
            $ defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = [ TransferItem wallet1 [] ]

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

    it "validates token id" $
      integrationalTestExpectation $ do
        let originationParams =
              addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers =
              [ TransferItem
                  { tiFrom = wallet1
                  , tiTxs =
                    [ TransferDestination wallet2 theTokenId 5
                    , TransferDestination wallet3 oneTokenId 1
                    , TransferDestination wallet4 theTokenId 4
                    ]
                  }
              ]

          err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
          fa2TokenUndefined err

    it "cannot transfer foreign money" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet2, ([], 10)) $
            addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet2)
            (#to_ .! wallet1)
            (#amount .! 1)

        err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
        fa2NotOperator err

    it "will create target account if it does not already exist" $ integrationalTestExpectation $ do
      let originationParams =
            addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 5)

        withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"

        let
          balanceRequestItem = BalanceRequestItem { briOwner = wallet2, briTokenId = theTokenId }
          balanceRequest = mkFA2View [balanceRequestItem] consumer
          balanceExpected = [ BalanceResponseItem { briRequest = balanceRequestItem, briBalance = 5 } ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest
        lExpectViewConsumerStorage consumer [balanceExpected]

    it "removes zero balance accounts after transfer" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ addAccount (wallet3, (commonOperators, 0))
          $ defaultOriginationParams

      withOriginated fa2Originate originationParams $ \stablecoinContract -> do
        let
          transfer1 = constructTransfersFromSender (#from_ .! wallet1)
            [ (#to_ .! wallet2, #amount .! 10)
            ]

        withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfer1

        lExpectStorage stablecoinContract $ \storage ->
          unless (M.lookup wallet1 (unBigMap $ sLedger storage) == Nothing) $
            Left $ CustomTestError "Zero balance account was not removed"

  describe "Self transfer" $ do

    it "Cannot transfer foreign money" $ integrationalTestExpectation $ do
      let originationParams = (addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams)
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 1)

        err <- expectError $ withSender wallet2 $ lCallEP fa2contract (Call @"Transfer") transfers
        fa2NotOperator err

    it "is permitted" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
      consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"

      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 5)

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers
        let
          balanceRequestItem = BalanceRequestItem { briOwner = wallet2, briTokenId = theTokenId }
          balanceRequest = mkFA2View [balanceRequestItem] consumer
          balanceExpected = [ BalanceResponseItem { briRequest = balanceRequestItem, briBalance = 5 } ]
        lCallEP fa2contract (Call @"Balance_of") balanceRequest
        lExpectViewConsumerStorage consumer [balanceExpected]

    it "validates token id" $
      integrationalTestExpectation $ do
        let originationParams =
              addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers =
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

          err <- expectError $ withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers
          fa2TokenUndefined err

    it "allows zero transfer from non-existent accounts" $ integrationalTestExpectation $ do
      -- Tests transactions are applied in order
      -- Update balances exactly
        withOriginated fa2Originate defaultOriginationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet1)
              (#to_ .! wallet2)
              (#amount .! 0)

          withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers

    it "aborts if there is a failure (due to low source balance)" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
        -- Tests transactions are applied atomically
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructTransfersFromSender (#from_ .! wallet1)
              [ (#to_ .! wallet2, #amount .! 4)
              , (#to_ .! wallet3, #amount .! 4)
              , (#to_ .! wallet4, #amount .! 4) -- Should fail
              , (#to_ .! wallet5, #amount .! 2)
              ]

          err <- expectError $ withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers

          fa2InsufficientBalance err

    it "aborts if there is a failure (due to non existent source for non-zero transfer)" $
      integrationalTestExpectation $ do
        let originationParams = addOperator (wallet3, commonOperator)
              $ addAccount (wallet1, ([], 10)) defaultOriginationParams
        -- Tests transactions are applied atomically
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet3)
              (#to_ .! wallet1)
              (#amount .! 2)

          err <- expectError $ withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          fa2InsufficientBalance err

    it "aborts if there is a failure (due to bad token id)" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
      -- Tests transactions are applied atomically
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers =
            [ TransferItem
                { tiFrom = wallet1
                , tiTxs =
                  [ TransferDestination { tdTo = wallet2, tdTokenId = theTokenId, tdAmount = 1 }
                  , TransferDestination { tdTo = wallet3, tdTokenId = theTokenId, tdAmount = 1 }
                  , TransferDestination { tdTo = wallet4, tdTokenId = oneTokenId, tdAmount = 1 } -- Should fail
                  , TransferDestination { tdTo = wallet5, tdTokenId = theTokenId, tdAmount = 1 }
                  ]
                }
            ]

        err <- expectError $ withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers

        fa2TokenUndefined err

    it "will create target account if it does not already exist" $ integrationalTestExpectation $ do
      let originationParams =
            addAccount (wallet1, ([], 10)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 5)

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers
        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"

        let
          balanceRequestItem = BalanceRequestItem { briOwner = wallet2, briTokenId = theTokenId }
          balanceRequest = mkFA2View [balanceRequestItem] consumer
          balanceExpected = [ BalanceResponseItem { briRequest = balanceRequestItem, briBalance = 5 } ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest
        lExpectViewConsumerStorage consumer [balanceExpected]

  -- Balance_of tests
  describe "Balance_of entrypoint" $ do
    it "returns results in the expected order and without de-duplication" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) $
            addAccount (wallet2, (commonOperators, 20)) $
            addAccount (wallet3, (commonOperators, 30)) $
            addAccount (wallet4, (commonOperators, 40)) $
            addAccount (wallet5, (commonOperators, 50)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ BalanceRequestItem { briOwner = wallet1, briTokenId = theTokenId }
            , BalanceRequestItem { briOwner = wallet4, briTokenId = theTokenId }
            , BalanceRequestItem { briOwner = wallet3, briTokenId = theTokenId }
            , BalanceRequestItem { briOwner = wallet5, briTokenId = theTokenId }
            , BalanceRequestItem { briOwner = wallet2, briTokenId = theTokenId }
            , BalanceRequestItem { briOwner = wallet3, briTokenId = theTokenId }
            ]
          balanceRequest = mkFA2View balanceRequestItems consumer
          balanceExpected =
            [ BalanceResponseItem { briRequest = BalanceRequestItem wallet1 theTokenId, briBalance = 10 }
            , BalanceResponseItem { briRequest = BalanceRequestItem wallet4 theTokenId, briBalance = 40 }
            , BalanceResponseItem { briRequest = BalanceRequestItem wallet3 theTokenId, briBalance = 30 }
            , BalanceResponseItem { briRequest = BalanceRequestItem wallet5 theTokenId, briBalance = 50 }
            , BalanceResponseItem { briRequest = BalanceRequestItem wallet2 theTokenId, briBalance = 20 }
            , BalanceResponseItem { briRequest = BalanceRequestItem wallet3 theTokenId, briBalance = 30 }
            ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest
        lExpectViewConsumerStorage consumer [balanceExpected]

    it "validates token id" $ integrationalTestExpectation $ do

      let originationParams =
            addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItem = BalanceRequestItem { briOwner = wallet1, briTokenId = oneTokenId }
          balanceRequest = mkFA2View [balanceRequestItem] consumer

        err <- expectError $ lCallEP fa2contract (Call @"Balance_of") balanceRequest
        fa2TokenUndefined err

    it "returns zero if the account does not exist" $ integrationalTestExpectation $ do

      let originationParams = defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItem = BalanceRequestItem { briOwner = wallet1, briTokenId = theTokenId }
          balanceRequest = mkFA2View [balanceRequestItem] consumer
          balanceExpected =
            [ BalanceResponseItem { briRequest = balanceRequestItem, briBalance = 0 }
            ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        lExpectViewConsumerStorage consumer [balanceExpected]

    it "accepts an empty list" $ integrationalTestExpectation $ do

      let originationParams = defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems = []
          balanceRequest = mkFA2View balanceRequestItems consumer
          balanceExpected = []

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        lExpectViewConsumerStorage consumer [balanceExpected]

  -- These tests require permission descriptor to be configured that allows for operator transfer.
  -- We have such a configuration set by default in defaultOriginationParams.
  let
    checkForOperator stablecoinContract owner operator expectation =
      lExpectStorage stablecoinContract $ \storage -> do
        let found = isJust $ M.lookup (owner, operator) (unBigMap $ sOperators storage)
        if found /= expectation
            then Left $ CustomTestError $ "Unexpected operator status. Expected: "
              <> show expectation <> " Found: " <> show found
            else Right ()

  describe "Configure operators entrypoint's add operator call" $ do

    it "adds operator as expected" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          withSender wallet1 $ do
            let operatorParam = OperatorParam { opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId }

            let addOperatorParam = AddOperator  operatorParam
            lCallEP fa2contract (Call @"Update_operators") [addOperatorParam]

            checkForOperator fa2contract wallet1 wallet2 True


  describe "Configure operators entrypoint's remove operator call" $ do
    it "removes operator as expected" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          withSender wallet1 $ do
            let operatorParam = OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId }

            let removeOperatorParam = RemoveOperator operatorParam
            lCallEP fa2contract (Call @"Update_operators") [removeOperatorParam]

            checkForOperator fa2contract wallet1 commonOperator False

  describe "Configure operators entrypoint" $ do
    it "retains the last operation in case of conflicting operations - Expect removal" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          withSender wallet1 $ do
            let operatorParam = OperatorParam { opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId }

            lCallEP fa2contract (Call @"Update_operators") [AddOperator  operatorParam, RemoveOperator operatorParam]

            checkForOperator fa2contract wallet1 wallet2 False

    it "retains the last operation in case of conflicting operations - Expect addition" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          withSender wallet1 $ do
            let operatorParam = OperatorParam { opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId }

            lCallEP fa2contract (Call @"Update_operators") [RemoveOperator operatorParam, AddOperator  operatorParam]

            checkForOperator fa2contract wallet1 wallet2 True


    it "add operator call validates token id" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          withSender wallet1 $ do
            let operatorParam =
                  OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = oneTokenId }

            err <- expectError $  lCallEP fa2contract (Call @"Update_operators")
                    [AddOperator  operatorParam]
            fa2TokenUndefined err

    it "remove operator call validates token id" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          withSender wallet1 $ do
            let operatorParam =
                  OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = oneTokenId }

            err <- expectError $ lCallEP fa2contract (Call @"Update_operators")
                    [RemoveOperator operatorParam]
            fa2TokenUndefined err

    it "add operator call can be paused" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          withSender testPauser $ lCallEP fa2contract (Call @"Pause") ()

          withSender wallet1 $ do
            let operatorParam =
                  OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId }

            err <- expectError $  lCallEP fa2contract (Call @"Update_operators")
                    [AddOperator  operatorParam]
            mgmContractPaused err

    it "remove operator call can be paused" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          withSender testPauser $ lCallEP fa2contract (Call @"Pause") ()

          withSender wallet1 $ do
            let operatorParam =
                  OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId }

            err <- expectError $ lCallEP fa2contract (Call @"Update_operators")
                    [RemoveOperator operatorParam]
            mgmContractPaused err

  -- Check whether "update operator", "remove operator" operations are executed only by contract owner.
  describe "Configure operators entrypoint" $
    it "denies addOperator call for non-owners" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
      withOriginated fa2Originate originationParams $ \stablecoinContract -> do

        withSender wallet2 $ do
          let operatorParam = OperatorParam { opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId }

          let addOperatorParam = AddOperator  operatorParam
          err <- expectError $ lCallEP stablecoinContract (Call @"Update_operators") [addOperatorParam]
          fa2NotOwner err

  it "denies removeOperator call for non-owners" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
    withOriginated fa2Originate originationParams $ \stablecoinContract -> do

      withSender wallet2 $ do
        let operatorParam = OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId }

        let removeOperatorParam = RemoveOperator operatorParam
        err <- expectError $ lCallEP stablecoinContract (Call @"Update_operators") [removeOperatorParam]
        fa2NotOwner err

  it "denies addOperator for operators" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
    withOriginated fa2Originate originationParams $ \stablecoinContract -> do

      withSender commonOperator $ do
        let operatorParam = OperatorParam { opOwner = wallet1, opOperator = wallet2, opTokenId = theTokenId }

        let addOperatorParam = AddOperator  operatorParam
        err <- expectError $ lCallEP stablecoinContract (Call @"Update_operators") [addOperatorParam]

        fa2NotOwner err

  it "denies removeOperator for operators" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
    withOriginated fa2Originate originationParams $ \stablecoinContract -> do

      withSender commonOperator $ do
        let operatorParam = OperatorParam { opOwner = wallet1, opOperator = commonOperator, opTokenId = theTokenId }

        let removeOperatorParam = RemoveOperator operatorParam
        err <- expectError $ lCallEP stablecoinContract (Call @"Update_operators") [removeOperatorParam]
        fa2NotOwner err

  ---- Owner hook tests
  --
  ---- The contract implements the default (as per FA2) permission descriptor and thus does not
  ---- call the hooks in either sender or receiver addresses.
  describe "Owner hook behavior on transfer" $ do
    it "does not call sender's transfer hook on transfer" $ integrationalTestExpectation $ do
        senderWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Sender hook consumer"
        let originationParams =
              addAccount (unTAddress senderWithHook, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! unTAddress senderWithHook)
              (#to_ .! wallet2)
              (#amount .! 10)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          lExpectStorageConst @[FA2OwnerHook] senderWithHook []

    it "does not call receiver's transfer hook on transfer" $
      integrationalTestExpectation $ do
        receiverWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Receiver hook consumer"
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet1)
              (#to_ .! unTAddress receiverWithHook)
              (#amount .! 10)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          lExpectStorageConst @[FA2OwnerHook] receiverWithHook []

  describe "Off-chain storage view getBalance" $ do
    it "computes the balance of the address correctly" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, (commonOperators, 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract ->
            checkView fa2contract "GetBalance" (ViewParam (0 :: Natural, wallet1)) (10 :: Natural)

  describe "Off-chain storage view getTotalSupply" $ do
    it "Returns the total supply value correctly" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 25))
              $ addAccount (wallet3, (commonOperators, 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract ->
            checkView fa2contract "GetTotalSupply" (ViewParam (0 :: Natural)) (35 :: Natural)

  describe "Off-chain storage view getAllTokens" $ do
    it "Returns only token id of Zero" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 25))
              $ addAccount (wallet3, (commonOperators, 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract ->
            checkView fa2contract "GetAllTokens" NoParam [0 :: Natural]

  describe "Off-chain storage view isOperator" $ do
    it "Returns the status of operator" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, ([], 25))
              $ addAccount (wallet3, ([], 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract ->
            checkView fa2contract "IsOperator" (ViewParam (0 :: Natural, (wallet1, commonOperator))) True

  describe "Off-chain storage view GetTokenMetadata" $ do
    it "Returns the metadata of token" $ integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, ([], 25))
              $ addAccount (wallet3, ([], 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract ->
            checkView fa2contract "GetTokenMetadata" (ViewParam (0 :: Natural))
              (0 :: Natural,
                M.fromList [ ([mt|decimals|], "3" :: ByteString)
                             , ([mt|name|],"TEST"), ([mt|symbol|], "TEST")])

fa2TokenUndefined :: ExecutorError -> IntegrationalScenario
fa2TokenUndefined = lExpectFailWith (== [mt|FA2_TOKEN_UNDEFINED|])

fa2InsufficientBalance :: ExecutorError -> IntegrationalScenario
fa2InsufficientBalance = lExpectFailWith (== [mt|FA2_INSUFFICIENT_BALANCE|])

fa2NotOwner :: ExecutorError -> IntegrationalScenario
fa2NotOwner = lExpectFailWith (== [mt|NOT_TOKEN_OWNER|])

fa2NotOperator :: ExecutorError -> IntegrationalScenario
fa2NotOperator = lExpectFailWith (== [mt|FA2_NOT_OPERATOR|])
