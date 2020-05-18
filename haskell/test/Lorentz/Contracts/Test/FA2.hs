-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

-- | Tests for FA2 interface.
-- https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md

module Lorentz.Contracts.Test.FA2
  ( OriginationParams (..)
  , addAccount
  , defaultOriginationParams
  , defaultPermissionDescriptor
  , fa2Spec
  ) where

import Test.Hspec (Spec, describe, it)

import Lorentz (mkView)
import qualified Lorentz as L
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.Common
import Lorentz.Test
import Lorentz.Value
import Util.Named

-- | Test suite for an FA2-specific entrypoints for stablecoin smart-contract which:
--
-- 1. Supports a single token type.
-- 2. Does not have an external permission checking transfer hook.
fa2Spec
  :: forall param. FA2ParameterC param
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
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
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
              [ (#owner .! wallet1, #token_id .! 0)
              , (#owner .! wallet2, #token_id .! 0)
              , (#owner .! wallet3, #token_id .! 0)
              , (#owner .! wallet4, #token_id .! 0)
              , (#owner .! wallet5, #token_id .! 0)
              ]
            balanceRequest = mkView (#requests .! balanceRequestItems) consumer
            balanceExpected =
              [ (#request .! (#owner .! wallet1, #token_id .! 0), #balance .! 0)
              , (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 0)
              , (#request .! (#owner .! wallet3, #token_id .! 0), #balance .! 0)
              , (#request .! (#owner .! wallet4, #token_id .! 0), #balance .! 0)
              , (#request .! (#owner .! wallet5, #token_id .! 0), #balance .! 10)
              ]

          lCallEP fa2contract (Call @"Balance_of") balanceRequest

          validate . Right $
            lExpectViewConsumerStorage consumer [balanceExpected]

    it "aborts if there is a failure (due to low balance)" $ integrationalTestExpectation $ do
      -- Tests transactions are applied in order
      -- Update balances exactly
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, (commonOperators, 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructTransfersFromSender (#from_ .! wallet1)
              [ (#to_ .! wallet2, #amount .! 10)
              , (#to_ .! wallet3, #amount .! 10)
              , (#to_ .! wallet3, #amount .! 10) -- should fail
              , (#to_ .! wallet5, #amount .! 10)
              ]

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "aborts if there is a failure (due to non existent source account)" $ integrationalTestExpectation $ do
      -- Tests transactions are applied in order
      -- Update balances exactly
        let
          originationParams =
              addAccount (wallet1, (commonOperators, 10))
            $ defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet2)
              (#to_ .! wallet1)
              (#amount .! 1)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "aborts if there is a failure (due to bad owner)" $ integrationalTestExpectation $ do
      -- Tests transactions are applied in order
      -- Update balances exactly
        let originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addAccount (wallet2, (commonOperators, 0))
              $ addAccount (wallet3, ([], 0))
              $ addAccount (wallet4, (commonOperators, 0)) defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructTransfersFromSender (#from_ .! wallet1)
              [ (#to_ .! wallet2, #amount .! 10)
              , (#to_ .! wallet3, #amount .! 10)
              , (#to_ .! wallet4, #amount .! 10) -- should fail
              , (#to_ .! wallet5, #amount .! 10)
              ]

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "accepts an empty list of transfers" $ do
      integrationalTestExpectation $ do
        let
          originationParams =
              addAccount (wallet1, (commonOperators, 10))
            $ defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = [(#from_ .! wallet1, #txs .! [])]

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
          validate (Right expectAnySuccess)

    it "is denied if operator transfer is denied in permissions descriptior" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) $
              defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet1)
              (#to_ .! wallet2)
              (#amount .! 1)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "validates token id" $
      integrationalTestExpectation $ do
        let originationParams =
              addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers =
              [ ( #from_ .! wallet1
              , (#txs .!
                  [ (#to_ .! wallet2, (#token_id .! 0, #amount .! 5))
                  , (#to_ .! wallet3, (#token_id .! 1, #amount .! 1))
                  , (#to_ .! wallet4, (#token_id .! 0, #amount .! 4))
                  ]))
              ]

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "cannot transfer foreign money" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet2, ([], 10)) $
            addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet2)
            (#to_ .! wallet1)
            (#amount .! 1)

        withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

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
          balanceRequestItems =
            [ (#owner .! wallet2, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 5)
            ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

  describe "Self transfer" $ do

    it "Cannot transfer foreign money" $ integrationalTestExpectation $ do
      let originationParams = (addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams)
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 1)

        withSender wallet2 $ lCallEP fa2contract (Call @"Transfer") transfers
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "is permitted if self transfer is permitted in permissions descriptior" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) $
            defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorSelfTransferAllowed }
      consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"

      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 5)

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers
        let
          balanceRequestItems =
            [ (#owner .! wallet2, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 5)
            ]
        lCallEP fa2contract (Call @"Balance_of") balanceRequest
        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

    it "is denied if self transfer is forbidden in permissions descriptior" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) $
            defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorSelfTransferDenied }
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet1)
            (#to_ .! wallet2)
            (#amount .! 1)

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "validates token id" $
      integrationalTestExpectation $ do
        let originationParams =
              addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorSelfTransferAllowed }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers =
              [ (#from_ .! wallet1
              , (#txs .! [(#to_ .! wallet2, (#token_id .! 1, #amount .! 10))]))
              ]

          withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers
          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "aborts if there is a failure (due to low source balance)" $ integrationalTestExpectation $ do
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

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers

        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "aborts if there is a failure (due to non existent source)" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
      -- Tests transactions are applied atomically
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers = constructSingleTransfer
            (#from_ .! wallet3)
            (#to_ .! wallet1)
            (#amount .! 2)

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers

        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "aborts if there is a failure (due to bad token id)" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, ([], 10)) defaultOriginationParams
      -- Tests transactions are applied atomically
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        let
          transfers =
            [( #from_ .! wallet1
             , #txs .!
               [ (#to_ .! wallet2, (#token_id .! 0, #amount .! 1))
               , (#to_ .! wallet3, (#token_id .! 0, #amount .! 1))
               , (#to_ .! wallet4, (#token_id .! 1, #amount .! 1)) -- Should fail
               , (#to_ .! wallet5, (#token_id .! 0, #amount .! 1))
               ]
             )]

        withSender wallet1 $ lCallEP fa2contract (Call @"Transfer") transfers

        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

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
          balanceRequestItems =
            [ (#owner .! wallet2, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 5)
            ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        validate . Right $
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
            [ (#owner .! wallet1, #token_id .! 0)
            , (#owner .! wallet4, #token_id .! 0)
            , (#owner .! wallet3, #token_id .! 0)
            , (#owner .! wallet5, #token_id .! 0)
            , (#owner .! wallet2, #token_id .! 0)
            , (#owner .! wallet3, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet1, #token_id .! 0), #balance .! 10)
            , (#request .! (#owner .! wallet4, #token_id .! 0), #balance .! 40)
            , (#request .! (#owner .! wallet3, #token_id .! 0), #balance .! 30)
            , (#request .! (#owner .! wallet5, #token_id .! 0), #balance .! 50)
            , (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 20)
            , (#request .! (#owner .! wallet3, #token_id .! 0), #balance .! 30)
            ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

    it "validates token id" $ integrationalTestExpectation $ do

      let originationParams =
            addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ (#owner .! wallet1, #token_id .! 1)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer

        lCallEP fa2contract (Call @"Balance_of") balanceRequest
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "returns zero if the account does not exist" $ integrationalTestExpectation $ do

      let originationParams = defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ (#owner .! wallet1, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet1, #token_id .! 0), #balance .! 0)
            ]

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

    it "accepts an empty list" $ integrationalTestExpectation $ do

      let originationParams = defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems = []
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected = []

        lCallEP fa2contract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

  describe "Total_supply entrypoint" $ do
    it "returns results in the expected order" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) $
            addAccount (wallet2, (commonOperators, 20)) $
            addAccount (wallet3, (commonOperators, 30)) $
            addAccount (wallet4, (commonOperators, 40)) $
            addAccount (wallet5, (commonOperators, 50)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[TotalSupplyResponse] contractConsumer "consumer"
        let
          totalSupplyRequest = mkView (#token_ids .! [0]) consumer
          result =
            [ (#token_id .! 0, #total_supply .! 150) ]

        lCallEP fa2contract (Call @"Total_supply") totalSupplyRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [result]

    it "returns results in without de-duplication" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) $
            addAccount (wallet2, (commonOperators, 20)) $
            addAccount (wallet3, (commonOperators, 30)) $
            addAccount (wallet4, (commonOperators, 40)) $
            addAccount (wallet5, (commonOperators, 50)) $ defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[TotalSupplyResponse] contractConsumer "consumer"
        let
          totalSupplyRequest = mkView (#token_ids .! [0, 0]) consumer
          result =
            [ (#token_id .! 0, #total_supply .! 150)
            , (#token_id .! 0, #total_supply .! 150)
            ]

        lCallEP fa2contract (Call @"Total_supply") totalSupplyRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [result]

    it "validates token id" $ integrationalTestExpectation $ do
      let originationParams =
            addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams

      withOriginated fa2Originate originationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[TotalSupplyResponse] contractConsumer "consumer"
        let
          totalSupplyRequest = mkView (#token_ids .! [1]) consumer

        lCallEP fa2contract (Call @"Total_supply") totalSupplyRequest
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  -- Metadata tests
  describe "Metadata query entrypoint" $ do
    it "returns at least one items" $ integrationalTestExpectation $
      withOriginated fa2Originate defaultOriginationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[TokenMetadata] contractConsumer "consumer"
        let tokenMetadataQuery = mkView (#token_ids .! [0]) consumer
        lCallEP fa2contract (Call @"Token_metadata") tokenMetadataQuery

        validate . Right $
          lExpectConsumerStorage consumer
            (\(tds :: [[TokenMetadata]]) -> case tds of
                [[(L.arg #token_id -> tid, _)]] -> if tid == 0 then Right () else Left $ CustomValidationError "Token metadata query returned unexpected token id"
                _ -> Left $ CustomValidationError "Token metadata query returned list of unexpected length")

    it "returns items without de-duplication when queried with duplicates" $ integrationalTestExpectation $
      withOriginated fa2Originate defaultOriginationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[TokenMetadata] contractConsumer "consumer"
        let tokenMetadataQuery = mkView (#token_ids .! [0, 0]) consumer
        lCallEP fa2contract (Call @"Token_metadata") tokenMetadataQuery

        validate . Right $
          lExpectConsumerStorage consumer
            (\(tds :: [[TokenMetadata]]) -> case tds of
                [[md1@(L.arg #token_id -> tid, _), md2]] ->
                  if tid == 0 && md1 == md2
                    then Right ()
                    else Left $
                      CustomValidationError "Token metadata query returned unexpected token id"
                _ -> Left $
                  CustomValidationError "Token metadata query returned list of unexpected length")

    it "validates token id" $ integrationalTestExpectation $
      withOriginated fa2Originate defaultOriginationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @[TokenMetadata] contractConsumer "consumer"
        let tokenMetadataQuery = mkView (#token_ids .! [1]) consumer
        lCallEP fa2contract (Call @"Token_metadata") tokenMetadataQuery
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  -- Permission descriptor query
  describe "Permissions_descriptor entrypoint" $
    it "is available" $ integrationalTestExpectation $
      withOriginated fa2Originate defaultOriginationParams $ \fa2contract -> do
        consumer <- lOriginateEmpty @PermissionsDescriptor contractConsumer "consumer"
        let permissionsDescriptorQuery = toContractRef consumer
        lCallEP fa2contract (Call @"Permissions_descriptor") permissionsDescriptorQuery

        validate . Right $ expectAnySuccess

  -- These tests require permission descriptor to be configured that allows for operator transfer.
  -- We have such a configuration set by default in defaultOriginationParams.
  describe "Configure operators entrypoint's add operator call" $ do
    it "adds operator as expected" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
          withSender wallet1 $ do
            let operatorParam = (#owner .! wallet1, #operator .! wallet2)

            let addOperatorParam = Add_operator operatorParam
            lCallEP fa2contract (Call @"Update_operators") [addOperatorParam]

            let isOperatorQuery = mkView (#operator .! operatorParam) consumer
            lCallEP fa2contract (Call @"Is_operator") isOperatorQuery

            (validate . Right $
              lExpectViewConsumerStorage consumer
                [(#operator .! operatorParam, #is_operator .! True)])

  describe "Configure operators entrypoint's remove operator call" $ do
    it "removes operator as expected" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
          withSender wallet1 $ do
            let operatorParam =
                  (#owner .! wallet1, #operator .! commonOperator)

            let removeOperatorParam = Remove_operator operatorParam
            lCallEP fa2contract (Call @"Update_operators") [removeOperatorParam]

            let isOperatorQuery = mkView (#operator .! operatorParam) consumer
            lCallEP fa2contract (Call @"Is_operator") isOperatorQuery

            (validate . Right $
              lExpectViewConsumerStorage consumer
                [(#operator .! operatorParam, #is_operator .! False)])

  describe "Configure operators entrypoint" $ do
    it "retains the last operation in case of conflicting operations - Expect removal" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
          withSender wallet1 $ do
            let operatorParam =
                  (#owner .! wallet1, #operator .! wallet2)

            lCallEP fa2contract (Call @"Update_operators") [Add_operator operatorParam, Remove_operator operatorParam]

            let isOperatorQuery = mkView (#operator .! operatorParam) consumer
            lCallEP fa2contract (Call @"Is_operator") isOperatorQuery

            (validate . Right $
              lExpectViewConsumerStorage consumer
                [(#operator .! operatorParam, #is_operator .! False)])

    it "retains the last operation in case of conflicting operations - Expect addition" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
          withSender wallet1 $ do
            let operatorParam =
                  (#owner .! wallet1, #operator .! wallet2)

            lCallEP fa2contract (Call @"Update_operators") [Remove_operator operatorParam, Add_operator operatorParam]

            let isOperatorQuery = mkView (#operator .! operatorParam) consumer
            lCallEP fa2contract (Call @"Is_operator") isOperatorQuery

            (validate . Right $
              lExpectViewConsumerStorage consumer
                [(#operator .! operatorParam, #is_operator .! True)])

  -- Check whether "update operator", "remove operator" operations are executed only by contract owner.
  describe "Configure operators entrypoint" $
    it "denies addOperator call for non-owners" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
      withOriginated fa2Originate originationParams $ \fa2contract -> do

        withSender wallet2 $ do
          let operatorParam = (#owner .! wallet1, #operator .! wallet2)

          let addOperatorParam = Add_operator operatorParam
          lCallEP fa2contract (Call @"Update_operators") [addOperatorParam]

          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  it "denies removeOperator call for non-owners" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
    withOriginated fa2Originate originationParams $ \fa2contract -> do

      withSender wallet2 $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! commonOperator)

        let removeOperatorParam = Remove_operator operatorParam
        lCallEP fa2contract (Call @"Update_operators") [removeOperatorParam]
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  it "denies addOperator for operators" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
    withOriginated fa2Originate originationParams $ \fa2contract -> do

      withSender commonOperator $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! wallet2)

        let addOperatorParam = Add_operator operatorParam
        lCallEP fa2contract (Call @"Update_operators") [addOperatorParam]

        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  it "denies removeOperator for operators" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperators, 10)) defaultOriginationParams
    withOriginated fa2Originate originationParams $ \fa2contract -> do

      withSender commonOperator $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! commonOperator)

        let removeOperatorParam = Remove_operator operatorParam
        lCallEP fa2contract (Call @"Update_operators") [removeOperatorParam]
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  -- -- FA2 Mandates that the entrypoints to configure operators should fail if
  -- -- operator transfer is denied in permissions descriptor.
  it "errors on addOperator call if operator transfer is forbidden" $
    integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperators, 10))
            defaultOriginationParams
              { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }

      withOriginated fa2Originate originationParams $ \fa2contract -> do

        let operatorParam =
              (#owner .! wallet1, #operator .! wallet2)

        let addOperatorParam = Add_operator operatorParam
        lCallEP fa2contract (Call @"Update_operators") [addOperatorParam]
        validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  -- FA2 Mandates that the entrypoints to check operator status should fail if
  -- operator transfer is denied in permissions descriptor.
  it "errors on isOperator call if operator transfer is forbidden" $ integrationalTestExpectation $ do

    let originationParams = addAccount (wallet1, (commonOperators, 10))
          defaultOriginationParams
            { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }

    withOriginated fa2Originate originationParams $ \fa2contract -> do

      consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
      let operatorParam =
            (#owner .! wallet1, #operator .! wallet2)

      let isOperatorQuery = mkView (#operator .! operatorParam) consumer
      lCallEP fa2contract (Call @"Is_operator") isOperatorQuery
      validate $ Left (lExpectAnyMichelsonFailed fa2contract)

  ---- Owner hook test
  ----
  ---- Tests that validate whether senders owner hook is called on transfer
  ---- using default origination options where both sender/receiver hooks
  ---- are set to be optional.
  describe "Owner hook behavior on transfer" $ do
    it "calls sender's transfer hook on transfer" $ integrationalTestExpectation $ do
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

          let expectedTransferDesc =
               ( #from .! Just (unTAddress senderWithHook)
               , (#to .! Just wallet2, (#token_id .! 0, #amount .! 10)))

          let expectedHookContractState =
                Tokens_sent ( #fa2 .! unTAddress fa2contract
                            , (#batch .! [expectedTransferDesc], #operator .! commonOperator))

          validate . Right $
            lExpectViewConsumerStorage senderWithHook [expectedHookContractState]

    it "calls receiver's transfer hook on transfer" $
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

          let
            expectedTransferDesc =
               ( #from .! Just wallet1
               , (#to .! (Just $ unTAddress receiverWithHook), (#token_id .! 0, #amount .! 10)))

          let expectedHookContractState
                = Tokens_received
                    ( #fa2 .! unTAddress fa2contract
                    , (#batch .! [expectedTransferDesc], #operator .! commonOperator))

          validate . Right $
            lExpectViewConsumerStorage receiverWithHook [expectedHookContractState]

    -- Tests that the senders/receiver owner hook are NOT called on transfer
    it "does not call sender's transfer hook if `OwnerNoOp` is selected in permission descriptor" $
      integrationalTestExpectation $ do
        senderWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Sender hook consumer"
        let originationParams = addAccount (unTAddress senderWithHook, (commonOperators, 10)) $
              defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorNoOpSenderHook }

        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! unTAddress senderWithHook)
              (#to_ .! wallet2)
              (#amount .! 10)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          validate . Right $
            lExpectStorageConst senderWithHook ([] :: [FA2OwnerHook])

    it "does not call receivers's transfer hook if `OwnerNoOp` is selected in permission descriptor" $
      integrationalTestExpectation $ do
        receiverWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Receiver hook consumer"
        let originationParams = addAccount (wallet1, (commonOperators, 10)) $ defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorNoOpReceiverHook }
        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet1)
              (#to_ .! unTAddress receiverWithHook)
              (#amount .! 10)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          validate . Right $
            lExpectStorageConst receiverWithHook ([] :: [FA2OwnerHook])

    -- Tests that the transaction fails if senders/receiver owner hooks are NOT available
    it "fails if owner hook is not available in sender and RequiredOwnerHook is configured for sender" $
      integrationalTestExpectation $ do
        senderWithHook <- lOriginateEmpty @() contractConsumer "Sender hook consumer"
        let originationParams = addAccount (unTAddress senderWithHook, (commonOperators, 10)) $
              defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorReqSenderHook }
        withOriginated fa2Originate originationParams $ \fa2contract -> do

          let
            transfers = constructSingleTransfer
              (#from_ .! unTAddress senderWithHook)
              (#to_ .! wallet2)
              (#amount .! 10)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers

          validate $ Left (lExpectAnyMichelsonFailed fa2contract)

    it "fails if owner hook is not available in receiver and RequiredOwnerHook is configured for receiver" $
      integrationalTestExpectation $ do
        receiverWithHook <- lOriginateEmpty @() contractConsumer "Receiver hook consumer"
        let originationParams = addAccount (wallet1, (commonOperators, 10)) $
              defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorReqReceiverHook }

        withOriginated fa2Originate originationParams $ \fa2contract -> do
          let
            transfers = constructSingleTransfer
              (#from_ .! wallet1)
              (#to_ .! unTAddress receiverWithHook)
              (#amount 10)

          withSender commonOperator $ lCallEP fa2contract (Call @"Transfer") transfers
          validate $ Left (lExpectAnyMichelsonFailed fa2contract)
