-- | Tests for FA2 interface.
-- https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md

module Lorentz.Contracts.Test.FA2
  ( OriginationParams (..)
  , defaultPermissionDescriptor
  , fa2Spec
  ) where

import qualified Data.Map as Map
import Test.Hspec (Spec, describe, it)

import Lorentz (mkView)
import qualified Lorentz as L
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test
import Lorentz.Value
import Tezos.Core (unsafeMkMutez)
import Util.Named

wallet1, wallet2, wallet3, wallet4, wallet5, commonOperator :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
wallet4 = genesisAddress4
wallet5 = genesisAddress5
commonOperator = genesisAddress6

type LedgerType = Map Address ([Address], Natural)
type LedgerInput = (Address, (Address, Natural))

insertLedgerItem :: LedgerInput -> LedgerType -> LedgerType
insertLedgerItem (addr, (operator, bal)) = Map.insert addr ([operator], bal)

data OriginationParams = OriginationParams
  { opBalances :: LedgerType
  , opPermissionsDescriptor :: PermissionsDescriptorMaybe
  , opTokenMetadata :: TokenMetadata
  }

defaultPermissionDescriptor :: PermissionsDescriptorMaybe
defaultPermissionDescriptor =
  ( #self .! Nothing
  , #pdr .! ( #operator .! Nothing
            , #pdr2 .! ( #receiver .! Nothing
                       , #pdr3 .! (#sender .! Nothing, #custom .! Nothing))))

permissionDescriptorSelfTransferDenied :: PermissionsDescriptorMaybe
permissionDescriptorSelfTransferDenied =
  defaultPermissionDescriptor &
    _1 .~ (#self .! (Just $ SelfTransferDenied (#self_transfer_denied .! ())))

permissionDescriptorOperatorTransferDenied :: PermissionsDescriptorMaybe
permissionDescriptorOperatorTransferDenied =
  defaultPermissionDescriptor
   & (_2.namedL #pdr._1)
      .~ (#operator .! (Just $ OperatorTransferDenied (#operator_transfer_denied .! ())))

permissionDescriptorNoOpReceiverHook :: PermissionsDescriptorMaybe
permissionDescriptorNoOpReceiverHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr._2.namedL #pdr2._1) .~ (#receiver .! (Just $ OwnerNoOp (#owner_no_op .! ())))

permissionDescriptorReqReceiverHook :: PermissionsDescriptorMaybe
permissionDescriptorReqReceiverHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr._2.namedL #pdr2._1)
    .~ (#receiver .! (Just $ RequiredOwnerHook (#required_owner_hook .! ())))

permissionDescriptorNoOpSenderHook :: PermissionsDescriptorMaybe
permissionDescriptorNoOpSenderHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr._2.namedL #pdr2._2.namedL #pdr3._1)
    .~ (#sender .! (Just $ OwnerNoOp (#owner_no_op .! ())))

permissionDescriptorReqSenderHook :: PermissionsDescriptorMaybe
permissionDescriptorReqSenderHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr._2.namedL #pdr2._2.namedL #pdr3._1)
    .~ (#sender .! (Just $ RequiredOwnerHook (#required_owner_hook .! ())))

defaultTokenMetadata :: TokenMetadata
defaultTokenMetadata =
  ( #token_id .! 0
  , #mdr .! (#symbol .! [mt|TestTokenSymbol|]
  , #mdr2 .! (#name .! [mt|TestTokenName|]
  , #mdr3 .! (#decimals .! 8
  , #extras .! (Map.fromList $
       [([mt|attr1|], [mt|val1|]), ([mt|attr2|], [mt|val2|]) ]))))
  )

defaultOriginationParams :: OriginationParams
defaultOriginationParams = OriginationParams
  { opBalances = mempty
  , opPermissionsDescriptor = defaultPermissionDescriptor
  , opTokenMetadata = defaultTokenMetadata
  }

addAccount
  :: LedgerInput
  -> OriginationParams
  -> OriginationParams
addAccount i op  =
  op { opBalances = insertLedgerItem i (opBalances op) }

-- | The return value of this function is a Maybe to handle the case where a contract
-- having hardcoded permission descriptor, and thus unable to initialize with a custom
-- permissions descriptor passed from the testing code.
--
-- In such cases, where the value is hard coded and is incompatible with what is required
-- for the test, this function should return a Nothing value, and the tests that depend
-- on such custom configuration will be skipped.
type OriginationFn param = (OriginationParams -> IntegrationalScenarioM (Maybe (TAddress param)))

-- | This is a temporary hack to workaround the inability to skip the
-- tests from an IntegrationalScenario without doing any validations.
skipTest :: IntegrationalScenario
skipTest = do
  let
    dummyContract :: L.ContractCode () ()
    dummyContract = L.unpair L.# L.drop L.# L.nil L.# L.pair
  c <- lOriginate dummyContract "skip test dummy" () (unsafeMkMutez 0)
  lCallEP c CallDefault ()
  validate (Right expectAnySuccess)

withOriginated
  :: forall param. OriginationFn param
  -> OriginationParams
  -> (TAddress param -> IntegrationalScenario)
  -> IntegrationalScenario
withOriginated fn op tests = do
  (fn op) >>= \case
    Nothing -> skipTest
    Just contract -> tests contract

-- | Tests for an FA2 contract which -
--
-- 1. Supports a single token type.
-- 2. Does not have an external permission checking transfer hook.
fa2Spec
  :: forall param. ParameterC param
  => OriginationFn param
  -> Spec
fa2Spec alOriginate = do
  describe "Transfer entrypoint call by an operator" $ do
  -- Transfer tests or tests for core transfer behavior, as per FA2
    it "executes transfer the given order" $ integrationalTestExpectation $ do
      -- Tests transactions are applied in order
      -- Update balances exactly
        let originationParams = addAccount (wallet1, (commonOperator, 10))
              $ addAccount (wallet2, (commonOperator, 0))
              $ addAccount (wallet3, (commonOperator, 0))
              $ addAccount (wallet4, (commonOperator, 0)) defaultOriginationParams
        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
              , (#from_ .! wallet2, (#to_ .! wallet3, (#token_id .! 0, #amount .! 10)))
              , (#from_ .! wallet3, (#to_ .! wallet4, (#token_id .! 0, #amount .! 10)))
              , (#from_ .! wallet4, (#to_ .! wallet5, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

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

          lCallEP al (Call @"Balance_of") balanceRequest

          validate . Right $
            lExpectViewConsumerStorage consumer [balanceExpected]

    it "Cannot transfer foreign money" $ integrationalTestExpectation $ do
      let originationParams = (addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams)
      withOriginated alOriginate originationParams $ \al -> do
        let
          transfers =
            [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 1)))
            ]

        withSender wallet2 $ lCallEP al (Call @"Transfer") transfers
        validate $ Left (lExpectFailWith (const @_ @MText True))

    it "denies self transfer if set so in permissions descriptior" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperator, 10)) $
            defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorSelfTransferDenied }
      withOriginated alOriginate originationParams $ \al -> do
        let
          transfers =
            [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 1)))
            ]

        withSender wallet1 $ lCallEP al (Call @"Transfer") transfers
        validate $ Left (lExpectFailWith (const @_ @MText True))

    it "denies operator transfer if set so in permissions descriptior" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperator, 10)) $
              defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }
        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 1)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers
          validate $ Left (lExpectFailWith (const @_ @MText True))

    it "aborts if there is a failure" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
      -- Tests transactions are applied atomically
      withOriginated alOriginate originationParams $ \al -> do
        let
          transfers =
            [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
            , (#from_ .! wallet2, (#to_ .! wallet3, (#token_id .! 0, #amount .! 10)))
            , (#from_ .! wallet4, (#to_ .! wallet3, (#token_id .! 0, #amount .! 10))) -- Should fail
            , (#from_ .! wallet4, (#to_ .! wallet5, (#token_id .! 0, #amount .! 10)))
            ]

        withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

        validate $ Left (lExpectFailWith (const @_ @MText True))

  -- Balance_of tests
  describe "Balance_of entrypoint" $
    it "returns results in the expected order" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperator, 10)) $
            addAccount (wallet2, (commonOperator, 20)) $
            addAccount (wallet3, (commonOperator, 30)) $
            addAccount (wallet4, (commonOperator, 40)) $
            addAccount (wallet5, (commonOperator, 50)) $ defaultOriginationParams
      withOriginated alOriginate originationParams $ \al -> do
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

        lCallEP al (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

  describe "Total_supply entrypoint" $
    it "returns results in the expected order" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperator, 10)) $
            addAccount (wallet2, (commonOperator, 20)) $
            addAccount (wallet3, (commonOperator, 30)) $
            addAccount (wallet4, (commonOperator, 40)) $
            addAccount (wallet5, (commonOperator, 50)) $ defaultOriginationParams
      withOriginated alOriginate originationParams $ \al -> do
        consumer <- lOriginateEmpty @[TotalSupplyResponse] contractConsumer "consumer"
        let
          totalSupplyRequest = mkView (#token_ids .! [0]) consumer
          result =
            [ (#token_id .! 0, #total_supply .! 150) ]

        lCallEP al (Call @"Total_supply") totalSupplyRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [result]

  ---- Metadata tests
  describe "Metadata query entrypoint" $
    it "is available" $ integrationalTestExpectation $
      withOriginated alOriginate defaultOriginationParams $ \al -> do
        consumer <- lOriginateEmpty @[TokenMetadata] contractConsumer "consumer"
        let tokenMetadataQuery = mkView (#token_ids .! [0]) consumer
        lCallEP al (Call @"Token_metadata") tokenMetadataQuery

        validate . Right $
          lExpectViewConsumerStorage
            consumer [[defaultTokenMetadata]]

  -- Permission descriptor query
  describe "Permissions_descriptor entrypoint" $
    it "is available" $ integrationalTestExpectation $
      withOriginated alOriginate defaultOriginationParams $ \al -> do
        consumer <- lOriginateEmpty @PermissionsDescriptor contractConsumer "consumer"
        let permissionsDescriptorQuery = toContractRef consumer
        lCallEP al (Call @"Permissions_descriptor") permissionsDescriptorQuery

        validate . Right $ expectAnySuccess

  ---- These tests require permission descriptor to be configured so that
  ---- Operator transfer is allowed. We have such a configuration in
  ---- defaultOriginationParams.
  describe "Configure operators entrypoint" $ do
    it "adds operator, removes operator, and answer operator query as expected" $
      integrationalTestExpectation $ do
        let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
        withOriginated alOriginate originationParams $ \al -> do

          consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
          withSender wallet1 $ do
            let operatorParam =
                  (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

            let addOperatorParam = Add_operator operatorParam
            lCallEP al (Call @"Update_operators") [addOperatorParam]

            let isOperatorQuery = mkView (#operator .! operatorParam) consumer
            lCallEP al (Call @"Is_operator") isOperatorQuery

            offshoot "Confirm operator was added" $
              (validate . Right $
                lExpectViewConsumerStorage consumer
                  [(#operator .! operatorParam, #is_operator .! True)])

            let removeOperatorParam = Remove_operator operatorParam
            lCallEP al (Call @"Update_operators") [removeOperatorParam]
            void $ validate . Right $ expectAnySuccess

            lCallEP al (Call @"Is_operator") isOperatorQuery
            validate . Right $
              lExpectViewConsumerStorage
                consumer
                  [ (#operator .! operatorParam, #is_operator .! True)
                  , (#operator .! operatorParam, #is_operator .! False)
                  ]

  ---- Check that the update operator, remove operator operations are only
  ---- allowed for the owner. From the FA2 spec
  ----
  ----  >Operator, other than the owner, MUST be approved to manage particular token types
  ----  >held by the owner to make a transfer from the owner account.

  describe "Configure operators entrypoint" $
    it "denies addOperator call for non-owners" $ integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
      withOriginated alOriginate originationParams $ \al -> do

        withSender wallet2 $ do
          let operatorParam =
                (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

          let addOperatorParam = Add_operator operatorParam
          lCallEP al (Call @"Update_operators") [addOperatorParam]

          validate $ Left (lExpectFailWith (const @_ @MText True))

  it "denies removeOperator call for non-owners" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
    withOriginated alOriginate originationParams $ \al -> do

      withSender wallet2 $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! commonOperator, #tokens .! All_tokens)

        let removeOperatorParam = Remove_operator operatorParam
        lCallEP al (Call @"Update_operators") [removeOperatorParam]
        validate $ Left (lExpectFailWith (const @_ @MText True))

  it "denies addOperator for operators" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
    withOriginated alOriginate originationParams $ \al -> do

      withSender commonOperator $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

        let addOperatorParam = Add_operator operatorParam
        lCallEP al (Call @"Update_operators") [addOperatorParam]

        validate $ Left (lExpectFailWith (const @_ @MText True))

  it "denies removeOperator for operators" $ integrationalTestExpectation $ do
    let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
    withOriginated alOriginate originationParams $ \al -> do

      withSender commonOperator $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! commonOperator, #tokens .! All_tokens)

        let removeOperatorParam = Remove_operator operatorParam
        lCallEP al (Call @"Update_operators") [removeOperatorParam]
        validate $ Left (lExpectFailWith (const @_ @MText True))

  -- FA2 Mandates that the entrypoints to configure operators should fail if
  -- operator transfer is denied in permissions descriptor.
  it "errors on addOperator call if operator transfer is forbidden" $
    integrationalTestExpectation $ do
      let originationParams = addAccount (wallet1, (commonOperator, 10))
            defaultOriginationParams
              { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }

      withOriginated alOriginate originationParams $ \al -> do

        let operatorParam =
              (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

        let addOperatorParam = Add_operator operatorParam
        lCallEP al (Call @"Update_operators") [addOperatorParam]
        validate $ Left (lExpectFailWith (const @_ @MText True))

  -- FA2 Mandates that the entrypoints to check operator status should fail if
  -- operator transfer is denied in permissions descriptor.
  it "errors on isOperator call if operator transfer is forbidden" $ integrationalTestExpectation $ do

    let originationParams = addAccount (wallet1, (commonOperator, 10))
          defaultOriginationParams
            { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }

    withOriginated alOriginate originationParams $ \al -> do

      consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
      let operatorParam =
            (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

      let isOperatorQuery = mkView (#operator .! operatorParam) consumer
      lCallEP al (Call @"Is_operator") isOperatorQuery
      validate $ Left (lExpectFailWith (const @_ @MText True))

  ---- Owner hooks test
  ----
  ---- Tests that the senders owner hook is called on transfer
  ---- uses defaultOptions where both sender/receiver hooks are set
  ---- to be optional.
  describe "Owner hook behavior on transfer" $ do
    it "calls sender's transfer hook on transfer" $ integrationalTestExpectation $ do
        senderWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Sender hook consumer"
        let originationParams =
              addAccount (unTAddress senderWithHook, (commonOperator, 10)) defaultOriginationParams
        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ (#from_ .! unTAddress senderWithHook, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

          let expectedTransferDesc =
               ( #from .! Just (unTAddress senderWithHook)
               , (#to .! Just wallet2, (#token_id .! 0, #amount .! 10)))

          let expectedHookContractState =
                Tokens_sent ( #fa2 .! unTAddress al
                            , (#batch .! [expectedTransferDesc], #operator .! commonOperator))

          validate . Right $
            lExpectViewConsumerStorage senderWithHook [expectedHookContractState]

    it "calls receiver's transfer hook on transfer" $
      integrationalTestExpectation $ do
        receiverWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Receiver hook consumer"
        let originationParams = addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ ( #from_ .! wallet1
                , (#to_ .! unTAddress receiverWithHook, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

          let expectedTransferDesc =
               ( #from .! Just wallet1
               , (#to .! (Just $ unTAddress receiverWithHook), (#token_id .! 0, #amount .! 10)))

          let expectedHookContractState
                = Tokens_received
                    ( #fa2 .! unTAddress al
                    , (#batch .! [expectedTransferDesc], #operator .! commonOperator))

          validate . Right $
            lExpectViewConsumerStorage receiverWithHook [expectedHookContractState]

    -- Tests that the senders/receiver owner hook are NOT called on transfer
    it "does not call sender's transfer hook if `OwnerNoOp` is selected in permission descriptor" $
      integrationalTestExpectation $ do
        senderWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Sender hook consumer"
        let originationParams = addAccount (unTAddress senderWithHook, (commonOperator, 10)) $
              defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorNoOpSenderHook }

        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ (#from_ .! unTAddress senderWithHook
                , (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

          validate . Right $
            lExpectViewConsumerStorage senderWithHook []

    it "does not call receivers's transfer hook if `OwnerNoOp` is selected in permission descriptor" $
      integrationalTestExpectation $ do
        receiverWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Receiver hook consumer"
        let originationParams = addAccount (wallet1, (commonOperator, 10)) $ defaultOriginationParams
                { opPermissionsDescriptor = permissionDescriptorNoOpReceiverHook }
        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ (#from_ .! wallet1,
                    (#to_ .! unTAddress receiverWithHook, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

          let expectedTransferDesc =
               (#from .! Just wallet1,
                  (#to .! (Just $ unTAddress receiverWithHook) , (#token_id .! 0, #amount .! 10)))

          let expectedHookContractState
                = Tokens_received (#fa2 .! unTAddress al,
                      (#batch .! [expectedTransferDesc], #operator .! commonOperator))

          validate . Right $
            lExpectViewConsumerStorage receiverWithHook [expectedHookContractState]

    -- Tests that the transaction fails if senders/receiver owner hooks are NOT available
    it "fails if owner hook is not available in sender and RequiredOwnerHook is configured for sender" $
      integrationalTestExpectation $ do
        senderWithHook <- lOriginateEmpty @() contractConsumer "Sender hook consumer"
        let originationParams = addAccount (unTAddress senderWithHook, (commonOperator, 10)) $
              defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorReqSenderHook }
        withOriginated alOriginate originationParams $ \al -> do

          let
            transfers =
              [ (#from_ .! unTAddress senderWithHook, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

          validate $ Left (lExpectFailWith (const @_ @MText True))

    it "fails if owner hook is not available in receiver and RequiredOwnerHook is configured for receiver" $
      integrationalTestExpectation $ do
        receiverWithHook <- lOriginateEmpty @() contractConsumer "Receiver hook consumer"
        let originationParams = addAccount (wallet1, (commonOperator, 10)) $
              defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorReqReceiverHook }

        withOriginated alOriginate originationParams $ \al -> do
          let
            transfers =
              [ (#from_ .! wallet1, (#to_ .! unTAddress receiverWithHook, (#token_id .! 0, #amount .! 10)))
              ]

          withSender commonOperator $ lCallEP al (Call @"Transfer") transfers
          validate $ Left (lExpectFailWith (const @_ @MText True))
