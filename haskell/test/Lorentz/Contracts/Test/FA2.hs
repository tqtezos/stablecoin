-- | Tests for FA2 interface.
-- https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md

module Lorentz.Contracts.Test.FA2
  ( OriginationParams (..)
  , TokenDomain (..)
  , defaultPermissionDescriptor
  , fa2Spec
  ) where

import Data.Kind
import qualified Data.Map as Map
import Test.Hspec (Spec, it)

import Lorentz (mkView)
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Test
import Lorentz.Value
import Util.Named

wallet1, wallet2, wallet3, wallet4, wallet5, commonOperator :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
wallet4 = genesisAddress4
wallet5 = genesisAddress5
commonOperator = genesisAddress6

data TokenDomain = SingleToken | MultiToken

class IsTokenDomain (d :: TokenDomain) where
  type LedgerType d :: Type
  type LedgerInput d :: Type
  insertLedgerItem :: LedgerInput d -> LedgerType d -> LedgerType d

instance IsTokenDomain 'SingleToken where
  type LedgerType 'SingleToken = Map Address ([Address], Natural)
  type LedgerInput 'SingleToken = (Address, (Address, Natural))
  insertLedgerItem (addr, (operator, bal)) = Map.insert addr ([operator], bal)

instance IsTokenDomain 'MultiToken where
  type LedgerType 'MultiToken = Map Address ([Address], (Map TokenId Natural))
  type LedgerInput 'MultiToken = (Address, [Address], Natural, TokenId)
  insertLedgerItem (addr, operators, amount, tokenId) =
    Map.alter (\case
        Nothing -> Just (operators, (Map.singleton tokenId amount))
        Just (operators_ext, p) ->
          Just (operators_ext <> operators, (Map.insert tokenId amount p)))
      addr

data OriginationParams (d :: TokenDomain) = OriginationParams
  { opBalances :: LedgerType d
  , opPermissionsDescriptor :: PermissionsDescriptor
  , opTokenMetadata :: TokenMetadata
  }

defaultPermissionDescriptor :: PermissionsDescriptor
defaultPermissionDescriptor =
  ( #self .! SelfTransferPermitted (#self_transfer_permitted .! ())
  , #pdr .! (#operator .! OperatorTransferPermitted (#operator_transfer_permitted .! ())
  , #pdr2 .! (#receiver .! OptionalOwnerHook (#optional_owner_hook .! ())
  , #pdr3 .! (#sender .! OptionalOwnerHook (#optional_owner_hook .! ()), #custom .! Nothing))))

permissionDescriptorSelfTransferDenied :: PermissionsDescriptor
permissionDescriptorSelfTransferDenied =
  defaultPermissionDescriptor & _1 .~ (#self .! SelfTransferDenied (#self_transfer_denied .! ()))

permissionDescriptorOperatorTransferDenied :: PermissionsDescriptor
permissionDescriptorOperatorTransferDenied =
  defaultPermissionDescriptor
    & (_2.namedL #pdr._1) .~ (#operator .! OperatorTransferDenied (#operator_transfer_denied .! ()))

permissionDescriptorNoOpReceiverHook :: PermissionsDescriptor
permissionDescriptorNoOpReceiverHook =
  defaultPermissionDescriptor & (_2.namedL #pdr._2.namedL #pdr2._1) .~ (#receiver .! OwnerNoOp (#owner_no_op .! ()))

permissionDescriptorReqReceiverHook :: PermissionsDescriptor
permissionDescriptorReqReceiverHook =
  defaultPermissionDescriptor & (_2.namedL #pdr._2.namedL #pdr2._1) .~ (#receiver .! RequiredOwnerHook (#required_owner_hook .! ()))

permissionDescriptorNoOpSenderHook :: PermissionsDescriptor
permissionDescriptorNoOpSenderHook =
  defaultPermissionDescriptor & (_2.namedL #pdr._2.namedL #pdr2._2.namedL #pdr3._1) .~ (#sender .! OwnerNoOp (#owner_no_op .! ()))

permissionDescriptorReqSenderHook :: PermissionsDescriptor
permissionDescriptorReqSenderHook =
  defaultPermissionDescriptor & (_2.namedL #pdr._2.namedL #pdr2._2.namedL #pdr3._1) .~ (#sender .! RequiredOwnerHook (#required_owner_hook .! ()))

defaultTokenMetadata :: TokenMetadata
defaultTokenMetadata =
  ( #token_id .! 0
  , #mdr .! (#symbol .! [mt|TestTokenSymbol|]
  , #mdr2 .! (#name .! [mt|TestTokenName|]
  , #mdr3 .! (#decimals .! 8
  , #extras .! (Map.fromList $
       [([mt|attr1|], [mt|val1|]), ([mt|attr2|], [mt|val2|]) ]))))
  )

defaultOriginationParams :: OriginationParams 'SingleToken
defaultOriginationParams = OriginationParams
  { opBalances = mempty
  , opPermissionsDescriptor = defaultPermissionDescriptor
  , opTokenMetadata = defaultTokenMetadata
  }

addAccount
  :: forall d. (IsTokenDomain d)
  => LedgerInput d
  -> OriginationParams d
  -> OriginationParams d
addAccount i op  =
  op { opBalances = insertLedgerItem @d i (opBalances op) }

-- | Tests for an FA2 contract which -
--
-- 1. Support a single token type.
-- 2. Does not have an external permission checking transfer hook.
fa2Spec
  :: forall param. ParameterC param
  => (OriginationParams 'SingleToken -> IntegrationalScenarioM (TAddress param))
  -> Spec
fa2Spec alOriginate = do
  -- Transfer tests or tests for core transfer behavior, as per FA2
  it "Transfer amount through a series of accounts by an operator" $
    -- Tests transactions are applied in order
    -- Update balances exactly
    integrationalTestExpectation $ do
      al <- alOriginate
        $ addAccount (wallet1, (commonOperator, 10))
        $ addAccount (wallet2, (commonOperator, 0))
        $ addAccount (wallet3, (commonOperator, 0))
        $ addAccount (wallet4, (commonOperator, 0)) defaultOriginationParams
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

  it "Cannot transfer foreign money" $
    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
      let
        transfers =
          [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 1)))
          ]

      withSender wallet2 $ lCallEP al (Call @"Transfer") transfers
      validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Self transfer is denied if set so in permissions descriptior" $
    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) $ defaultOriginationParams
        { opPermissionsDescriptor = permissionDescriptorSelfTransferDenied }
      let
        transfers =
          [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 1)))
          ]

      withSender wallet1 $ lCallEP al (Call @"Transfer") transfers
      validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Operator transfer is denied if set so in permissions descriptior" $
    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) $ defaultOriginationParams
        { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied }
      let
        transfers =
          [ (#from_ .! wallet1, (#to_ .! wallet2, (#token_id .! 0, #amount .! 1)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers
      validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Transfer aborts if there is a failure" $
    -- Tests transactions are applied atomically
    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
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
  it "Balance query returns results in the expected order" $
    integrationalTestExpectation $ do
      al <- alOriginate $
        addAccount (wallet1, (commonOperator, 10)) $
        addAccount (wallet2, (commonOperator, 20)) $
        addAccount (wallet3, (commonOperator, 30)) $
        addAccount (wallet4, (commonOperator, 40)) $
        addAccount (wallet5, (commonOperator, 50)) $ defaultOriginationParams
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

-- Total_supply tests
  it "Total supply query returns results in the expected order" $
    integrationalTestExpectation $ do
      al <- alOriginate $
        addAccount (wallet1, (commonOperator, 10)) $
        addAccount (wallet2, (commonOperator, 20)) $
        addAccount (wallet3, (commonOperator, 30)) $
        addAccount (wallet4, (commonOperator, 40)) $
        addAccount (wallet5, (commonOperator, 50)) $ defaultOriginationParams
      consumer <- lOriginateEmpty @[TotalSupplyResponse] contractConsumer "consumer"
      let
        totalSupplyRequest = mkView (#token_ids .! [0]) consumer
        result =
          [ (#token_id .! 0, #total_supply .! 150) ]

      lCallEP al (Call @"Total_supply") totalSupplyRequest

      validate . Right $
        lExpectViewConsumerStorage consumer [result]

  -- Metadata tests
  it "Token metada is queryable" $
    integrationalTestExpectation $ do
      al <- alOriginate defaultOriginationParams
      consumer <- lOriginateEmpty @[TokenMetadata] contractConsumer "consumer"
      let tokenMetadataQuery = mkView (#token_ids .! [0]) consumer
      lCallEP al (Call @"Token_metadata") tokenMetadataQuery

      validate . Right $
        lExpectViewConsumerStorage
          consumer [[defaultTokenMetadata]]

  -- Permission descriptor query
  it "Permissions descriptor is queryable" $
    integrationalTestExpectation $ do
      al <- alOriginate defaultOriginationParams
      consumer <- lOriginateEmpty @PermissionsDescriptor contractConsumer "consumer"
      let permissionsDescriptorQuery = toContractRef consumer
      lCallEP al (Call @"Permissions_descriptor") permissionsDescriptorQuery

      validate . Right $ expectAnySuccess

  -- These tests require permission descriptor to be configured so that
  -- Operator transfer is allowed. We have such a configuration in
  -- defaultOriginationParams.
  it "Add operator, remove operator, is_operator works as expected" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams

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
--
  -- Check that the update operator, remove operator operations are only
  -- allowed for the owner. From the FA2 spec
  --
  --  >Operator, other than the owner, MUST be approved to manage particular token types
  --  >held by the owner to make a transfer from the owner account.

  it "Add operator is denied for non-owners" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams

      withSender wallet2 $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

        let addOperatorParam = Add_operator operatorParam
        lCallEP al (Call @"Update_operators") [addOperatorParam]

        validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Remove operator is denied for non-owners" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams

      withSender wallet2 $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! commonOperator, #tokens .! All_tokens)

        let removeOperatorParam = Remove_operator operatorParam
        lCallEP al (Call @"Update_operators") [removeOperatorParam]
        validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Add operator is denied for operators" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams

      withSender commonOperator $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

        let addOperatorParam = Add_operator operatorParam
        lCallEP al (Call @"Update_operators") [addOperatorParam]

        validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Remove operator is denied for operators" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams

      withSender commonOperator $ do
        let operatorParam =
              (#owner .! wallet1, #operator .! commonOperator, #tokens .! All_tokens)

        let removeOperatorParam = Remove_operator operatorParam
        lCallEP al (Call @"Update_operators") [removeOperatorParam]
        validate $ Left (lExpectFailWith (const @_ @MText True))

  -- FA2 Mandates that the entrypoints to configure operators should fail if
  -- operator transfer is denied in permissions descriptor.
  it "Operator config entrypoint must fail if Operator transfer is forbidden" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10))
        (defaultOriginationParams
          { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied })

      let operatorParam =
            (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

      let addOperatorParam = Add_operator operatorParam
      lCallEP al (Call @"Update_operators") [addOperatorParam]
      validate $ Left (lExpectFailWith (const @_ @MText True))

  -- FA2 Mandates that the entrypoints to check operator status should fail if
  -- operator transfer is denied in permissions descriptor.
  it "Is Operator entrypoint must fail if Operator transfer is forbidden" $

    integrationalTestExpectation $ do
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10))
        (defaultOriginationParams
          { opPermissionsDescriptor = permissionDescriptorOperatorTransferDenied })

      consumer <- lOriginateEmpty @IsOperatorResponse contractConsumer "consumer"
      let operatorParam =
            (#owner .! wallet1, #operator .! wallet2, #tokens .! All_tokens)

      let isOperatorQuery = mkView (#operator .! operatorParam) consumer
      lCallEP al (Call @"Is_operator") isOperatorQuery
      validate $ Left (lExpectFailWith (const @_ @MText True))

  -- Owner hooks test
  --
  -- Tests that the senders owner hook is called on transfer
  -- uses defaultOptions where both sender/receiver hooks are set
  -- to be optional.
  it "Sender's transfer hook is called on transfer" $
    integrationalTestExpectation $ do
      senderWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Sender hook consumer"
      al <- alOriginate $
        addAccount (unTAddress senderWithHook, (commonOperator, 10)) defaultOriginationParams
      let
        transfers =
          [ (#from_ .! unTAddress senderWithHook, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

      let expectedTransferDesc =
           (#from .! Just (unTAddress senderWithHook), (#to .! Just wallet2, (#token_id .! 0, #amount .! 10)))

      let expectedHookContractState =
            Tokens_sent (#fa2 .! unTAddress al, (#batch .! [expectedTransferDesc], #operator .! commonOperator))

      validate . Right $
        lExpectViewConsumerStorage senderWithHook [expectedHookContractState]
--
  it "Receiver's transfer hook is called on transfer" $
    integrationalTestExpectation $ do
      receiverWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Receiver hook consumer"
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) defaultOriginationParams
      let
        transfers =
          [ (#from_ .! wallet1, (#to_ .! unTAddress receiverWithHook, (#token_id .! 0, #amount .! 10)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

      let expectedTransferDesc =
           (#from .! Just wallet1, (#to .! (Just $ unTAddress receiverWithHook), (#token_id .! 0, #amount .! 10)))

      let expectedHookContractState
            = Tokens_received
                (#fa2 .! unTAddress al, (#batch .! [expectedTransferDesc], #operator .! commonOperator))

      validate . Right $
        lExpectViewConsumerStorage receiverWithHook [expectedHookContractState]

  -- Tests that the senders/receiver owner hook are NOT called on transfer
  -- using @OwnerNoOp@ values in permissions_decriptor
  it "Sender's transfer hook is NOT called on transfer" $
    integrationalTestExpectation $ do
      senderWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Sender hook consumer"
      al <- alOriginate $
        addAccount (unTAddress senderWithHook, (commonOperator, 10)) $
          defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorNoOpSenderHook }
      let
        transfers =
          [ (#from_ .! unTAddress senderWithHook, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

      validate . Right $
        lExpectViewConsumerStorage senderWithHook []

  it "Receiver's transfer hook is NOT called on transfer" $
    integrationalTestExpectation $ do
      receiverWithHook <- lOriginateEmpty @FA2OwnerHook contractConsumer "Receiver hook consumer"
      al <- alOriginate $ addAccount (wallet1, (commonOperator, 10)) $ defaultOriginationParams
              { opPermissionsDescriptor = permissionDescriptorNoOpReceiverHook }
      let
        transfers =
          [ (#from_ .! wallet1, (#to_ .! unTAddress receiverWithHook, (#token_id .! 0, #amount .! 10)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

      let expectedTransferDesc =
           (#from .! Just wallet1, (#to .! (Just $ unTAddress receiverWithHook) , (#token_id .! 0, #amount .! 10)))

      let expectedHookContractState
            = Tokens_received (#fa2 .! unTAddress al, (#batch .! [expectedTransferDesc], #operator .! commonOperator))

      validate . Right $
        lExpectViewConsumerStorage receiverWithHook [expectedHookContractState]

  -- Tests that the transaction fails if senders/receiver owner hooks are NOT available
  -- using @RequiredOwnerHook@ values in permissions_decriptor
  it "Transaction fails if owner hook is not available in sender" $
    integrationalTestExpectation $ do
      senderWithHook <- lOriginateEmpty @() contractConsumer "Sender hook consumer"
      al <- alOriginate $
          addAccount (unTAddress senderWithHook, (commonOperator, 10)) $
            defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorReqSenderHook }
      let
        transfers =
          [ (#from_ .! unTAddress senderWithHook, (#to_ .! wallet2, (#token_id .! 0, #amount .! 10)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers

      validate $ Left (lExpectFailWith (const @_ @MText True))

  it "Transaction fails if owner hook is not available in receiver" $
    integrationalTestExpectation $ do
      receiverWithHook <- lOriginateEmpty @() contractConsumer "Receiver hook consumer"
      al <- alOriginate $
        addAccount (wallet1, (commonOperator, 10)) $
          defaultOriginationParams { opPermissionsDescriptor = permissionDescriptorReqReceiverHook }
      let
        transfers =
          [ (#from_ .! wallet1, (#to_ .! unTAddress receiverWithHook, (#token_id .! 0, #amount .! 10)))
          ]

      withSender commonOperator $ lCallEP al (Call @"Transfer") transfers
      validate $ Left (lExpectFailWith (const @_ @MText True))
