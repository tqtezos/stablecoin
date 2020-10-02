-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

{-# LANGUAGE PackageImports #-}

-- | Tests for management entrypoints of stablecoin smart-contract

module Lorentz.Contracts.Test.Management
  ( managementSpec
  , mgmNotContractOwner
  , mgmNotPendingOwner
  , mgmNotPauser
  , mgmNotMasterMinter
  ) where

import Data.Map as M (fromList, lookup)
import Data.Map.Strict as M (size)
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it)

import Lorentz (mkView, mt, unBigMap)
import Lorentz.Address
import Lorentz.Test
import Michelson.Runtime (ExecutorError)
import Tezos.Address (detGenKeyAddress)
import Tezos.Core (unsafeMkMutez)
import Util.Named

import qualified Indigo.Contracts.Transferlist.Internal as Transferlist
import qualified "stablecoin" Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin hiding (stablecoinContract)
import Lorentz.Contracts.Test.Common

mgmXtzReceived :: ExecutorError -> IntegrationalScenario
mgmXtzReceived = lExpectFailWith (== [mt|XTZ_RECEIVED|])

mgmNotContractOwner :: ExecutorError -> IntegrationalScenario
mgmNotContractOwner = lExpectFailWith (== [mt|NOT_CONTRACT_OWNER|])

mgmNotPendingOwner :: ExecutorError -> IntegrationalScenario
mgmNotPendingOwner = lExpectFailWith (== [mt|NOT_PENDING_OWNER|])

mgmNoPendingOwnerSet :: ExecutorError -> IntegrationalScenario
mgmNoPendingOwnerSet = lExpectFailWith (== [mt|NO_PENDING_OWNER_SET|])

mgmNotPauser :: ExecutorError -> IntegrationalScenario
mgmNotPauser = lExpectFailWith (== [mt|NOT_PAUSER|])

mgmNotMasterMinter :: ExecutorError -> IntegrationalScenario
mgmNotMasterMinter = lExpectFailWith (== [mt|NOT_MASTER_MINTER|])

mgmNotMinter :: ExecutorError -> IntegrationalScenario
mgmNotMinter = lExpectFailWith (== [mt|NOT_MINTER|])

mgmContractNotPaused :: ExecutorError -> IntegrationalScenario
mgmContractNotPaused = lExpectFailWith (== [mt|CONTRACT_NOT_PAUSED|])

mgmInsufficientBalance :: ExecutorError -> IntegrationalScenario
mgmInsufficientBalance = lExpectFailWith (== [mt|FA2_INSUFFICIENT_BALANCE|])

mgmCurrentAllowanceRequired :: ExecutorError -> IntegrationalScenario
mgmCurrentAllowanceRequired = lExpectFailWith (== [mt|CURRENT_ALLOWANCE_REQUIRED|])

mgmAllowanceMismatch :: ExecutorError -> IntegrationalScenario
mgmAllowanceMismatch = lExpectFailWith (== [mt|ALLOWANCE_MISMATCH|])

mgmAddrNotMinter :: ExecutorError -> IntegrationalScenario
mgmAddrNotMinter = lExpectFailWith (== [mt|ADDR_NOT_MINTER|])

mgmAllowanceExceeded :: ExecutorError -> IntegrationalScenario
mgmAllowanceExceeded = lExpectFailWith (== [mt|ALLOWANCE_EXCEEDED|])

mgmBadTransferlist :: ExecutorError -> IntegrationalScenario
mgmBadTransferlist = lExpectFailWith (== [mt|BAD_TRANSFERLIST|])

mgmMinterLimitExceeded :: ExecutorError -> IntegrationalScenario
mgmMinterLimitExceeded = lExpectFailWith (== [mt|MINTER_LIMIT_REACHED|])

managementSpec
  :: forall param. ParameterC param
  => OriginationFn param
  -> Spec
managementSpec originate = do
  describe "Contract meta" $ do
    it "fails if contract is received non-zero amount of xtz" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 0))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        err <- expectError $ lTransfer @param
          (#from .! commonOperator)
          (#to .! unTAddress stablecoinContract)
          (unsafeMkMutez 10) -- Error here
          (Call @"Transfer")
          -- Dummy transfer needed to call something from a contract since we don't have default entrypoint set
          (constructTransfersFromSender (#from_ .! wallet1) [])
        mgmXtzReceived err

    it "token metadata registry is present in storage" $ integrationalTestExpectation $ do
      mrAddress <- originateMetadataRegistry
      let originationParams = defaultOriginationParams { opTokenMetadataRegistry = Just mrAddress }
      withOriginated originate originationParams $ \stablecoinContract -> do
        lExpectStorage @Storage stablecoinContract $ \storage ->
          unless (sTokenMetadataRegistry storage == mrAddress) $
            Left $ CustomTestError "Malformed token metadata registry address in contract storage"

  describe "Contract pausing" $ do
    it "pauses contract as expected" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testPauser $ lCallEP stablecoinContract (Call @"Pause") ()
        lExpectStorage @Storage stablecoinContract $ \storage ->
          unless (sIsPaused storage) $
            Left $ CustomTestError "Contract is not paused as was expected"

    it "cannot pause if sender does not have corresponding permissions" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet2 $ lCallEP stablecoinContract (Call @"Pause") ()
        mgmNotPauser err

    it "pause cannot be called multiple times in a row" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testPauser $ do
          lCallEP stablecoinContract (Call @"Pause") ()
          err <- expectError $ lCallEP stablecoinContract (Call @"Pause") ()
          mgmContractPaused err

    it "unpauses contract as expected" $ integrationalTestExpectation $ do
      let originationParams = defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender testPauser $ lCallEP stablecoinContract (Call @"Unpause") ()
        lExpectStorage @Storage stablecoinContract $ \storage ->
          when (sIsPaused storage) $
            Left $ CustomTestError "Contract is paused which wasn't expected"

    it "cannot unpause if sender does not have corresponding permissions" $ integrationalTestExpectation $ do
      let originationParams = defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet2 $ lCallEP stablecoinContract (Call @"Unpause") ()
        mgmNotPauser err

    it "unpause cannot be called multiple times in a row" $ integrationalTestExpectation $ do
      let originationParams = defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender testPauser $ lCallEP stablecoinContract (Call @"Unpause") ()
        err <- expectError $ withSender testPauser $ lCallEP stablecoinContract (Call @"Unpause") ()
        mgmContractNotPaused err

    it "prevents transfers while contract is paused" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ addAccount (wallet2, ([], 0))
          $ defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        let transfers = constructSingleTransfer (#from_ .! wallet1) (#to_ .! wallet2) (#amount .! 10)
        err <- expectError $ withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfers
        mgmContractPaused err

    it "can successfully transfer tokens after contract unpause" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ addAccount (wallet2, (commonOperators, 0))
          $ addAccount (wallet3, (commonOperators, 0))
          $ defaultOriginationParams { opPaused = True }

      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender testPauser $ lCallEP stablecoinContract (Call @"Unpause") ()
        let
          transfer1 = constructTransfersFromSender (#from_ .! wallet1)
            [ (#to_ .! wallet2, #amount .! 5)
            , (#to_ .! wallet2, #amount .! 5)
            ]

          transfer2 = constructSingleTransfer
            (#from_ .! wallet2)
            (#to_ .! wallet3)
            (#amount .! 10)

        withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfer1
        withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfer2

        consumer <- lOriginateEmpty @[FA2.BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ FA2.BalanceRequestItem wallet1 0
            , FA2.BalanceRequestItem wallet2 0
            , FA2.BalanceRequestItem wallet3 0
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet1 0, briBalance = 0 }
            , FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet2 0, briBalance = 0 }
            , FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet3 0, briBalance = 10 }
            ]

        lCallEP stablecoinContract (Call @"Balance_of") balanceRequest

        lExpectViewConsumerStorage consumer [balanceExpected]

  describe "Configure minter" $ do
    it "configures minter properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam = ConfigureMinterParam
            { cmpMinter = wallet1
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 30
            }

          configureMinterParam1 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 20
            }

        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam
        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        let
          configureMinterParam2 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Just 20
            , cmpNewMintingAllowance = 10
            }

        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam2

        lExpectStorage @Storage stablecoinContract $ \storage ->
          let expectedMinters = fromList [(wallet1, 30), (wallet2, 10)]
          in  when (sMintingAllowances storage /= expectedMinters) $
                Left $ CustomTestError "Configure_minter call produced a malformed minter list"

    it "fails if expected and actual minting allowances do not match" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam1 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 20
            }

        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        let
          configureMinterParam2 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Just 2000 -- Mismatched allowance here
            , cmpNewMintingAllowance = 10
            }

        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam2

        mgmAllowanceMismatch err

    it "fails if minter is present in list of minters which was not expected" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam1 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 20
            }

        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        let
          configureMinterParam2 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Nothing -- Here we expect for `wallet2` being non-present in minting allowances map
            , cmpNewMintingAllowance = 10
            }

        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam2

        mgmCurrentAllowanceRequired err

    it "fails if sender does not have master minter permissions" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam1 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 20
            }

        err <- expectError $ withSender wallet2 $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        mgmNotMasterMinter err

    it "fails if contract is paused" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testPauser $ lCallEP stablecoinContract (Call @"Pause") ()

        let
          configureMinterParam1 = ConfigureMinterParam
            { cmpMinter = wallet2
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 20
            }

        err <- expectError $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        mgmContractPaused err

  describe "Minter limit check" $ do
    let
      configureMinterParam :: Int -> ConfigureMinterParam
      configureMinterParam i = ConfigureMinterParam
        { cmpMinter = detGenKeyAddress $ encodeUtf8 @Text ("a" <> show i)
        , cmpCurrentMintingAllowance = Nothing
        , cmpNewMintingAllowance = 20
        }

    it "Can add minter until minter limit" $ integrationalTestExpectation $ do

      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testMasterMinter $ do
          mapM_ (lCallEP stablecoinContract (Call @"Configure_minter")) (configureMinterParam <$> [1..minterLimit])

          lExpectStorage @Storage stablecoinContract $ \storage ->
            unless (M.size (sMintingAllowances storage) == minterLimit) $
                Left $ CustomTestError "Configure_minter call produced a malformed minter list"

    it "Throws error when minter limit is exceeded" $ integrationalTestExpectation $ do

      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testMasterMinter $ do
         err <- expectError $ mapM_ (lCallEP stablecoinContract (Call @"Configure_minter")) (configureMinterParam <$> [1..(minterLimit + 1)])

         mgmMinterLimitExceeded err

  describe "Remove minter" $ do
    it "successfully removes minter from minting list" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 10)
          $ addMinter (wallet2, 0)
          $ addMinter (wallet3, 100)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet2
        lExpectStorage @Storage stablecoinContract $ \storage ->
          let expectedMinters = fromList [(wallet3, 100)]
          in  when (sMintingAllowances storage /= expectedMinters) $
                Left $ CustomTestError "Remove minter does not change minter list"

    it "fails if sender is not master minter" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet2 $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        mgmNotMasterMinter err

    it "cannot remove the same wallet if it's already removed" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        mgmAddrNotMinter err

    it "cannot remove non-minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        mgmAddrNotMinter err



  describe "Minting" $ do
    it "successfully mints tokens" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 30)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        let
          mintings =
            [ MintParam wallet1 10
            , MintParam wallet2 5
            , MintParam wallet3 15
            ]

        withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

        consumer <- lOriginateEmpty @[FA2.BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ FA2.BalanceRequestItem wallet1 0
            , FA2.BalanceRequestItem wallet2 0
            , FA2.BalanceRequestItem wallet3 0
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet1 0, briBalance = 10 }
            , FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet2 0, briBalance = 5 }
            , FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet3 0, briBalance = 15 }
            ]

        lCallEP stablecoinContract (Call @"Balance_of") balanceRequest

        lExpectViewConsumerStorage consumer [balanceExpected]

    it "aborts whole transaction if the sum of minting tokens at a given step exceeds current minting allowance" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 10)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        let
          mintings =
            [ MintParam wallet1 5
            , MintParam wallet2 10 -- Error here
            , MintParam wallet3 5
            ]

        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

        lExpectAnyMichelsonFailed stablecoinContract err

    it "fails if sender is not minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let mintings = [MintParam wallet1 5]
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings
        lExpectAnyMichelsonFailed stablecoinContract err

    it "fails minting if contract is paused" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 10)
          $ defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        let mintings = [MintParam wallet1 5]
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings
        mgmContractPaused err



  describe "Burning" $ do
    it "burns tokens as expected" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addMinter (wallet2, 0)
          $ addAccount (wallet1, (commonOperators, 35))
          $ addAccount (wallet2, (commonOperators, 0))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do

        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10, 20 ]
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Burn") [ 0 ]

        consumer <- lOriginateEmpty @[FA2.BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ FA2.BalanceRequestItem wallet1 0
            , FA2.BalanceRequestItem wallet2 0
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet1 0, briBalance = 5 }
            , FA2.BalanceResponseItem { briRequest = FA2.BalanceRequestItem wallet2 0, briBalance = 0 }
            ]

        lCallEP stablecoinContract (Call @"Balance_of") balanceRequest

        lExpectViewConsumerStorage consumer [balanceExpected]

    it "removes account if balance after burning is zero" $ integrationalTestExpectation $ do

      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 35))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do

        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 35 ]

        lExpectStorage @Storage stablecoinContract $ \storage ->
          unless (M.lookup wallet1 (unBigMap $ sLedger storage) == Nothing) $
            Left $ CustomTestError "Zero balance account was not removed after burning"

    it "fails to burn tokens if sender is not minter" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
        mgmNotMinter err

    it "fails to burn if sender has insufficient amount of tokens" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10, 10 ]
        mgmInsufficientBalance err

    it "burning tokens will not increase the minting allowance of the address doing the burning" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
        let mintings = [MintParam wallet1 10]
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings
        mgmAllowanceExceeded err

    it "fails if contract is paused" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
        mgmContractPaused err

  describe "Contract ownership" $ do
    it "transfers ownership properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        lExpectStorage @Storage stablecoinContract $ \storage ->
          when ((rOwner $ sRoles storage) /= wallet2) $
            Left $ CustomTestError "Owner was not changed"

    it "current contract owner retains its privileges if ownership weren't accepted yet" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        lExpectStorage @Storage stablecoinContract $ \storage ->
          when ((rOwner $ sRoles storage) /= testOwner) $
            Left $ CustomTestError "Owner was changed"

    it "transferring ownership fails if sender is not contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        mgmNotContractOwner err

    it "fails if previous contract owner tries to use ownership privileges" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        err <- expectError $ withSender testOwner $
          lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        mgmNotContractOwner err

    it "accepting ownership fails if sender is not pending contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        err <- expectError $ withSender wallet2 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        mgmNotPendingOwner err

    it "accepting ownership fails if pending owner is not set" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet2 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        mgmNoPendingOwnerSet err

    it "transfer ownership can be called multiple times each of which invalidates the previous call" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet3
        withSender wallet3 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        lExpectStorage @Storage stablecoinContract $ \storage ->
          when (rOwner (sRoles storage) /= wallet3) $
            Left $ CustomTestError "Owner was not changed"

    it "contract cannot retain ownership privileges if pending owner was changed by subsequent transfer ownership call" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender testOwner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        mgmNotPendingOwner err

    it "contract owner changes master minter properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet1
        lExpectStorage @Storage stablecoinContract $ \storage ->
          when (rMasterMinter (sRoles storage) /= wallet1) $
            Left $ CustomTestError "Master minter was not changed"

    it "contract owner changes contract pauser properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender testOwner $ lCallEP stablecoinContract (Call @"Change_pauser") wallet1
        lExpectStorage @Storage stablecoinContract $ \storage ->
          when (rPauser (sRoles storage) /= wallet1) $
            Left $ CustomTestError "Pauser was not changed"


  -- All successfull master minter capabilities are already tested
  describe "Master minter" $ do
    it "cannot change master minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet1
        mgmNotContractOwner err

    it "fails to change contract master minter if sender is not contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet1 $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet2
        mgmNotContractOwner err

    it "master minter cannot change contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        mgmNotContractOwner err

    it "master minter cannot change contract pauser" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testMasterMinter $ lCallEP stablecoinContract (Call @"Change_pauser") wallet1
        mgmNotContractOwner err


  -- Successfull contract pause test is already passed
  describe "Pauser" $ do
    it "changes contract pauser properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testPauser $ lCallEP stablecoinContract (Call @"Change_pauser") wallet1
        mgmNotContractOwner err

    it "fails to change contract pauser if sender is not contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender wallet1 $
          lCallEP stablecoinContract (Call @"Change_pauser") wallet2
        mgmNotContractOwner err

    it "pauser cannot change contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testPauser $
          lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        mgmNotContractOwner err

    it "pauser cannot change master minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        err <- expectError $ withSender testPauser $
          lCallEP stablecoinContract (Call @"Change_master_minter") wallet1
        mgmNotContractOwner err

    describe "Set_transferlist entrypoint" $ do
      let transferlistStorage = Transferlist.Storage
            { sTransfers = Set.empty
            , sReceivers = Set.fromList []
            }
      it "can set transferlist contract address in storage" $ integrationalTestExpectation $ do
        let originationParams = defaultOriginationParams
        transferlistContract <- unTAddress <$> lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender (opOwner originationParams) $
            lCallEP stablecoinContract (Call @"Set_transferlist") (Just transferlistContract)

          lExpectStorage @Storage stablecoinContract $ \storage ->
            case sTransferlistContract storage of
              Just addr
                | addr == transferlistContract -> Right ()
                | otherwise -> Left $ CustomTestError "Transferlist contract address was not set correctly"
              Nothing ->
                Left $ CustomTestError "Transferlist contract address was not set"

      it "can unset transferlist contract address in storage" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let originationParams = defaultOriginationParams {
              opTransferlistContract = Just transferlistContract
              }
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender (opOwner originationParams) $
            lCallEP stablecoinContract (Call @"Set_transferlist") Nothing

          lExpectStorage @Storage stablecoinContract $ \storage ->
            case sTransferlistContract storage of
              Just _ -> Left $ CustomTestError "Transferlist contract address was not unset"
              Nothing -> Right ()

      it "should fail if parameter of transferlist contract does not have the required entrypoints" $ integrationalTestExpectation $ do
        let originationParams = defaultOriginationParams
        withOriginated originate originationParams $ \stablecoinContract -> do
          err <- expectError $ withSender (opOwner originationParams) $
            lCallEP stablecoinContract (Call @"Set_transferlist") (Just wallet1)
          mgmBadTransferlist err

    describe "Transferlist contract interaction: fail behavior" $ do
      let transferlistStorage = Transferlist.Storage
            { sTransfers = Set.empty -- We want the transferlist contract to reject the operations. So we leave the whitelist empty
            , sReceivers = Set.empty
            }
      it "can make the transfer fail" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ defaultOriginationParams { opTransferlistContract = Just transferlistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do
          let
            transfers =
              [FA2.TransferParam wallet1 [FA2.TransferDestination wallet2 0 10]]

          err <- expectError $ withSender commonOperator $
            lCallEP stablecoinContract (Call @"Transfer") transfers

          lExpectAnyMichelsonFailed transferlistContract err

      it "can make mint operation fail" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addMinter (wallet1, 30)
              $ defaultOriginationParams { opTransferlistContract = Just transferlistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do
          let mintings = [MintParam wallet1 10]
          err <- expectError $ withSender wallet1 $
            lCallEP stablecoinContract (Call @"Mint") mintings

          lExpectAnyMichelsonFailed transferlistContract err

      it "can make burn operation fail" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let
          originationParams =
              addMinter (wallet1, 0)
            $ addAccount (wallet1, (commonOperators, 35))
            $ defaultOriginationParams { opTransferlistContract = Just transferlistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do
          err <- expectError $ withSender wallet1 $
            lCallEP stablecoinContract (Call @"Burn") [ 10 ]
          lExpectAnyMichelsonFailed transferlistContract err

    describe "Transferlist contract interaction: approve behavior" $ do
      let transferlistStorage = Transferlist.Storage
            { sTransfers = Set.fromList [(wallet1, wallet2)]
            , sReceivers = Set.fromList [wallet1, wallet2]
            }
      it "can approve transfers" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ defaultOriginationParams { opTransferlistContract = Just transferlistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do
          let
            transfers =
              [ FA2.TransferParam
                  { tpFrom = wallet1
                  , tpTxs = [FA2.TransferDestination { tdTo = wallet2, tdTokenId = 0, tdAmount = 10 }]
                  }
              ]

          withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfers

      it "can approve mint operation" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addMinter (wallet1, 30)
              $ defaultOriginationParams { opTransferlistContract = Just transferlistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do

          let mintings = [MintParam wallet1 10]
          withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

      it "can approve burn operation" $ integrationalTestExpectation $ do
        transferlistContract <- lOriginate Transferlist.transferlistContract "Transferlist test dummy" transferlistStorage (unsafeMkMutez 0)
        let
          originationParams =
              addMinter (wallet1, 0)
            $ addAccount (wallet1, (commonOperators, 35))
            $ defaultOriginationParams { opTransferlistContract = Just transferlistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
