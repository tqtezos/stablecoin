-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

-- | Tests for management entrypoints of stablecoin smart-contract

module Lorentz.Contracts.Test.Management
  ( managementSpec
  ) where

import Data.Map (fromList)
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it)

import qualified Indigo.Contracts.Safelist as Safelist
import Lorentz (mkView)
import Lorentz.Address
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin
import Lorentz.Contracts.Test.Common
import Lorentz.Test
import Tezos.Core (unsafeMkMutez)
import Util.Named

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
              { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed
              }
      withOriginated originate originationParams $ \stablecoinContract -> do
        lTransfer @param
          (#from .! commonOperator)
          (#to .! unTAddress stablecoinContract)
          (unsafeMkMutez 10) -- Error here
          (Call @"Transfer")
          -- Dummy transfer needed to call something from a contract since we don't have default entrypoint set
          (constructTransfersFromSender (#from_ .! wallet1) [])
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

  describe "Contract pausing" $ do
    it "pauses contract as expected" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender pauser $ lCallEP stablecoinContract (Call @"Pause") ()
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StoragePaused isPaused)
            | not isPaused ->
                Left $ CustomValidationError "Contract is not paused as was expected"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "cannot pause if sender does not have corresponding permissions" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Pause") ()
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "pause cannot be called multiple times in a row" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        lCallEP stablecoinContract (Call @"Pause") ()
        lCallEP stablecoinContract (Call @"Pause") ()
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "unpauses contract as expected" $ integrationalTestExpectation $ do
      let originationParams = defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender pauser $ lCallEP stablecoinContract (Call @"Unpause") ()
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StoragePaused isPaused)
            | isPaused ->
                Left $ CustomValidationError "Contract is paused which wasn't expected"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "cannot unpause if sender does not have corresponding permissions" $ integrationalTestExpectation $ do
      let originationParams = defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Unpause") ()
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "unpause cannot be called multiple times in a row" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        lCallEP stablecoinContract (Call @"Unpause") ()
        lCallEP stablecoinContract (Call @"Unpause") ()
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "prevents transfers while contract is paused" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ addAccount (wallet2, ([], 0))
          $ defaultOriginationParams
              { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed
              , opPaused = True
              }
      withOriginated originate originationParams $ \stablecoinContract -> do
        let transfers = constructSingleTransfer (#from_ .! wallet1) (#to_ .! wallet2) (#amount .! 10)
        withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfers
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "can successfully transfer tokens after contract unpause" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ addAccount (wallet2, (commonOperators, 0))
          $ addAccount (wallet3, (commonOperators, 0))
          $ defaultOriginationParams
              { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed
              , opPaused = True
              }

      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender pauser $ lCallEP stablecoinContract (Call @"Unpause") ()
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

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ (#owner .! wallet1, #token_id .! 0)
            , (#owner .! wallet2, #token_id .! 0)
            , (#owner .! wallet3, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet1, #token_id .! 0), #balance .! 0)
            , (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 0)
            , (#request .! (#owner .! wallet3, #token_id .! 0), #balance .! 10)
            ]

        lCallEP stablecoinContract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]


  describe "Configure minter" $ do
    it "configures minter properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam =
            ( #minter .! wallet1
            , ( #current_minting_allowance .! Nothing
              , #new_minting_allowance .! 30
              ))

          configureMinterParam1 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Nothing
              , #new_minting_allowance .! 20
              ))

        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        let
          configureMinterParam2 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Just 20
              , #new_minting_allowance .! 10
              ))

        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam2

        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageMinters minters)
            | minters /= expectedMinters  ->
                Left $ CustomValidationError "Configure_minter call produced a malformed minter list"
            | otherwise -> Right ()
            where
              expectedMinters = fromList [(wallet1, 30), (wallet2, 10)]
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "fails if expected and actual minting allowances do not match" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam1 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Nothing
              , #new_minting_allowance .! 20
              ))

        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        let
          configureMinterParam2 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Just 2000 -- Mismatched allowance here
              , #new_minting_allowance .! 10
              ))

        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam2

        validate . Left $  lExpectAnyMichelsonFailed stablecoinContract

    it "fails if minter is present in list of minters which was not expected" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam1 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Nothing
              , #new_minting_allowance .! 20
              ))

        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        let
          configureMinterParam2 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Nothing -- Here we expect for `wallet2` being non-present in minting allowances map
              , #new_minting_allowance .! 10
              ))

        withSender masterMinter $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam2

        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails if sender does not have master minter permissions" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let
          configureMinterParam1 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Nothing
              , #new_minting_allowance .! 20
              ))

        withSender wallet2 $ lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        validate . Left $  lExpectAnyMichelsonFailed stablecoinContract

    it "fails if contract is paused" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        lCallEP stablecoinContract (Call @"Pause") ()

        let
          configureMinterParam1 =
            ( #minter .! wallet2
            , ( #current_minting_allowance .! Nothing
              , #new_minting_allowance .! 20
              ))

        lCallEP stablecoinContract (Call @"Configure_minter") configureMinterParam1

        validate . Left $  lExpectAnyMichelsonFailed stablecoinContract



  describe "Remove minter" $ do
    it "successfully removes minter from minting list" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 10)
          $ addMinter (wallet2, 0)
          $ addMinter (wallet3, 100)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet2
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageMinters minters)
            | minters /= expectedMinters  ->
                Left $ CustomValidationError "Remove minter does not change minter list"
            | otherwise -> Right ()
            where
              expectedMinters = fromList [(wallet3, 100)]
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "fails if sender is not master minter" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "cannot remove the same wallet if it's already removed" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "cannot remove non-minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Remove_minter") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract



  describe "Minting" $ do
    it "successfully mints tokens" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 30)
          $ defaultOriginationParams
              { opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed }
      withOriginated originate originationParams $ \stablecoinContract -> do
        let
          mintings =
            [ (#to_ .! wallet1, #amount .! 10)
            , (#to_ .! wallet2, #amount .! 5)
            , (#to_ .! wallet3, #amount .! 15)
            ]

        withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ (#owner .! wallet1, #token_id .! 0)
            , (#owner .! wallet2, #token_id .! 0)
            , (#owner .! wallet3, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet1, #token_id .! 0), #balance .! 10)
            , (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 5)
            , (#request .! (#owner .! wallet3, #token_id .! 0), #balance .! 15)
            ]

        lCallEP stablecoinContract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

    it "aborts whole transaction if the sum of minting tokens at a given step exceeds current minting allowance" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 10)
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        let
          mintings =
            [ (#to_ .! wallet1, #amount .! 5)
            , (#to_ .! wallet2, #amount .! 10) -- Error here
            , (#to_ .! wallet3, #amount .! 5)
            ]

        withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails if sender is not minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        let mintings = [(#to_ .! wallet1, #amount .! 5)]
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails minting if contract is paused" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 10)
          $ defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        let mintings = [(#to_ .! wallet1, #amount .! 5)]
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract



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

        consumer <- lOriginateEmpty @[BalanceResponseItem] contractConsumer "consumer"
        let
          balanceRequestItems =
            [ (#owner .! wallet1, #token_id .! 0)
            , (#owner .! wallet2, #token_id .! 0)
            ]
          balanceRequest = mkView (#requests .! balanceRequestItems) consumer
          balanceExpected =
            [ (#request .! (#owner .! wallet1, #token_id .! 0), #balance .! 5)
            , (#request .! (#owner .! wallet2, #token_id .! 0), #balance .! 0)
            ]

        lCallEP stablecoinContract (Call @"Balance_of") balanceRequest

        validate . Right $
          lExpectViewConsumerStorage consumer [balanceExpected]

    it "fails to burn tokens if sender is not minter" $ integrationalTestExpectation $ do
      let
        originationParams =
            addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails to burn if sender has insufficient amount of tokens" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10, 10 ]
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "burning tokens will not increase the minting allowance of the address doing the burning" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
        let mintings = [(#to_ .! wallet1, #amount .! 10)]
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails if contract is paused" $ integrationalTestExpectation $ do
      let
        originationParams =
            addMinter (wallet1, 0)
          $ addAccount (wallet1, (commonOperators, 10))
          $ defaultOriginationParams { opPaused = True }
      withOriginated originate originationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

  describe "Contract ownership" $ do
    it "transfers ownership properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageRoles (OwnerRole currentOwner))
            | currentOwner /= wallet2 -> Left $
                CustomValidationError "Owner was not changed"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "current contract owner retains its privileges if ownership weren't accepted yet" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageRoles (OwnerRole currentOwner))
            | currentOwner /= owner -> Left $
                CustomValidationError "Owner was changed"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "transferring ownership fails if sender is not contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails if previous contract owner tries to use ownership privileges" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "accepting ownership fails if sender is not pending contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender wallet2 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "transfer ownership can be called multiple times each of which invalidates the previous call" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet3
        withSender wallet3 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageRoles (OwnerRole currentOwner))
            | currentOwner /= wallet3 -> Left $
                CustomValidationError "Owner was not changed"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "contract cannot retain ownership privileges if pending owner was changed by subsequent transfer ownership call" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        withSender owner $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Accept_ownership") ()
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "contract owner changes master minter properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet1
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageRoles (MasterMinterRole currentMasterMinter))
            | currentMasterMinter /= wallet1 -> Left $
                CustomValidationError "Master minter was not changed"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

    it "contract owner changes contract pauser properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender owner $ lCallEP stablecoinContract (Call @"Change_pauser") wallet1
        validate . Right . lExpectStorage stablecoinContract $ \case
          (StorageRoles (PauserRole currentPauser))
            | currentPauser /= wallet1 -> Left $
                CustomValidationError "Pauser was not changed"
            | otherwise -> Right ()
          _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"


  -- All successfull master minter capabilities are already tested
  describe "Master minter" $ do
    it "cannot change master minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails to change contract master minter if sender is not master minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet2
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "master minter cannot change contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "master minter cannot change contract pauser" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender masterMinter $ lCallEP stablecoinContract (Call @"Change_pauser") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract


  -- Successfull contract pause test is already passed
  describe "Pauser" $ do
    it "changes contract pauser properly" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender pauser $ lCallEP stablecoinContract (Call @"Change_pauser") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "fails to change contract pauser if sender is not contract pauser" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender wallet1 $ lCallEP stablecoinContract (Call @"Change_pauser") wallet2
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "pauser cannot change contract owner" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender pauser $ lCallEP stablecoinContract (Call @"Transfer_ownership") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    it "pauser cannot change master minter" $ integrationalTestExpectation $ do
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        withSender pauser $ lCallEP stablecoinContract (Call @"Change_master_minter") wallet1
        validate . Left $ lExpectAnyMichelsonFailed stablecoinContract

    describe "Set_safelist entrypoint" $ do
      let safelistStorage = Safelist.Storage
            { sTransfers = Set.empty
            , sReceivers = Set.fromList []
            }
      it "can set safelist contract address in storage" $ integrationalTestExpectation $ do
        let originationParams = defaultOriginationParams
        safelistContract <- unTAddress <$> lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender (opOwner originationParams) $
            lCallEP stablecoinContract (Call @"Set_safelist") (Just safelistContract)

          validate . Right . lExpectStorage stablecoinContract $ \case
            StorageSafelistContract (Just addr)
              | addr == safelistContract -> Right ()
              | otherwise -> Left $ CustomValidationError "Safelist contract address was not set"
            _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"

      it "can unset safelist contract address in storage" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let originationParams = defaultOriginationParams {
              opSafelistContract = Just safelistContract
              }
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender (opOwner originationParams) $
            lCallEP stablecoinContract (Call @"Set_safelist") Nothing

          validate . Right . lExpectStorage stablecoinContract $ \case
            StorageSafelistContract (Just _) -> Left $ CustomValidationError "Safelist contract address was not unset"
            StorageSafelistContract Nothing -> Right ()
            _ -> Left $ CustomValidationError "Token produced malformed storage, this should not have happened"


    describe "Safelist contract interaction: fail behavior" $ do
      let safelistStorage = Safelist.Storage
            { sTransfers = Set.empty -- We want the safelist contract to reject the operations. So we leave the whitelist empty
            , sReceivers = Set.empty
            }
      it "can make the transfer fail" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ defaultOriginationParams {
                  opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed,
                  opSafelistContract = Just safelistContract
                }
        withOriginated originate originationParams $ \stablecoinContract -> do
          let
            transfers =
              [(#from_ .! wallet1, #txs .! [(#to_ .! wallet2, (#token_id .! 0, #amount .! 10))])]

          withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfers

          validate $ Left (lExpectAnyMichelsonFailed safelistContract)

      it "can make mint operation fail" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addMinter (wallet1, 30)
              $ defaultOriginationParams {
                  opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed,
                  opSafelistContract = Just safelistContract
                }
        withOriginated originate originationParams $ \stablecoinContract -> do
          let
            mintings =
              [ (#to_ .! wallet1, #amount .! 10)
              ]

          withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

          validate $ Left (lExpectAnyMichelsonFailed safelistContract)

      it "can make burn operation fail" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let
          originationParams =
              addMinter (wallet1, 0)
            $ addAccount (wallet1, (commonOperators, 35))
            $ defaultOriginationParams { opSafelistContract = Just safelistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do

          withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]

          validate $ Left (lExpectAnyMichelsonFailed safelistContract)

    describe "Safelist contract interaction: approve behavior" $ do
      let safelistStorage = Safelist.Storage
            { sTransfers = Set.fromList [(wallet1, wallet2)]
            , sReceivers = Set.fromList [wallet1, wallet2]
            }
      it "can approve transfers" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ defaultOriginationParams {
                  opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed,
                  opSafelistContract = Just safelistContract
                }
        withOriginated originate originationParams $ \stablecoinContract -> do
          let
            transfers =
              [(#from_ .! wallet1, #txs .! [(#to_ .! wallet2, (#token_id .! 0, #amount .! 10))])]

          withSender commonOperator $ lCallEP stablecoinContract (Call @"Transfer") transfers

          validate (Right expectAnySuccess)

      it "can approve mint operation" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let
          originationParams = addAccount (wallet1, (commonOperators, 10))
              $ addMinter (wallet1, 30)
              $ defaultOriginationParams {
                  opPermissionsDescriptor = permissionDescriptorOperatorTransferAllowed,
                  opSafelistContract = Just safelistContract
                }
        withOriginated originate originationParams $ \stablecoinContract -> do

          let
            mintings =
              [ (#to_ .! wallet1, #amount .! 10)
              ]

          withSender wallet1 $ lCallEP stablecoinContract (Call @"Mint") mintings

          validate (Right expectAnySuccess)

      it "can approve burn operation" $ integrationalTestExpectation $ do
        safelistContract <- lOriginate Safelist.safelistContract "Safelist test dummy" safelistStorage (unsafeMkMutez 0)
        let
          originationParams =
              addMinter (wallet1, 0)
            $ addAccount (wallet1, (commonOperators, 35))
            $ defaultOriginationParams { opSafelistContract = Just safelistContract }
        withOriginated originate originationParams $ \stablecoinContract -> do

          withSender wallet1 $ lCallEP stablecoinContract (Call @"Burn") [ 10 ]

          validate (Right expectAnySuccess)

  -- Permission descriptor query
  describe "Contract's Permissions_descriptor entrypoint" $
    it "returns the expected value" $ integrationalTestExpectation $
      withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
        consumer <- lOriginateEmpty @PermissionsDescriptor contractConsumer "consumer"
        let permissionsDescriptorQuery = toContractRef consumer
        lCallEP stablecoinContract (Call @"Permissions_descriptor") permissionsDescriptorQuery

        validate . Right $
          lExpectConsumerStorage consumer $ \case
            (pd:_) -> if mkPermissionDescriptor pd == stablecoinPermissionsDescriptor
              then Right ()
              else Left $ CustomValidationError "Unexpected permission descriptor"
            _ -> Left $ CustomValidationError "Unexpected permission descriptor"
