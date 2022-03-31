-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

-- | Tests for management entrypoints of stablecoin smart-contract
module Lorentz.Contracts.Test.Management
  ( managementSpec
  , mgmNotContractOwner
  , mgmNotPendingOwner
  , mgmNotPauser
  , mgmNotMasterMinter
  ) where

import Lorentz (mt)

import Data.Map as M (fromList)
import Data.Map.Strict as M (size)
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)

import Lorentz.Address
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Tezos.Address (detGenKeyAddress)
import Morley.Util.Named
import Test.Cleveland hiding (originate)
import Test.Cleveland.Lorentz.Consumer

import qualified Indigo.Contracts.Transferlist.Internal as Transferlist
import Lorentz.Contracts.Stablecoin hiding (stablecoinContract)
import Lorentz.Contracts.Test.Common

import Lorentz.Contracts.Test.AsRPC ()

mgmXtzReceived
  , mgmNotContractOwner
  , mgmNotPendingOwner
  , mgmNoPendingOwnerSet
  , mgmNotPauser
  , mgmNotMasterMinter
  , mgmNotMinter
  , mgmContractNotPaused
  , mgmInsufficientBalance
  , mgmCurrentAllowanceRequired
  , mgmAllowanceMismatch
  , mgmAddrNotMinter
  , mgmAllowanceExceeded
  , mgmBadTransferlist
  , mgmMinterLimitExceeded
  :: MonadCleveland caps m => m () -> m ()
mgmXtzReceived = expectFailedWith [mt|XTZ_RECEIVED|]
mgmNotContractOwner = expectFailedWith [mt|NOT_CONTRACT_OWNER|]
mgmNotPendingOwner = expectFailedWith [mt|NOT_PENDING_OWNER|]
mgmNoPendingOwnerSet = expectFailedWith [mt|NO_PENDING_OWNER_SET|]
mgmNotPauser = expectFailedWith [mt|NOT_PAUSER|]
mgmNotMasterMinter = expectFailedWith [mt|NOT_MASTER_MINTER|]
mgmNotMinter = expectFailedWith [mt|NOT_MINTER|]
mgmContractNotPaused = expectFailedWith [mt|CONTRACT_NOT_PAUSED|]
mgmInsufficientBalance = expectFailedWith [mt|FA2_INSUFFICIENT_BALANCE|]
mgmCurrentAllowanceRequired = expectFailedWith [mt|CURRENT_ALLOWANCE_REQUIRED|]
mgmAllowanceMismatch = expectFailedWith [mt|ALLOWANCE_MISMATCH|]
mgmAddrNotMinter = expectFailedWith [mt|ADDR_NOT_MINTER|]
mgmAllowanceExceeded = expectFailedWith [mt|ALLOWANCE_EXCEEDED|]
mgmBadTransferlist = expectFailedWith [mt|BAD_TRANSFERLIST|]
mgmMinterLimitExceeded = expectFailedWith [mt|MINTER_LIMIT_REACHED|]

managementSpec
  :: forall param st.
     ParameterC param
  => (forall caps m. MonadCleveland caps m => OriginationFn param st m)
  -> [TestTree]
managementSpec originate =
  [ testGroup "Contract meta" $
      [ testScenario "fails if contract is received non-zero amount of xtz" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 0)) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
            stablecoinContract <- originate originationParams
            do
              mgmXtzReceived $
                withSender commonOperator $
                  transfer
                    TransferData
                      { tdTo = stablecoinContract
                      , tdAmount = 10 -- Error here
                      , tdEntrypoint = ep "transfer"
                      , -- Dummy transfer needed to call something from a contract since we don't have default entrypoint set
                        tdParameter = constructTransfersFromSender (#from_ :! wallet1) []
                      }
      ]
  , testGroup "Contract pausing" $
      [ testScenario "pauses contract as expected" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testPauser $ call stablecoinContract (Call @"Pause") ()
              storage <- getStorage @Storage (chAddress stablecoinContract)
              unless (sPausedRPC storage) $ failure "Contract is not paused as was expected"
      , testScenario "cannot pause if sender does not have corresponding permissions" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotPauser $ withSender wallet2 $ call stablecoinContract (Call @"Pause") ()
      , testScenario "pause cannot be called multiple times in a row" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testPauser $ do
                call stablecoinContract (Call @"Pause") ()
                mgmContractPaused $ call stablecoinContract (Call @"Pause") ()
      , testScenario "unpauses contract as expected" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            let originationParams =
                  ( defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
                  )
                    { opPaused = True
                    }
            stablecoinContract <- originate originationParams
            do
              withSender testPauser $ call stablecoinContract (Call @"Unpause") ()
              storage <- getStorage @Storage (chAddress stablecoinContract)
              when (sPausedRPC storage) $ failure "Contract is paused which wasn't expected"
      , testScenario "cannot unpause if sender does not have corresponding permissions" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            let originationParams =
                  ( defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
                  )
                    { opPaused = True
                    }
            stablecoinContract <- originate originationParams
            do
              mgmNotPauser $ withSender wallet2 $ call stablecoinContract (Call @"Unpause") ()
      , testScenario "unpause cannot be called multiple times in a row" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            let originationParams =
                  ( defaultOriginationParams
                      (#owner :! testOwner)
                      (#pauser :! testPauser)
                      (#masterMinter :! testMasterMinter)
                  )
                    { opPaused = True
                    }
            stablecoinContract <- originate originationParams
            do
              withSender testPauser $ call stablecoinContract (Call @"Unpause") ()
              mgmContractNotPaused $ withSender testPauser $ call stablecoinContract (Call @"Unpause") ()
      , testScenario "prevents transfers while contract is paused" $
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
                    addAccount (wallet2, ([], 0)) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                        { opPaused = True
                        }
            stablecoinContract <- originate originationParams
            do
              let transfers = constructSingleTransfer (#from_ :! wallet1) (#to_ :! wallet2) (#amount :! 10)
              mgmContractPaused $ withSender commonOperator $ call stablecoinContract (Call @"Transfer") transfers
      , testScenario "can successfully transfer tokens after contract unpause" $
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
                    addAccount (wallet2, (commonOperators, 0)) $
                      addAccount (wallet3, (commonOperators, 0)) $
                        ( defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
                        )
                          { opPaused = True
                          }

            stablecoinContract <- originate originationParams
            do
              withSender testPauser $ call stablecoinContract (Call @"Unpause") ()
              let transfer1 =
                    constructTransfersFromSender
                      (#from_ :! wallet1)
                      [ (#to_ :! wallet2, #amount :! 5)
                      , (#to_ :! wallet2, #amount :! 5)
                      ]

                  transfer2 =
                    constructSingleTransfer
                      (#from_ :! wallet2)
                      (#to_ :! wallet3)
                      (#amount :! 10)

              withSender commonOperator $ call stablecoinContract (Call @"Transfer") transfer1
              withSender commonOperator $ call stablecoinContract (Call @"Transfer") transfer2

              consumer <- originateSimple @[FA2.BalanceResponseItem] "consumer" [] contractConsumer
              let balanceRequestItems =
                    [ FA2.BalanceRequestItem wallet1 FA2.theTokenId
                    , FA2.BalanceRequestItem wallet2 FA2.theTokenId
                    , FA2.BalanceRequestItem wallet3 FA2.theTokenId
                    ]
                  balanceRequest = FA2.mkFA2View balanceRequestItems consumer
                  balanceExpected =
                    [ FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet1 FA2.theTokenId
                        , briBalance = 0
                        }
                    , FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet2 FA2.theTokenId
                        , briBalance = 0
                        }
                    , FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet3 FA2.theTokenId
                        , briBalance = 10
                        }
                    ]

              call stablecoinContract (Call @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]
      ]
  , testGroup "Configure minter" $
      [ testScenario "configures minter properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              let configureMinterParam =
                    ConfigureMinterParam
                      { cmpMinter = wallet1
                      , cmpCurrentMintingAllowance = Nothing
                      , cmpNewMintingAllowance = 30
                      }

                  configureMinterParam1 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Nothing
                      , cmpNewMintingAllowance = 20
                      }

              withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam
              withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam1

              let configureMinterParam2 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Just 20
                      , cmpNewMintingAllowance = 10
                      }

              withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam2

              storage <- getStorage @Storage (chAddress stablecoinContract)
              let expectedMinters = fromList [(wallet1, 30), (wallet2, 10)]
               in when (sMintingAllowancesRPC storage /= expectedMinters) $
                    failure "Configure_minter call produced a malformed minter list"
      , testScenario "fails if expected and actual minting allowances do not match" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              let configureMinterParam1 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Nothing
                      , cmpNewMintingAllowance = 20
                      }

              withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam1

              let configureMinterParam2 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Just 2000 -- Mismatched allowance here
                      , cmpNewMintingAllowance = 10
                      }

              mgmAllowanceMismatch $ withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam2
      , testScenario "fails if minter is present in list of minters which was not expected" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              let configureMinterParam1 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Nothing
                      , cmpNewMintingAllowance = 20
                      }

              withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam1

              let configureMinterParam2 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Nothing -- Here we expect for `wallet2` being non-present in minting allowances map
                      , cmpNewMintingAllowance = 10
                      }

              mgmCurrentAllowanceRequired $ withSender testMasterMinter $ call stablecoinContract (Call @"Configure_minter") configureMinterParam2
      , testScenario "fails if sender does not have master minter permissions" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              let configureMinterParam1 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Nothing
                      , cmpNewMintingAllowance = 20
                      }

              mgmNotMasterMinter $ withSender wallet2 $ call stablecoinContract (Call @"Configure_minter") configureMinterParam1
      , testScenario "fails if contract is paused" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testPauser $ call stablecoinContract (Call @"Pause") ()

              let configureMinterParam1 =
                    ConfigureMinterParam
                      { cmpMinter = wallet2
                      , cmpCurrentMintingAllowance = Nothing
                      , cmpNewMintingAllowance = 20
                      }

              mgmContractPaused $ call stablecoinContract (Call @"Configure_minter") configureMinterParam1
      ]
  , let configureMinterParam :: Int -> ConfigureMinterParam
        configureMinterParam i =
          ConfigureMinterParam
            { cmpMinter = detGenKeyAddress $ encodeUtf8 @Text ("a" <> show i)
            , cmpCurrentMintingAllowance = Nothing
            , cmpNewMintingAllowance = 20
            }
     in testGroup "Minter limit check" $
          [ testScenario "Can add minter until minter limit" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                stablecoinContract <-
                  originate $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
                do
                  withSender testMasterMinter $ do
                    mapM_ (call stablecoinContract (Call @"Configure_minter")) (configureMinterParam <$> [1 .. minterLimit])

                    storage <- getStorage @Storage (chAddress stablecoinContract)
                    unless (M.size (sMintingAllowancesRPC storage) == minterLimit) $
                      failure "Configure_minter call produced a malformed minter list"
          , testScenario "Throws error when minter limit is exceeded" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                stablecoinContract <-
                  originate $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
                do
                  withSender testMasterMinter $ do
                    mgmMinterLimitExceeded $ mapM_ (call stablecoinContract (Call @"Configure_minter")) (configureMinterParam <$> [1 .. (minterLimit + 1)])
          ]
  , testGroup "Remove minter" $
      [ testScenario "successfully removes minter from minting list" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            let originationParams =
                  addMinter (wallet1, 10) $
                    addMinter (wallet2, 0) $
                      addMinter (wallet3, 100) $
                        ( defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
                        )
            stablecoinContract <- originate originationParams
            do
              withSender testMasterMinter $ call stablecoinContract (Call @"Remove_minter") wallet1
              withSender testMasterMinter $ call stablecoinContract (Call @"Remove_minter") wallet2
              storage <- getStorage @Storage (chAddress stablecoinContract)
              let expectedMinters = fromList [(wallet3, 100)]
               in when (sMintingAllowancesRPC storage /= expectedMinters) $
                    failure "Remove minter does not change minter list"
      , testScenario "fails if sender is not master minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let originationParams =
                  addMinter (wallet1, 0) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
            stablecoinContract <- originate originationParams
            do
              mgmNotMasterMinter $ withSender wallet2 $ call stablecoinContract (Call @"Remove_minter") wallet1
      , testScenario "cannot remove the same wallet if testScenario's already removed" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            let originationParams =
                  addMinter (wallet1, 0) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
            stablecoinContract <- originate originationParams
            do
              withSender testMasterMinter $ call stablecoinContract (Call @"Remove_minter") wallet1
              mgmAddrNotMinter $ withSender testMasterMinter $ call stablecoinContract (Call @"Remove_minter") wallet1
      , testScenario "cannot remove non-minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmAddrNotMinter $ withSender testMasterMinter $ call stablecoinContract (Call @"Remove_minter") wallet1
      ]
  , testGroup "Minting" $
      [ testScenario "successfully mints tokens and updates total_supply" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            let originationParams =
                  addMinter (wallet1, 30) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
            stablecoinContract <- originate originationParams
            do
              let mintings =
                    [ MintParam wallet1 10
                    , MintParam wallet2 5
                    , MintParam wallet3 15
                    ]

              withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings

              consumer <- originateSimple @[FA2.BalanceResponseItem] "consumer" [] contractConsumer
              let balanceRequestItems =
                    [ FA2.BalanceRequestItem wallet1 FA2.theTokenId
                    , FA2.BalanceRequestItem wallet2 FA2.theTokenId
                    , FA2.BalanceRequestItem wallet3 FA2.theTokenId
                    ]
                  balanceRequest = FA2.mkFA2View balanceRequestItems consumer
                  balanceExpected =
                    [ FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet1 FA2.theTokenId
                        , briBalance = 10
                        }
                    , FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet2 FA2.theTokenId
                        , briBalance = 5
                        }
                    , FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet3 FA2.theTokenId
                        , briBalance = 15
                        }
                    ]

              call stablecoinContract (Call @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]

              storage <- getStorage @Storage (chAddress stablecoinContract)
              unless (sTotalSupplyRPC storage == 30) $ -- Started with 0 tokens and minted 30 tokens so total supply should be 30
                failure "Total supply was not updated as expected"
      , testScenario "aborts whole transaction if the sum of minting tokens at a given step exceeds current minting allowance" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            let originationParams =
                  addMinter (wallet1, 10) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
            stablecoinContract <- originate originationParams
            do
              let mintings =
                    [ MintParam wallet1 5
                    , MintParam wallet2 10 -- Error here
                    , MintParam wallet3 5
                    ]

              expectFailedWithAny $ withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings
      , testScenario "fails if sender is not minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              let mintings = [MintParam wallet1 5]
              expectFailedWithAny $ withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings
      , testScenario "fails minting if contract is paused" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            let originationParams =
                  addMinter (wallet1, 10) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
                      { opPaused = True
                      }
            stablecoinContract <- originate originationParams
            do
              let mintings = [MintParam wallet1 5]
              mgmContractPaused $ withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings
      ]
  , testGroup "Burning" $
      [ testScenario "burns tokens and updates total_supply as expected" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addMinter (wallet1, 0) $
                    addMinter (wallet2, 0) $
                      addAccount (wallet1, (commonOperators, 35)) $
                        addAccount (wallet2, (commonOperators, 0)) $
                          ( defaultOriginationParams
                              (#owner :! testOwner)
                              (#pauser :! testPauser)
                              (#masterMinter :! testMasterMinter)
                          )
            stablecoinContract <- originate originationParams
            do
              withSender wallet1 $ call stablecoinContract (Call @"Burn") [10, 20]
              withSender wallet2 $ call stablecoinContract (Call @"Burn") [0]

              consumer <- originateSimple @[FA2.BalanceResponseItem] "consumer" [] contractConsumer
              let balanceRequestItems =
                    [ FA2.BalanceRequestItem wallet1 FA2.theTokenId
                    , FA2.BalanceRequestItem wallet2 FA2.theTokenId
                    ]
                  balanceRequest = FA2.mkFA2View balanceRequestItems consumer
                  balanceExpected =
                    [ FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet1 FA2.theTokenId
                        , briBalance = 5
                        }
                    , FA2.BalanceResponseItem
                        { briRequest = FA2.BalanceRequestItem wallet2 FA2.theTokenId
                        , briBalance = 0
                        }
                    ]

              call stablecoinContract (Call @"Balance_of") balanceRequest

              getStorage consumer @@== [balanceExpected]

              storage <- getStorage @Storage (chAddress stablecoinContract)
              unless (sTotalSupplyRPC storage == 5) $ -- Started with 35 tokens and burned 30 tokens so total supply should be 5
                failure "Total supply was not updated as expected"
      , testScenario "removes account if balance after burning is zero" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addMinter (wallet1, 0) $
                    addAccount (wallet1, (commonOperators, 35)) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
            stablecoinContract <- originate originationParams
            do
              withSender wallet1 $ call stablecoinContract (Call @"Burn") [35]

              storage <- getStorage @Storage (chAddress stablecoinContract)
              val <- getBigMapValueMaybe (sLedgerRPC storage) wallet1
              unless (isNothing val) $
                failure "Zero balance account was not removed after burning"
      , testScenario "fails to burn tokens if sender is not minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
            stablecoinContract <- originate originationParams
            do
              mgmNotMinter $ withSender wallet1 $ call stablecoinContract (Call @"Burn") [10]
      , testScenario "fails to burn if sender has insufficient amount of tokens" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addMinter (wallet1, 0) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
            stablecoinContract <- originate originationParams
            do
              mgmInsufficientBalance $ withSender wallet1 $ call stablecoinContract (Call @"Burn") [10, 10]
      , testScenario "burning tokens will not increase the minting allowance of the address doing the burning" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addMinter (wallet1, 0) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
            stablecoinContract <- originate originationParams
            do
              withSender wallet1 $ call stablecoinContract (Call @"Burn") [10]
              let mintings = [MintParam wallet1 10]
              mgmAllowanceExceeded $ withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings
      , testScenario "fails if contract is paused" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            let originationParams =
                  addMinter (wallet1, 0) $
                    addAccount (wallet1, (commonOperators, 10)) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                        { opPaused = True
                        }
            stablecoinContract <- originate originationParams
            do
              mgmContractPaused $ withSender wallet1 $ call stablecoinContract (Call @"Burn") [10]
      ]
  , testGroup "Contract ownership" $
      [ testScenario "transfers ownership properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet1
              withSender wallet1 $ call stablecoinContract (Call @"Accept_ownership") ()
              withSender wallet1 $ call stablecoinContract (Call @"Transfer_ownership") wallet2
              withSender wallet2 $ call stablecoinContract (Call @"Accept_ownership") ()
              storage <- getStorage @Storage (chAddress stablecoinContract)
              when ((rOwner $ sRolesRPC storage) /= wallet2) $
                failure "Owner was not changed"
      , testScenario "current contract owner retains its privileges if ownership weren't accepted yet" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet1
              storage <- getStorage @Storage (chAddress stablecoinContract)
              when ((rOwner $ sRolesRPC storage) /= testOwner) $
                failure "Owner was changed"
      , testScenario "transferring ownership fails if sender is not contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender wallet1 $ call stablecoinContract (Call @"Transfer_ownership") wallet1
      , testScenario "fails if previous contract owner tries to use ownership privileges" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet1
              withSender wallet1 $ call stablecoinContract (Call @"Accept_ownership") ()
              mgmNotContractOwner $ withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet2
      , testScenario "accepting ownership fails if sender is not pending contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet1
              mgmNotPendingOwner $ withSender wallet2 $ call stablecoinContract (Call @"Accept_ownership") ()
      , testScenario "accepting ownership fails if pending owner is not set" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNoPendingOwnerSet $ withSender wallet2 $ call stablecoinContract (Call @"Accept_ownership") ()
      , testScenario "transfer ownership can be called multiple times each of which invalidates the previous call" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet1
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet2
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet3
              withSender wallet3 $ call stablecoinContract (Call @"Accept_ownership") ()
              storage <- getStorage @Storage (chAddress stablecoinContract)
              when (rOwner (sRolesRPC storage) /= wallet3) $
                failure "Owner was not changed"
      , testScenario "contract cannot retain ownership privileges if pending owner was changed by subsequent transfer ownership call" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet1
              withSender testOwner $ call stablecoinContract (Call @"Transfer_ownership") wallet2
              mgmNotPendingOwner $ withSender wallet1 $ call stablecoinContract (Call @"Accept_ownership") ()
      , testScenario "contract owner changes master minter properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Change_master_minter") wallet1
              storage <- getStorage @Storage (chAddress stablecoinContract)
              when (rMasterMinter (sRolesRPC storage) /= wallet1) $
                failure "Master minter was not changed"
      , testScenario "contract owner changes contract pauser properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testOwner $ call stablecoinContract (Call @"Change_pauser") wallet1
              storage <- getStorage @Storage (chAddress stablecoinContract)
              when (rPauser (sRolesRPC storage) /= wallet1) $
                failure "Pauser was not changed"

                -- All successfull master minter capabilities are already tested
      ]
  , testGroup "Master minter" $
      [ testScenario "cannot change master minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender testMasterMinter $ call stablecoinContract (Call @"Change_master_minter") wallet1
      , testScenario "fails to change contract master minter if sender is not contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender wallet1 $ call stablecoinContract (Call @"Change_master_minter") wallet2
      , testScenario "master minter cannot change contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender testMasterMinter $ call stablecoinContract (Call @"Transfer_ownership") wallet1
      , testScenario "master minter cannot change contract pauser" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender testMasterMinter $ call stablecoinContract (Call @"Change_pauser") wallet1

              -- Successfull contract pause test is already passed
      ]
  , testGroup "Pauser" $
      [ testScenario "changes contract pauser properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender testPauser $ call stablecoinContract (Call @"Change_pauser") wallet1
      , testScenario "fails to change contract pauser if sender is not contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender wallet1 $ call stablecoinContract (Call @"Change_pauser") wallet2
      , testScenario "pauser cannot change contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender testPauser $ call stablecoinContract (Call @"Transfer_ownership") wallet1
      , testScenario "pauser cannot change master minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originate $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              mgmNotContractOwner $ withSender testPauser $ call stablecoinContract (Call @"Change_master_minter") wallet1
      ]
  , let transferlistStorage =
          Transferlist.Storage
            { sTransfers = Set.empty
            , sReceivers = Set.fromList []
            }
     in testGroup "Set_transferlist entrypoint" $
          [ testScenario "can set transferlist contract address in storage" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                let originationParams =
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                stablecoinContract <- originate originationParams
                do
                  withSender (opOwner originationParams) $
                    call stablecoinContract (Call @"Set_transferlist") ((Just $ chAddress transferlistContract))

                  storage <- getStorage @Storage (chAddress stablecoinContract)
                  case sTransferlistContractRPC storage of
                    Just addr
                      | addr == chAddress transferlistContract -> pass
                      | otherwise -> failure "Transferlist contract address was not set correctly"
                    Nothing ->
                      failure "Transferlist contract address was not set"
          , testScenario "can unset transferlist contract address in storage" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                let originationParams =
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                        { opTransferlistContract = (Just $ toTAddress transferlistContract)
                        }
                stablecoinContract <- originate originationParams
                do
                  withSender (opOwner originationParams) $
                    call stablecoinContract (Call @"Set_transferlist") Nothing

                  storage <- getStorage @Storage (chAddress stablecoinContract)
                  case sTransferlistContractRPC storage of
                    Just _ -> failure "Transferlist contract address was not unset"
                    Nothing -> pass
          , testScenario "should fail if parameter of transferlist contract does not have the required entrypoints" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                let originationParams =
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                stablecoinContract <- originate originationParams
                do
                  mgmBadTransferlist $ withSender (opOwner originationParams) $ call stablecoinContract (Call @"Set_transferlist") (Just wallet1)
          ]
  , let transferlistStorage =
          Transferlist.Storage
            { sTransfers = Set.empty -- We want the transferlist contract to reject the operations. So we leave the whitelist empty
            , sReceivers = Set.empty
            }
     in testGroup "Transferlist contract interaction: fail behavior" $
          [ testScenario "can make the transfer fail" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                wallet2 <- newAddress "wallet2"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        ( defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
                        )
                          { opTransferlistContract = (Just $ toTAddress transferlistContract)
                          }
                stablecoinContract <- originate originationParams
                do
                  let transfers =
                        [FA2.TransferItem wallet1 [FA2.TransferDestination wallet2 FA2.theTokenId 10]]

                  expectFailedWithAny $ withSender commonOperator $ call stablecoinContract (Call @"Transfer") transfers
          , testScenario "can make mint operation fail" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        addMinter (wallet1, 30) $
                          ( defaultOriginationParams
                              (#owner :! testOwner)
                              (#pauser :! testPauser)
                              (#masterMinter :! testMasterMinter)
                          )
                            { opTransferlistContract = (Just $ toTAddress transferlistContract)
                            }
                stablecoinContract <- originate originationParams
                do
                  let mintings = [MintParam wallet1 10]
                  expectFailedWithAny $ withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings
          , testScenario "can make burn operation fail" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                let originationParams =
                      addMinter (wallet1, 0) $
                        addAccount (wallet1, (commonOperators, 35)) $
                          ( defaultOriginationParams
                              (#owner :! testOwner)
                              (#pauser :! testPauser)
                              (#masterMinter :! testMasterMinter)
                          )
                            { opTransferlistContract = (Just $ toTAddress transferlistContract)
                            }
                stablecoinContract <- originate originationParams
                do
                  expectFailedWithAny $ withSender wallet1 $ call stablecoinContract (Call @"Burn") [10]
          ]
  , testGroup "Transferlist contract interaction: approve behavior" $
      [ testScenario "can approve transfers" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let transferlistStorage =
                  Transferlist.Storage
                    { sTransfers = Set.fromList [(wallet1, wallet2)]
                    , sReceivers = Set.fromList [wallet1, wallet2]
                    }
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
                      { opTransferlistContract = (Just $ toTAddress transferlistContract)
                      }
            stablecoinContract <- originate originationParams
            do
              let transfers =
                    [ FA2.TransferItem
                        { tiFrom = wallet1
                        , tiTxs = [FA2.TransferDestination {tdTo = wallet2, tdTokenId = FA2.theTokenId, tdAmount = 10}]
                        }
                    ]

              withSender commonOperator $ call stablecoinContract (Call @"Transfer") transfers
      , testScenario "can approve mint operation" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let transferlistStorage =
                  Transferlist.Storage
                    { sTransfers = Set.fromList [(wallet1, wallet2)]
                    , sReceivers = Set.fromList [wallet1, wallet2]
                    }
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    addMinter (wallet1, 30) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                        { opTransferlistContract = (Just $ toTAddress transferlistContract)
                        }
            stablecoinContract <- originate originationParams
            do
              let mintings = [MintParam wallet1 10]
              withSender wallet1 $ call stablecoinContract (Call @"Mint") mintings
      , testScenario "can approve burn operation" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let transferlistStorage =
                  Transferlist.Storage
                    { sTransfers = Set.fromList [(wallet1, wallet2)]
                    , sReceivers = Set.fromList [wallet1, wallet2]
                    }
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            transferlistContract <- originateSimple "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
            let originationParams =
                  addMinter (wallet1, 0) $
                    addAccount (wallet1, (commonOperators, 35)) $
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                        { opTransferlistContract = (Just $ toTAddress transferlistContract)
                        }
            stablecoinContract <- originate originationParams
            do
              withSender wallet1 $ call stablecoinContract (Call @"Burn") [10]
      ]
  ]
