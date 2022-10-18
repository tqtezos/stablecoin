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
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)

import Lorentz.Address
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Tezos.Address (detGenKeyAddress)
import Morley.Util.Named
import Test.Cleveland
import Test.Cleveland.Lorentz.Consumer

import Indigo.Contracts.Transferlist.Internal qualified as Transferlist
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

managementSpec :: [TestTree]
managementSpec =
  [ testGroup "Contract meta" $
      [ testScenario "fails if contract receives non-zero amount of xtz" $
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
            stablecoinContract <- originateStablecoin originationParams
            do
              mgmXtzReceived $
                withSender commonOperator $
                  transfer stablecoinContract [tz|10u|] $
                    calling (ep @"Transfer") (constructTransfersFromSender (#from_ :! wallet1) [])
      ]
  , testGroup "Contract pausing" $
      [ testScenario "pauses contract as expected" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testPauser $
              transfer stablecoinContract $ calling (ep @"Pause") ()
            storage <- getStorage stablecoinContract
            unless (sPausedRPC storage) $ failure "Contract is not paused as was expected"
      , testScenario "cannot pause if sender does not have corresponding permissions" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotPauser $
              withSender wallet2 $
                transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "pause cannot be called multiple times in a row" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            do
              withSender testPauser $ do
                transfer stablecoinContract $ calling (ep @"Pause") ()
                mgmContractPaused $
                  transfer stablecoinContract $ calling (ep @"Pause") ()
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
            stablecoinContract <- originateStablecoin originationParams
            withSender testPauser $
              transfer stablecoinContract $ calling (ep @"Unpause") ()
            storage <- getStorage stablecoinContract
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
            stablecoinContract <- originateStablecoin originationParams
            mgmNotPauser $
              withSender wallet2 $
                transfer stablecoinContract $ calling (ep @"Unpause") ()
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
            stablecoinContract <- originateStablecoin originationParams
            withSender testPauser do
              transfer stablecoinContract $ calling (ep @"Unpause") ()
              mgmContractNotPaused $
                transfer stablecoinContract $ calling (ep @"Unpause") ()
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
            stablecoinContract <- originateStablecoin originationParams
            let transfers = constructSingleTransfer (#from_ :! toAddress wallet1) (#to_ :! toAddress wallet2) (#amount :! 10)
            mgmContractPaused $
              withSender commonOperator $
                transfer stablecoinContract $ calling (ep @"Transfer") transfers
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

            stablecoinContract <- originateStablecoin originationParams
            withSender testPauser $
              transfer stablecoinContract $ calling (ep @"Unpause") ()
            let transfer1 =
                  constructTransfersFromSender
                    (#from_ :! wallet1)
                    [ (#to_ :! wallet2, #amount :! 5)
                    , (#to_ :! wallet2, #amount :! 5)
                    ]

                transfer2 =
                  constructSingleTransfer
                    (#from_ :! toAddress wallet2)
                    (#to_ :! toAddress wallet3)
                    (#amount :! 10)

            withSender commonOperator do
              transfer stablecoinContract $ calling (ep @"Transfer") transfer1
              transfer stablecoinContract $ calling (ep @"Transfer") transfer2

            consumer <- originate "consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
            let balanceRequestItems =
                  [ FA2.BalanceRequestItem (toAddress wallet1) FA2.theTokenId
                  , FA2.BalanceRequestItem (toAddress wallet2) FA2.theTokenId
                  , FA2.BalanceRequestItem (toAddress wallet3) FA2.theTokenId
                  ]
                balanceRequest = FA2.mkFA2View balanceRequestItems consumer
                balanceExpected =
                  [ FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet1) FA2.theTokenId
                      , briBalance = 0
                      }
                  , FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet2) FA2.theTokenId
                      , briBalance = 0
                      }
                  , FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet3) FA2.theTokenId
                      , briBalance = 10
                      }
                  ]

            transfer stablecoinContract $ calling (ep @"Balance_of") balanceRequest

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
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            let configureMinterParam =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet1
                    , cmpCurrentMintingAllowance = Nothing
                    , cmpNewMintingAllowance = 30
                    }

                configureMinterParam1 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Nothing
                    , cmpNewMintingAllowance = 20
                    }

                configureMinterParam2 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Just 20
                    , cmpNewMintingAllowance = 10
                    }

            withSender testMasterMinter do
              transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam
              transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam1
              transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam2

            storage <- getStorage stablecoinContract
            let expectedMinters = M.fromList [(toAddress wallet1, 30), (toAddress wallet2, 10)]
              in when (sMintingAllowancesRPC storage /= expectedMinters) $
                  failure "Configure_minter call produced a malformed minter list"
      , testScenario "fails if expected and actual minting allowances do not match" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            let configureMinterParam1 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Nothing
                    , cmpNewMintingAllowance = 20
                    }

            let configureMinterParam2 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Just 2000 -- Mismatched allowance here
                    , cmpNewMintingAllowance = 10
                    }

            withSender testMasterMinter do
              transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam1
              mgmAllowanceMismatch $
                transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam2
      , testScenario "fails if minter is present in list of minters which was not expected" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            let configureMinterParam1 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Nothing
                    , cmpNewMintingAllowance = 20
                    }

            let configureMinterParam2 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Nothing -- Here we expect for `wallet2` being non-present in minting allowances map
                    , cmpNewMintingAllowance = 10
                    }

            withSender testMasterMinter do
              transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam1
              mgmCurrentAllowanceRequired $
                transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam2
      , testScenario "fails if sender does not have master minter permissions" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            let configureMinterParam1 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Nothing
                    , cmpNewMintingAllowance = 20
                    }

            mgmNotMasterMinter $
              withSender wallet2 $
                transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam1
      , testScenario "fails if contract is paused" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testPauser $
              transfer stablecoinContract $ calling (ep @"Pause") ()

            let configureMinterParam1 =
                  ConfigureMinterParam
                    { cmpMinter = toAddress wallet2
                    , cmpCurrentMintingAllowance = Nothing
                    , cmpNewMintingAllowance = 20
                    }

            mgmContractPaused $
              transfer stablecoinContract $ calling (ep @"Configure_minter") configureMinterParam1
      ]
  , let configureMinterParam :: Int -> ConfigureMinterParam
        configureMinterParam i =
          ConfigureMinterParam
            { cmpMinter = toAddress $ detGenKeyAddress $ encodeUtf8 @Text ("a" <> show i)
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
                  originateStablecoin $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
                withSender testMasterMinter $ do
                  forM_ (configureMinterParam <$> [1 .. minterLimit]) \param ->
                    transfer stablecoinContract $ calling (ep @"Configure_minter") param

                  storage <- getStorage stablecoinContract
                  unless (M.size (sMintingAllowancesRPC storage) == minterLimit) $
                    failure "Configure_minter call produced a malformed minter list"
          , testScenario "Throws error when minter limit is exceeded" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                stablecoinContract <-
                  originateStablecoin $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )

                withSender testMasterMinter $ do
                  mgmMinterLimitExceeded $
                    forM_ (configureMinterParam <$> [1 .. (minterLimit + 1)]) \param ->
                      transfer stablecoinContract $ calling (ep @"Configure_minter") param
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
            stablecoinContract <- originateStablecoin originationParams
            withSender testMasterMinter do
              transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress wallet1
              transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress wallet2
            storage <- getStorage stablecoinContract
            let expectedMinters = M.fromList [(toAddress wallet3, 100)]
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
            stablecoinContract <- originateStablecoin originationParams
            mgmNotMasterMinter $
              withSender wallet2 $
                transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress wallet1
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
            stablecoinContract <- originateStablecoin originationParams
            withSender testMasterMinter do
              transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress wallet1
              mgmAddrNotMinter $
                transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress wallet1
      , testScenario "cannot remove non-minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmAddrNotMinter $
              withSender testMasterMinter $
                transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress wallet1
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
            stablecoinContract <- originateStablecoin originationParams
            let mintings =
                  [ MintParam (toAddress wallet1) 10
                  , MintParam (toAddress wallet2) 5
                  , MintParam (toAddress wallet3) 15
                  ]

            withSender wallet1 $
              transfer stablecoinContract $ calling (ep @"Mint") mintings

            consumer <- originate "consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
            let balanceRequestItems =
                  [ FA2.BalanceRequestItem (toAddress wallet1) FA2.theTokenId
                  , FA2.BalanceRequestItem (toAddress wallet2) FA2.theTokenId
                  , FA2.BalanceRequestItem (toAddress wallet3) FA2.theTokenId
                  ]
                balanceRequest = FA2.mkFA2View balanceRequestItems consumer
                balanceExpected =
                  [ FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet1) FA2.theTokenId
                      , briBalance = 10
                      }
                  , FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet2) FA2.theTokenId
                      , briBalance = 5
                      }
                  , FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet3) FA2.theTokenId
                      , briBalance = 15
                      }
                  ]

            transfer stablecoinContract $ calling (ep @"Balance_of") balanceRequest

            getStorage consumer @@== [balanceExpected]

            storage <- getStorage stablecoinContract
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
            stablecoinContract <- originateStablecoin originationParams
            let mintings =
                  [ MintParam (toAddress wallet1) 5
                  , MintParam (toAddress wallet2) 10 -- Error here
                  , MintParam (toAddress wallet3) 5
                  ]

            expectFailedWithAny $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Mint") mintings
      , testScenario "fails if sender is not minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            let mintings = [MintParam (toAddress wallet1) 5]
            expectFailedWithAny $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Mint") mintings
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
            stablecoinContract <- originateStablecoin originationParams
            let mintings = [MintParam (toAddress wallet1) 5]
            mgmContractPaused $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Mint") mintings
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
            stablecoinContract <- originateStablecoin originationParams
            withSender wallet1 $
              transfer stablecoinContract $ calling (ep @"Burn") [10, 20]
            withSender wallet2 $
              transfer stablecoinContract $ calling (ep @"Burn") [0]

            consumer <- originate "consumer" [] (contractConsumer @[FA2.BalanceResponseItem])
            let balanceRequestItems =
                  [ FA2.BalanceRequestItem (toAddress wallet1) FA2.theTokenId
                  , FA2.BalanceRequestItem (toAddress wallet2) FA2.theTokenId
                  ]
                balanceRequest = FA2.mkFA2View balanceRequestItems consumer
                balanceExpected =
                  [ FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet1) FA2.theTokenId
                      , briBalance = 5
                      }
                  , FA2.BalanceResponseItem
                      { briRequest = FA2.BalanceRequestItem (toAddress wallet2) FA2.theTokenId
                      , briBalance = 0
                      }
                  ]

            transfer stablecoinContract $ calling (ep @"Balance_of") balanceRequest

            getStorage consumer @@== [balanceExpected]

            storage <- getStorage stablecoinContract
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
            stablecoinContract <- originateStablecoin originationParams
            withSender wallet1 $
              transfer stablecoinContract $ calling (ep @"Burn") [35]

            storage <- getStorage stablecoinContract
            val <- getBigMapValueMaybe (sLedgerRPC storage) (toAddress wallet1)
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
            stablecoinContract <- originateStablecoin originationParams
            mgmNotMinter $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Burn") [10]
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
            stablecoinContract <- originateStablecoin originationParams
            mgmInsufficientBalance $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Burn") [10, 10]
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
            stablecoinContract <- originateStablecoin originationParams
            withSender wallet1 do
              transfer stablecoinContract $ calling (ep @"Burn") [10]
              let mintings = [MintParam (toAddress wallet1) 10]
              mgmAllowanceExceeded $
                transfer stablecoinContract $ calling (ep @"Mint") mintings
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
            stablecoinContract <- originateStablecoin originationParams
            mgmContractPaused $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Burn") [10]
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
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner $
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
            withSender wallet1 do
              transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
            withSender wallet2 $
              transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
            storage <- getStorage stablecoinContract
            when ((rOwner $ sRolesRPC storage) /= toAddress wallet2) $
              failure "Owner was not changed"
      , testScenario "current contract owner retains its privileges if ownership weren't accepted yet" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner $
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
            storage <- getStorage stablecoinContract
            when ((rOwner $ sRolesRPC storage) /= toAddress testOwner) $
              failure "Owner was changed"
      , testScenario "transferring ownership fails if sender is not contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
      , testScenario "fails if previous contract owner tries to use ownership privileges" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner $
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
            withSender wallet1 $
              transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
            mgmNotContractOwner $
              withSender testOwner $
                transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
      , testScenario "accepting ownership fails if sender is not pending contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner $
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
            mgmNotPendingOwner $
              withSender wallet2 $
                transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
      , testScenario "accepting ownership fails if pending owner is not set" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNoPendingOwnerSet $
              withSender wallet2 $
                transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
      , testScenario "transfer ownership can be called multiple times each of which invalidates the previous call" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            wallet3 <- newAddress "wallet3"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner do
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet3
            withSender wallet3 $
              transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
            storage <- getStorage stablecoinContract
            when (rOwner (sRolesRPC storage) /= toAddress wallet3) $
              failure "Owner was not changed"
      , testScenario "contract cannot retain ownership privileges if pending owner was changed by subsequent transfer ownership call" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner do
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
            mgmNotPendingOwner $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
      , testScenario "contract owner changes master minter properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner $
              transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet1
            storage <- getStorage stablecoinContract
            when (rMasterMinter (sRolesRPC storage) /= toAddress wallet1) $
              failure "Master minter was not changed"
      , testScenario "contract owner changes contract pauser properly" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            withSender testOwner $
              transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet1
            storage <- getStorage stablecoinContract
            when (rPauser (sRolesRPC storage) /= toAddress wallet1) $
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
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender testMasterMinter $
                transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet1
      , testScenario "fails to change contract master minter if sender is not contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet2
      , testScenario "master minter cannot change contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender testMasterMinter $
                transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
      , testScenario "master minter cannot change contract pauser" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender testMasterMinter $
                transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet1

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
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender testPauser $
                transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet1
      , testScenario "fails to change contract pauser if sender is not contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender wallet1 $
                transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet2
      , testScenario "pauser cannot change contract owner" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender testPauser $
                transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet1
      , testScenario "pauser cannot change master minter" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            stablecoinContract <-
              originateStablecoin $
                ( defaultOriginationParams
                    (#owner :! testOwner)
                    (#pauser :! testPauser)
                    (#masterMinter :! testMasterMinter)
                )
            mgmNotContractOwner $
              withSender testPauser $
                transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet1
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
                transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                stablecoinContract <- originateStablecoin originationParams
                withSender testOwner $
                  transfer stablecoinContract $ calling (ep @"Set_transferlist") (Just $ toAddress transferlistContract)

                storage <- getStorage stablecoinContract
                case sTransferlistContractRPC storage of
                  Just addr
                    | addr == toAddress transferlistContract -> pass
                    | otherwise -> failure "Transferlist contract address was not set correctly"
                  Nothing ->
                    failure "Transferlist contract address was not set"
          , testScenario "can unset transferlist contract address in storage" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                let originationParams =
                      ( defaultOriginationParams
                          (#owner :! testOwner)
                          (#pauser :! testPauser)
                          (#masterMinter :! testMasterMinter)
                      )
                        { opTransferlistContract = (Just $ toTAddress transferlistContract)
                        }
                stablecoinContract <- originateStablecoin originationParams
                withSender testOwner $
                  transfer stablecoinContract $ calling (ep @"Set_transferlist") Nothing

                storage <- getStorage stablecoinContract
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
                stablecoinContract <- originateStablecoin originationParams
                mgmBadTransferlist $
                  withSender testOwner $
                  transfer stablecoinContract $ calling (ep @"Set_transferlist") (Just $ toAddress wallet1)
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
                transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
                let originationParams =
                      addAccount (wallet1, (commonOperators, 10)) $
                        ( defaultOriginationParams
                            (#owner :! testOwner)
                            (#pauser :! testPauser)
                            (#masterMinter :! testMasterMinter)
                        )
                          { opTransferlistContract = (Just $ toTAddress transferlistContract)
                          }
                stablecoinContract <- originateStablecoin originationParams
                let transfers =
                      [FA2.TransferItem (toAddress wallet1) [FA2.TransferDestination (toAddress wallet2) FA2.theTokenId 10]]

                expectFailedWithAny $
                  withSender commonOperator $
                    transfer stablecoinContract $ calling (ep @"Transfer") transfers
          , testScenario "can make mint operation fail" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
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
                stablecoinContract <- originateStablecoin originationParams
                let mintings = [MintParam (toAddress wallet1) 10]
                expectFailedWithAny $
                  withSender wallet1 $
                    transfer stablecoinContract $ calling (ep @"Mint") mintings
          , testScenario "can make burn operation fail" $
              scenario do
                testOwner <- newAddress "testOwner"
                testPauser <- newAddress "testPauser"
                testMasterMinter <- newAddress "testMasterMinter"
                wallet1 <- newAddress "wallet1"
                commonOperator <- newAddress "commonOperator"
                let commonOperators = [commonOperator]
                transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
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
                stablecoinContract <- originateStablecoin originationParams
                expectFailedWithAny $
                  withSender wallet1 $
                    transfer stablecoinContract $ calling (ep @"Burn") [10]
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
                    { sTransfers = Set.fromList [(toAddress wallet1, toAddress wallet2)]
                    , sReceivers = Set.fromList [toAddress wallet1, toAddress wallet2]
                    }
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
            let originationParams =
                  addAccount (wallet1, (commonOperators, 10)) $
                    ( defaultOriginationParams
                        (#owner :! testOwner)
                        (#pauser :! testPauser)
                        (#masterMinter :! testMasterMinter)
                    )
                      { opTransferlistContract = (Just $ toTAddress transferlistContract)
                      }
            stablecoinContract <- originateStablecoin originationParams
            let transfers =
                  [ FA2.TransferItem
                      { tiFrom = toAddress wallet1
                      , tiTxs = [FA2.TransferDestination {tdTo = toAddress wallet2, tdTokenId = FA2.theTokenId, tdAmount = 10}]
                      }
                  ]

            withSender commonOperator $
              transfer stablecoinContract $ calling (ep @"Transfer") transfers
      , testScenario "can approve mint operation" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let transferlistStorage =
                  Transferlist.Storage
                    { sTransfers = Set.fromList [(toAddress wallet1, toAddress wallet2)]
                    , sReceivers = Set.fromList [toAddress wallet1, toAddress wallet2]
                    }
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
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
            stablecoinContract <- originateStablecoin originationParams
            let mintings = [MintParam (toAddress wallet1) 10]
            withSender wallet1 $
              transfer stablecoinContract $ calling (ep @"Mint") mintings
      , testScenario "can approve burn operation" $
          scenario do
            testOwner <- newAddress "testOwner"
            testPauser <- newAddress "testPauser"
            testMasterMinter <- newAddress "testMasterMinter"
            wallet1 <- newAddress "wallet1"
            wallet2 <- newAddress "wallet2"
            let transferlistStorage =
                  Transferlist.Storage
                    { sTransfers = Set.fromList [(toAddress wallet1, toAddress wallet2)]
                    , sReceivers = Set.fromList [toAddress wallet1, toAddress wallet2]
                    }
            commonOperator <- newAddress "commonOperator"
            let commonOperators = [commonOperator]
            transferlistContract <- originate "Transferlist test dummy" transferlistStorage Transferlist.transferlistContract
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
            stablecoinContract <- originateStablecoin originationParams
            withSender wallet1 $
              transfer stablecoinContract $ calling (ep @"Burn") [10]
      ]
  ]
