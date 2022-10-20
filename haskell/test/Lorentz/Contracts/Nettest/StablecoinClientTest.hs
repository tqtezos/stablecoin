-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{-# LANGUAGE OverloadedLists #-}

module Lorentz.Contracts.Nettest.StablecoinClientTest
  ( test_stablecoinClientScenario
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Morley.Tezos.Address (Address)
import Test.Cleveland as NT
import Test.Cleveland.Internal.Abstract (Moneybag(..), ccMoneybag)
import Test.Cleveland.Lorentz (toContractAddress)
import Test.Cleveland.Tasty (whenNetworkEnabled)

import Indigo.Contracts.Transferlist.Internal qualified as Transferlist
import Stablecoin.Client (AddressAndAlias(..), UpdateOperatorData(AddOperator, RemoveOperator))
import Stablecoin.Client.Cleveland
  (StablecoinT, acceptOwnership, assertEq, burn, changeMasterMinter, changePauser, configureMinter,
  deploy, getBalanceOf, getContractOwner, getMasterMinter, getMintingAllowance, getPaused,
  getPauser, getPendingContractOwner, getTokenMetadata, getTransferlist, isOperator, mint, pause,
  removeMinter, setTransferlist, transferOwnership, unpause, updateOperators)
import Stablecoin.Client.Cleveland qualified as SC
import Stablecoin.Client.Cleveland.Caps (runStablecoinClient)
import Stablecoin.Client.Contract (InitialStorageOptions(..))
import Stablecoin.Client.L1AddressOrAlias (KindedAddressOrAlias(..), L1AddressOrAlias(..))
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

test_stablecoinClientScenario :: TestTree
test_stablecoinClientScenario =
  whenNetworkEnabled $ \withEnv ->
    testCase "stablecoinClientScenario" do
      withEnv (`runStablecoinClient` stablecoinClientScenario)

-- | Check that all the `stablecoin-client` commands work.
stablecoinClientScenario :: StablecoinT m ()
stablecoinClientScenario = do
  Moneybag originator <- asks $ ccMoneybag . SC.scCleveland

  comment "Creating roles"
  masterMinter <- newAddress "master-minter"
  pauser <- newAddress "pauser"
  contractOwner <- newAddress "contract-owner"
  minter <- newAddress "minter"
  transferlist <-
    originate "transferlist"
      (Transferlist.Storage mempty mempty)
      Transferlist.transferlistContract

  comment "Deploying contract"
  contractAddr <- deploy (#sender :! originator) InitialStorageOptions
    { isoMasterMinter = L1AOAKindSpecified $ KAOAAddress masterMinter
    , isoContractOwner = L1AOAKindSpecified $ KAOAAddress contractOwner
    , isoPauser = L1AOAKindSpecified $ KAOAAddress pauser
    , isoTransferlist = Just $ KAOAAddress $ toContractAddress transferlist
    , isoTokenSymbol = "a"
    , isoTokenName = "b"
    , isoTokenDecimals = 3
    , isoDefaultExpiry = 1000
    , isoContractMetadataStorage = OpRemoteContract Nothing
    }
  let contract = #contract :! toContractAddress contractAddr
  comment "Testing get-balance"
  actualBalance <- SC.getBalance contract
  expectedBalance <- NT.getBalance contractAddr
  actualBalance `assertEq` expectedBalance

  comment "Testing set/get-transferlist"
  getTransferlist contract >>= \tl ->
    fmap getAddress tl `assertEq` Just (toAddress transferlist)
  setTransferlist (#sender :! contractOwner) contract Nothing
  getTransferlist contract >>= \tl ->
    tl `assertEq` Nothing

  comment "Testing pause/unpause/get-paused"
  getPaused contract >>= (`assertEq` False)
  pause (#sender :! pauser) contract
  getPaused contract >>= (`assertEq` True)
  unpause (#sender :! pauser) contract
  getPaused contract >>= (`assertEq` False)

  comment "Testing minting / burning / transfers"
  -- Note: Reusing existing accounts here for efficiency
  let user1 = toL1Address minter
  let user2 = toL1Address pauser
  configureMinter (#sender :! masterMinter) contract (toL1Address minter) Nothing 100

  mint (#sender :! minter) contract user1 8
  burn (#sender :! minter) contract [1]
  getBalanceOf contract user1 >>=  \balance -> balance `assertEq` 7
  getBalanceOf contract user2 >>=  \balance -> balance `assertEq` 0

  SC.transfer (#sender :! minter) contract user1 user2 2
  getBalanceOf contract user1 >>=  \balance -> balance `assertEq` 5
  getBalanceOf contract user2 >>=  \balance -> balance `assertEq` 2

  getMintingAllowance contract (toL1Address minter) >>= \allowance -> allowance `assertEq` 92
  removeMinter (#sender :! masterMinter) contract (toL1Address minter)
  getMintingAllowance contract (toL1Address minter) >>= \allowance -> allowance `assertEq` 0

  comment "Testing operators"
  updateOperators (#sender :! minter) contract (one $ AddOperator $ L1AOAKindSpecified $ KAOAAddress pauser)
  isOperator contract (toL1Address minter) (toL1Address pauser) >>= (`assertEq` True)
  updateOperators (#sender :! minter) contract (one $ RemoveOperator $ L1AOAKindSpecified $ KAOAAddress pauser)
  isOperator contract (toL1Address minter) (toL1Address pauser) >>= (`assertEq` False)

  comment "Testing change-master-minter"
  getMasterMinter contract >>= \(AddressAndAlias mm _) ->
    mm `assertEq` toAddress masterMinter
  changeMasterMinter (#sender :! contractOwner) contract $ toL1Address originator

  comment "Testing change-pauser"
  getPauser contract >>= \(AddressAndAlias p _) ->
    p `assertEq` toAddress pauser
  changePauser (#sender :! contractOwner) contract $ toL1Address originator

  comment "Testing transfer/accept-ownership"
  getContractOwner contract >>= \(AddressAndAlias owner _) ->
    owner `assertEq` toAddress contractOwner
  getPendingContractOwner contract >>= \po ->
    po `assertEq` Nothing
  transferOwnership (#sender :! contractOwner) contract (toL1Address pauser)
  getPendingContractOwner contract >>= \po ->
    fmap getAddress po `assertEq` Just (toAddress pauser)
  acceptOwnership (#sender :! pauser) contract

  comment "Testing token metadata"
  getTokenMetadata contract >>= \tm ->
    tm `assertEq` (#symbol :! "a", #name :! "b", #decimals :! 3)

  where
    getAddress :: AddressAndAlias -> Address
    getAddress (AddressAndAlias addr _) = addr
