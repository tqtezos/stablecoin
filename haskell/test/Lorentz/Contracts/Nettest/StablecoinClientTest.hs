-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{-# LANGUAGE OverloadedLists #-}

module Lorentz.Contracts.Nettest.StablecoinClientTest
  ( test_stablecoinClientScenario
  ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

import Morley.Tezos.Address (Address)
import Morley.Tezos.Address.Alias (AddressOrAlias(..))
import Morley.Util.Named (pattern (:!))
import Test.Cleveland as NT
import Test.Cleveland.Internal.Abstract (Moneybag(..), SpecificOrDefaultAlias, ccMoneybag)
import Test.Cleveland.Tasty.Internal (whenNetworkEnabled)

import Stablecoin.Client
  (AddressAndAlias(..), InitialStorageData(..), UpdateOperatorData(AddOperator, RemoveOperator))
import Stablecoin.Client.Cleveland
  (StablecoinT, acceptOwnership, assertEq, burn, changeMasterMinter, changePauser, configureMinter,
  deploy, getBalanceOf, getContractOwner, getMasterMinter, getMintingAllowance, getPaused,
  getPauser, getPendingContractOwner, getTokenMetadata, getTransferlist, isOperator, mint, pause,
  removeMinter, setTransferlist, transferOwnership, unpause, updateOperators)
import Stablecoin.Client.Cleveland qualified as SC
import Stablecoin.Client.Cleveland.Caps (runStablecoinClient)
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

test_stablecoinClientScenario :: TestTree
test_stablecoinClientScenario =
  whenNetworkEnabled $ \withEnv ->
    testCase "stablecoinClientScenario" do
      withEnv (`runStablecoinClient` stablecoinClientScenario)

-- | Check that all the `stablecoin-client` commands work.

createRole :: SpecificOrDefaultAlias -> StablecoinT m (Address, AddressOrAlias)
createRole alias = do
  addr <- newAddress alias
  return (addr, AddressResolved addr)

stablecoinClientScenario :: StablecoinT m ()
stablecoinClientScenario = do
  Moneybag nettestAddress <- asks $ ccMoneybag . SC.scCleveland
  let originator = AddressResolved nettestAddress

  comment "Creating roles"
  (masterMinterAddr, masterMinter) <- createRole "master-minter"
  (pauserAddr, pauser) <- createRole "pauser"
  (contractOwnerAddr, contractOwner) <- createRole "contract-owner"
  (_, minter) <- createRole "minter"
  (transferlistAddr, transferlist) <- createRole "transferlist"

  comment "Deploying contract"
  contractAddr <- deploy (#sender :! originator) InitialStorageData
    { isdMasterMinter = masterMinter
    , isdContractOwner = contractOwner
    , isdPauser = pauser
    , isdTransferlist = Just transferlist
    , isdTokenSymbol = "a"
    , isdTokenName = "b"
    , isdTokenDecimals = 3
    , isdDefaultExpiry = 1000
    , isdContractMetadataStorage = OpRemoteContract Nothing
    }
  let contract = #contract :! AddressResolved contractAddr
  comment "Testing get-balance"
  actualBalance <- SC.getBalance contract
  expectedBalance <- NT.getBalance contractAddr
  actualBalance `assertEq` expectedBalance

  comment "Testing set/get-transferlist"
  getTransferlist contract >>= \tl ->
    fmap getAddress tl `assertEq` Just transferlistAddr
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
  let user1 = minter
  let user2 = pauser
  configureMinter (#sender :! masterMinter) contract minter Nothing 100

  mint (#sender :! minter) contract user1 8
  burn (#sender :! minter) contract [1]
  getBalanceOf contract user1 >>=  \balance -> balance `assertEq` 7
  getBalanceOf contract user2 >>=  \balance -> balance `assertEq` 0

  SC.transfer (#sender :! minter) contract user1 user2 2
  getBalanceOf contract user1 >>=  \balance -> balance `assertEq` 5
  getBalanceOf contract user2 >>=  \balance -> balance `assertEq` 2

  getMintingAllowance contract minter >>= \allowance -> allowance `assertEq` 92
  removeMinter (#sender :! masterMinter) contract minter
  getMintingAllowance contract minter >>= \allowance -> allowance `assertEq` 0

  comment "Testing operators"
  updateOperators (#sender :! minter) contract (one $ AddOperator pauser)
  isOperator contract minter pauser >>= (`assertEq` True)
  updateOperators (#sender :! minter) contract (one $ RemoveOperator pauser)
  isOperator contract minter pauser >>= (`assertEq` False)

  comment "Testing change-master-minter"
  getMasterMinter contract >>= \(AddressAndAlias mm _) ->
    mm `assertEq` masterMinterAddr
  changeMasterMinter (#sender :! contractOwner) contract $ AddressResolved nettestAddress

  comment "Testing change-pauser"
  getPauser contract >>= \(AddressAndAlias p _) ->
    p `assertEq` pauserAddr
  changePauser (#sender :! contractOwner) contract $ AddressResolved nettestAddress

  comment "Testing transfer/accept-ownership"
  getContractOwner contract >>= \(AddressAndAlias owner _) ->
    owner `assertEq` contractOwnerAddr
  getPendingContractOwner contract >>= \po ->
    po `assertEq` Nothing
  transferOwnership (#sender :! contractOwner) contract pauser
  getPendingContractOwner contract >>= \po ->
    fmap getAddress po `assertEq` Just pauserAddr
  acceptOwnership (#sender :! pauser) contract

  comment "Testing token metadata"
  getTokenMetadata contract >>= \tm ->
    tm `assertEq` (#symbol :! "a", #name :! "b", #decimals :! 3)

  where
    getAddress :: AddressAndAlias -> Address
    getAddress (AddressAndAlias addr _) = addr
