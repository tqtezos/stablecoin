-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

{-# LANGUAGE OverloadedLists #-}

module StablecoinClientTest
  ( stablecoinClientScenario
  ) where

import Morley.Nettest as NT
import Tezos.Address (Address)
import Util.Named ((.!))

import Stablecoin.Client
  (AddressAndAlias(..), InitialStorageData(..), UpdateOperatorData(AddOperator, RemoveOperator))
import Stablecoin.Client.Cleveland
  (StablecoinScenario, acceptOwnership, assertEq, burn, changeMasterMinter, changePauser,
  configureMinter, deploy, getBalanceOf, getContractOwner, getMasterMinter, getMintingAllowance,
  getPaused, getPauser, getPendingContractOwner, getTokenMetadata, getTransferlist, isOperator,
  mint, pause, removeMinter, revealKeyUnlessRevealed, setTransferlist, transferOwnership, unpause,
  updateOperators)
import qualified Stablecoin.Client.Cleveland as SC
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

-- | Check that all the `stablecoin-client` commands work.
stablecoinClientScenario :: StablecoinScenario m ()
stablecoinClientScenario = do
  let originator = nettestAddress

  comment "Creating roles"
  (masterMinterAlias, masterMinterAddr, masterMinter) <- createRole "master-minter"
  (pauserAlias, pauserAddr, pauser) <- createRole "pauser"
  (contractOwnerAlias, contractOwnerAddr, contractOwner) <- createRole "contract-owner"
  (_, _, minter) <- createRole "minter"
  (transferlistAlias, transferlistAddr, transferlist) <- createRole "transferlist"

  comment "Deploying contract"
  contractAddr <- deploy (#sender.! originator) InitialStorageData
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
  let contract = #contract .! AddressResolved contractAddr
  comment "Testing get-balance"
  actualBalance <- SC.getBalance contract
  expectedBalance <- NT.getBalance (AddressResolved contractAddr)
  actualBalance `assertEq` expectedBalance

  comment "Testing set/get-transferlist"
  getTransferlist contract >>= \tl ->
    tl `assertEq` Just (AddressAndAlias transferlistAddr (Just transferlistAlias))
  setTransferlist (#sender .! contractOwner) contract Nothing
  getTransferlist contract >>= \tl ->
    tl `assertEq` Nothing

  comment "Testing pause/unpause/get-paused"
  getPaused contract >>= (`assertEq` False)
  pause (#sender .! pauser) contract
  getPaused contract >>= (`assertEq` True)
  unpause (#sender .! pauser) contract
  getPaused contract >>= (`assertEq` False)

  comment "Testing minting / burning / transfers"
  -- Note: Reusing existing accounts here for efficiency
  let user1 = minter
  let user2 = pauser
  configureMinter (#sender .! masterMinter) contract minter Nothing 100

  mint (#sender .! minter) contract user1 8
  burn (#sender .! minter) contract [1]
  getBalanceOf contract user1 >>=  \balance -> balance `assertEq` 7
  getBalanceOf contract user2 >>=  \balance -> balance `assertEq` 0

  SC.transfer (#sender .! minter) contract user1 user2 2
  getBalanceOf contract user1 >>=  \balance -> balance `assertEq` 5
  getBalanceOf contract user2 >>=  \balance -> balance `assertEq` 2

  getMintingAllowance contract minter >>= \allowance -> allowance `assertEq` 92
  removeMinter (#sender .! masterMinter) contract minter
  getMintingAllowance contract minter >>= \allowance -> allowance `assertEq` 0

  comment "Testing operators"
  updateOperators (#sender .! minter) contract (one $ AddOperator pauser)
  isOperator contract minter pauser >>= (`assertEq` True)
  updateOperators (#sender .! minter) contract (one $ RemoveOperator pauser)
  isOperator contract minter pauser >>= (`assertEq` False)

  comment "Testing change-master-minter"
  getMasterMinter contract >>= \mm ->
    mm `assertEq` AddressAndAlias masterMinterAddr (Just masterMinterAlias)
  changeMasterMinter (#sender .! contractOwner) contract nettestAddress

  comment "Testing change-pauser"
  getPauser contract >>= \p ->
    p `assertEq` AddressAndAlias pauserAddr (Just pauserAlias)
  changePauser (#sender .! contractOwner) contract nettestAddress

  comment "Testing transfer/accept-ownership"
  getContractOwner contract >>= \owner ->
    owner `assertEq` AddressAndAlias contractOwnerAddr (Just contractOwnerAlias)
  getPendingContractOwner contract >>= \po ->
    po `assertEq` Nothing
  transferOwnership (#sender .! contractOwner) contract pauser
  getPendingContractOwner contract >>= \po ->
    po `assertEq` Just (AddressAndAlias pauserAddr (Just pauserAlias))
  acceptOwnership (#sender .! pauser) contract

  comment "Testing token metadata"
  getTokenMetadata contract >>= \tm ->
    tm `assertEq` (#symbol .! "a", #name .! "b", #decimals .! 3)

  where
    createRole :: AliasHint -> StablecoinScenario m (Alias, Address, AddressOrAlias)
    createRole aliasHint = do
      addr <- newAddress aliasHint

      -- Note: `newAddress` prepends the alias with the prefix from the
      -- `MorleyClientConfig`.
      -- So we use `getAlias` to get the actual alias (prefix included)
      -- associated with this address.
      alias <- getAlias (AddressResolved addr)

      revealKeyUnlessRevealed alias
      pure (alias, addr, AddressResolved addr)
