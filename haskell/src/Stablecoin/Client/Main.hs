-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Main
  ( mainProgram
  , mainProgramIO
  ) where

import Control.Exception.Uncaught (displayUncaughtException)
import Data.Map (lookup)
import Fmt (pretty)
import Morley.Client
  (AliasBehavior(..), MorleyClientM, ResolveError(..), TezosClientError(ResolveError),
  mkMorleyClientEnv, rememberContract, runMorleyClientM)
import Morley.Client.TezosClient.Impl (resolveAddressEither)
import Morley.Michelson.Text (mt)
import Morley.Tezos.Address.Alias (AddressOrAlias(..), Alias(..), ContractAlias)
import Morley.Util.Named (pattern (:!))
import Options.Applicative qualified as Opt

import Stablecoin.Client.Contract (InitialStorageOptions(..))
import Stablecoin.Client.Impl
  (AddressAndAlias(..), acceptOwnership, burn, changeMasterMinter, changePauser, configureMinter,
  deploy, getBalance, getBalanceOf, getContractOwner, getMasterMinter, getMintingAllowance,
  getPaused, getPauser, getPendingContractOwner, getTokenMetadata, getTransferlist, isOperator,
  mint, pause, removeMinter, setTransferlist, transfer, transferOwnership, unpause, updateOperators)
import Stablecoin.Client.Parser
  (BurnOptions(..), ChangeMasterMinterOptions(..), ChangePauserOptions(..), ClientArgs(..),
  ClientArgsRaw(..), ConfigureMinterOptions(..), DeployContractOptions(..), GetBalanceOfOptions(..),
  GetMintingAllowanceOptions(..), GlobalOptions(..), IsOperatorOptions(..), MintOptions(..),
  RemoveMinterOptions(..), SetTransferlistOptions(..), TransferOptions(..),
  TransferOwnershipOptions(..), UpdateOperatorsOptions(..), stablecoinClientInfo)

mainProgramIO :: IO ()
mainProgramIO = do
  args <- Opt.execParser stablecoinClientInfo
  morleyClientEnv <- mkMorleyClientEnv (caMorleyClientConfig args)
  displayUncaughtException $
    runMorleyClientM morleyClientEnv $ mainProgram args

mainProgram :: ClientArgs -> MorleyClientM ()
mainProgram (ClientArgs _ globalOptions cmd) = do
  case cmd of
    CmdDeployContract DeployContractOptions {..} -> do
      let contractAlias = ContractAlias "stablecoin"
      aliasAlreadyExists <- checkIfAliasExists contractAlias

      (opHash, contractAddr, mCMetadataAddr) <- deploy user contractAlias InitialStorageOptions
        { isoMasterMinter = dcoMasterMinter
        , isoContractOwner = dcoContractOwner
        , isoPauser = dcoPauser
        , isoTransferlist = dcoTransferlist
        , isoTokenName = dcoTokenName
        , isoTokenSymbol = dcoTokenSymbol
        , isoTokenDecimals = dcoTokenDecimals
        , isoDefaultExpiry = dcoDefaultExpiry
        , isoContractMetadataStorage = dcoContractMetadata
        }

      putTextLn "Contract was successfully deployed."
      putTextLn $ "Operation hash: " <> pretty opHash
      putTextLn $ "Contract address: " <> pretty contractAddr
      whenJust mCMetadataAddr $ \addr ->
        putTextLn $ "Metadata contract address: " <> pretty addr

      let printAlias = putTextLn $ "Created alias '" <> pretty contractAlias <> "'."

      if aliasAlreadyExists
        then if dcoReplaceAlias
          then do
            -- silently replace existing alias
            rememberContract OverwriteDuplicateAlias contractAddr contractAlias
            printAlias
          else do
            -- ask the user whether they want to replace the existing alias
            putTextLn ""
            putTextLn $ "Alias '" <> pretty contractAlias <> "' already exists."
            confirmAction "Would you like to replace it with the newly deployed contract?" >>= \case
              Canceled -> pass
              Confirmed -> do
                rememberContract OverwriteDuplicateAlias contractAddr contractAlias
                printAlias
        else
          printAlias

    CmdTransfer TransferOptions {..} ->
      transfer user contract toFrom toTo toAmount

    CmdGetBalanceOf GetBalanceOfOptions {..} -> do
      balance <- getBalanceOf contract gbooOwner
      putTextLn $ "Current balance: " <> pretty balance

    CmdUpdateOperators UpdateOperatorsOptions {..} ->
      updateOperators user contract uooUpdateOperators

    CmdIsOperator IsOperatorOptions {..} -> do
      ifM (isOperator contract iooOwner iooOperator)
        (putTextLn "This account is an operator.")
        (putTextLn "This account is not an operator.")

    CmdPause -> pause user contract
    CmdUnpause -> unpause user contract

    CmdConfigureMinter ConfigureMinterOptions {..} ->
      configureMinter user contract cmoMinter cmoCurrentMintingAllowance cmoNewMintingAllowance

    CmdRemoveMinter RemoveMinterOptions {..} ->
      removeMinter user contract rmoMinter

    CmdMint MintOptions {..} ->
      mint user contract moTo moAmount

    CmdBurn BurnOptions {..} ->
      burn user contract boAmounts

    CmdTransferOwnership TransferOwnershipOptions {..} ->
      transferOwnership user contract tooTo

    CmdAcceptOwnership ->
      acceptOwnership user contract

    CmdChangeMasterMinter ChangeMasterMinterOptions {..} ->
      changeMasterMinter user contract cmmoTo

    CmdChangePauser ChangePauserOptions {..} ->
      changePauser user contract cpoTo

    CmdSetTransferlist SetTransferlistOptions {..} ->
      setTransferlist user contract ssoTransferlist

    CmdGetBalance -> do
      balance <- getBalance contract
      putTextLn $ "Current balance: " <> pretty balance

    CmdGetPaused ->
      getPaused contract >>= \case
        True -> putTextLn "Contract is paused."
        False -> putTextLn "Contract is not paused."

    CmdGetContractOwner ->
      getContractOwner contract >>= putAddressAndAlias "Contract owner"

    CmdGetPendingContractOwner ->
      getPendingContractOwner contract >>= \case
        Nothing -> putTextLn "There is no pending contract owner."
        Just addrAndAlias -> putAddressAndAlias "Pending contract owner" addrAndAlias

    CmdGetMasterMinter ->
      getMasterMinter contract >>= putAddressAndAlias "Master minter"

    CmdGetPauser ->
      getPauser contract >>= putAddressAndAlias "Pauser"

    CmdGetTransferlist ->
      getTransferlist contract >>= \case
        Nothing -> putTextLn "Transferlist contract is not set."
        Just addrAndAlias -> putAddressAndAlias "Transferlist contract" addrAndAlias

    CmdGetMintingAllowance GetMintingAllowanceOptions {..} -> do
      allowance <- getMintingAllowance contract gmaoMinter
      putTextLn $ "Minting allowance: " <> pretty allowance

    CmdGetTokenMetadata -> do
      tm <- getTokenMetadata contract
      putTextLn $ "Token symbol: " <> (decodeUtf8 $ fromMaybe "" $ lookup [mt|symbol|] tm)
      putTextLn $ "Token name: " <> (decodeUtf8 $ fromMaybe "" $ lookup [mt|name|] tm)
      putTextLn $ "Token decimals: " <> (decodeUtf8 $ fromMaybe "" $ lookup [mt|decimals|] tm)

  where
    user = #sender :! goUser globalOptions
    contract = #contract :! goContract globalOptions

    putAddressAndAlias :: Text -> AddressAndAlias -> MorleyClientM ()
    putAddressAndAlias prefix (AddressAndAlias addr aliasMb) = do
      putTextLn $ prefix <> " address: " <> pretty addr
      whenJust aliasMb $ \alias ->
        putTextLn $ prefix <> " alias: " <> pretty alias

    checkIfAliasExists :: ContractAlias -> MorleyClientM Bool
    checkIfAliasExists alias = do
      resolveAddressEither (AddressAlias alias) >>= \case
        Right{} -> pure True
        Left REAliasNotFound{} -> pure False
        Left err -> throwM (ResolveError err)

    confirmAction :: Text -> MorleyClientM ConfirmationResult
    confirmAction msg = do
      putTextLn $ msg <> " [Y/N]"
      res <- getLine
      case res of
        x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
        x | x `elem` ["N", "n", "no"] -> pure Canceled
        _ -> confirmAction msg

data ConfirmationResult = Confirmed | Canceled
