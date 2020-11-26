-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Main
  ( mainProgram
  , mainProgramIO
  ) where

import Data.Coerce (coerce)
import Fmt (pretty)
import Morley.Client
  (AddressOrAlias(AddressAlias), Alias(..), AliasHint(..), MorleyClientM,
  TezosClientError(UnknownAddressAlias), mkMorleyClientEnv, rememberContract, runMorleyClientM)
import Morley.Client.TezosClient (resolveAddress)
import qualified Options.Applicative as Opt
import Util.Exception (displayUncaughtException)
import Util.Named ((.!))

import Lorentz.Contracts.Spec.FA2Interface (TokenMetadata(..))
import Stablecoin.Client.Contract (InitialStorageData(..))
import Stablecoin.Client.Impl
  (AddressAndAlias(..), acceptOwnership, burn, changeMasterMinter, changePauser, configureMinter,
  deploy, getBalance, getBalanceOf, getContractOwner, getMasterMinter, getMintingAllowance,
  getPaused, getPauser, getPendingContractOwner, getTokenMetadata, getTransferlist, isOperator,
  mint, pause, removeMinter, setTransferlist, transfer, transferOwnership, unpause,
  updateOperators)
import Stablecoin.Client.Parser
  (BurnOptions(..), ChangeMasterMinterOptions(..), ChangePauserOptions(..), ClientArgs(..),
  ClientArgsRaw(..), ConfigureMinterOptions(..), DeployContractOptions(..),
  GetBalanceOfOptions(..), GetMintingAllowanceOptions(..), GlobalOptions(..),
  IsOperatorOptions(..), MintOptions(..), RemoveMinterOptions(..), SetTransferlistOptions(..),
  TransferOptions(..), TransferOwnershipOptions(..), UpdateOperatorsOptions(..),
  stablecoinClientInfo)

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
      let contractAlias = AliasHint "stablecoin"
      -- TODO: we're using coerce in a few places to convert `AliasHint` to `Alias`
      -- where necessary, but we shouldn't be doing this because it assumes no alias
      -- prefix option was passed in via the CLI.
      aliasAlreadyExists <- checkIfAliasExists (coerce contractAlias)

      (opHash, contractAddr, metadataRegAddr) <- deploy user (coerce contractAlias) InitialStorageData
        { isdMasterMinter = dcoMasterMinter
        , isdContractOwner = dcoContractOwner
        , isdPauser = dcoPauser
        , isdTransferlist = dcoTransferlist
        , isdTokenName = dcoTokenName
        , isdTokenSymbol = dcoTokenSymbol
        , isdTokenDecimals = dcoTokenDecimals
        , isdTokenMetadataRegistry = dcoTokenMetadataRegistry
        , isdDefaultExpiry = dcoDefaultExpiry
        }

      putTextLn "Contract was successfully deployed."
      putTextLn $ "Operation hash: " <> pretty opHash
      putTextLn $ "Contract address: " <> pretty contractAddr
      putTextLn $ "Metadata registry contract address: " <> pretty metadataRegAddr

      let printAlias = putTextLn $ "Created alias '" <> pretty contractAlias <> "'."

      if aliasAlreadyExists
        then if dcoReplaceAlias
          then do
            -- silently replace existing alias
            rememberContract True contractAddr (coerce contractAlias)
            printAlias
          else do
            -- ask the user whether they want to replace the existing alias
            putTextLn ""
            putTextLn $ "Alias '" <> pretty contractAlias <> "' already exists."
            confirmAction "Would you like to replace it with the newly deployed contract?" >>= \case
              Canceled -> pass
              Confirmed -> do
                rememberContract True contractAddr (coerce contractAlias)
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
      putTextLn $ "Token symbol: " <> pretty (tmSymbol tm)
      putTextLn $ "Token name: " <> pretty (tmName tm)
      putTextLn $ "Token decimals: " <> pretty (tmDecimals tm)

  where
    user = #sender .! goUser globalOptions
    contract = #contract .! goContract globalOptions

    putAddressAndAlias :: Text -> AddressAndAlias -> MorleyClientM ()
    putAddressAndAlias prefix (AddressAndAlias addr aliasMb) = do
      putTextLn $ prefix <> " address: " <> pretty addr
      whenJust aliasMb $ \alias ->
        putTextLn $ prefix <> " alias: " <> pretty alias

    checkIfAliasExists :: Alias -> MorleyClientM Bool
    checkIfAliasExists alias = do
      (resolveAddress (AddressAlias alias) $> True) `catch` \case
        UnknownAddressAlias _ -> pure False
        err -> throwM err

    confirmAction :: Text -> MorleyClientM ConfirmationResult
    confirmAction msg = do
      putTextLn $ msg <> " [Y/N]"
      res <- getLine
      case res of
        x | x `elem` ["Y", "y", "yes"] -> pure Confirmed
        x | x `elem` ["N", "n", "no"] -> pure Canceled
        _ -> confirmAction msg

data ConfirmationResult = Confirmed | Canceled
