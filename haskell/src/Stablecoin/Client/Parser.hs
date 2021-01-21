-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

{-# LANGUAGE ApplicativeDo #-}

module Stablecoin.Client.Parser
  ( BurnOptions(..)
  , ChangeMasterMinterOptions(..)
  , ChangePauserOptions(..)
  , ClientArgs(..)
  , ClientArgsRaw(..)
  , ConfigureMinterOptions(..)
  , ContractMetadataOptions(..)
  , DeployContractOptions(..)
  , GetBalanceOfOptions(..)
  , GetMintingAllowanceOptions(..)
  , GlobalOptions(..)
  , IsOperatorOptions(..)
  , MintOptions(..)
  , RemoveMinterOptions(..)
  , SetTransferlistOptions(..)
  , TransferOptions(..)
  , TransferOwnershipOptions(..)
  , UpdateOperatorsOptions(..)
  , stablecoinClientInfo
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Version (showVersion)
import Morley.Client (AddressOrAlias(..), Alias(..), MorleyClientConfig, clientConfigParser)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import Paths_stablecoin (version)
import Util.CLI (mkCLOptionParser)
import Util.Named ((:!), (.!))

import Lorentz.Contracts.Stablecoin (Expiry, UpdateOperatorData(..))

data ClientArgs = ClientArgs
  { caMorleyClientConfig :: MorleyClientConfig
  , caGlobalOptions :: GlobalOptions
  , caClientArgsRaw :: ClientArgsRaw
  }
  deriving stock Show

data GlobalOptions = GlobalOptions
  { goUser :: AddressOrAlias
  , goContract :: AddressOrAlias
  }
  deriving stock Show

data ClientArgsRaw
  = CmdDeployContract DeployContractOptions
  -- FA2 entrypoints
  | CmdTransfer TransferOptions
  | CmdGetBalanceOf GetBalanceOfOptions
  | CmdUpdateOperators UpdateOperatorsOptions
  | CmdIsOperator IsOperatorOptions
  -- Stablecoin entrypoints
  | CmdPause
  | CmdUnpause
  | CmdConfigureMinter ConfigureMinterOptions
  | CmdRemoveMinter RemoveMinterOptions
  | CmdMint MintOptions
  | CmdBurn BurnOptions
  | CmdTransferOwnership TransferOwnershipOptions
  | CmdAcceptOwnership
  | CmdChangeMasterMinter ChangeMasterMinterOptions
  | CmdChangePauser ChangePauserOptions
  | CmdSetTransferlist SetTransferlistOptions
  -- Query Commands
  | CmdGetBalance
  | CmdGetPaused
  | CmdGetContractOwner
  | CmdGetPendingContractOwner
  | CmdGetMasterMinter
  | CmdGetPauser
  | CmdGetTransferlist
  | CmdGetMintingAllowance GetMintingAllowanceOptions
  | CmdGetTokenMetadata
  deriving stock Show

data DeployContractOptions = DeployContractOptions
  { dcoMasterMinter :: AddressOrAlias
  , dcoContractOwner :: AddressOrAlias
  , dcoPauser :: AddressOrAlias
  , dcoTransferlist :: Maybe AddressOrAlias
  , dcoTokenSymbol :: Text
  , dcoTokenName :: Text
  , dcoTokenDecimals :: Natural
  , dcoReplaceAlias :: Bool
  , dcoDefaultExpiry :: Expiry
  , dcoContractMetadata :: ContractMetadataOptions
  }
  deriving stock Show

data TransferOptions = TransferOptions
  { toFrom :: AddressOrAlias
  , toTo :: AddressOrAlias
  , toAmount :: Natural
  }
  deriving stock Show

data GetBalanceOfOptions = GetBalanceOfOptions
  { gbooOwner :: AddressOrAlias
  }
  deriving stock Show

data UpdateOperatorsOptions = UpdateOperatorsOptions
  { uooUpdateOperators :: NonEmpty UpdateOperatorData
  }
  deriving stock Show

data IsOperatorOptions = IsOperatorOptions
  { iooOwner :: AddressOrAlias
  , iooOperator :: AddressOrAlias
  }
  deriving stock Show

data ConfigureMinterOptions = ConfigureMinterOptions
  { cmoMinter :: AddressOrAlias
  , cmoCurrentMintingAllowance :: Maybe Natural
  , cmoNewMintingAllowance :: Natural
  }
  deriving stock Show

data RemoveMinterOptions = RemoveMinterOptions
  { rmoMinter :: AddressOrAlias
  }
  deriving stock Show

data MintOptions = MintOptions
  { moTo :: AddressOrAlias
  , moAmount :: Natural
  }
  deriving stock Show

data BurnOptions = BurnOptions
  { boAmounts :: NonEmpty Natural
  }
  deriving stock Show

data TransferOwnershipOptions = TransferOwnershipOptions
  { tooTo :: AddressOrAlias
  }
  deriving stock Show

data ChangeMasterMinterOptions = ChangeMasterMinterOptions
  { cmmoTo :: AddressOrAlias
  }
  deriving stock Show

data ChangePauserOptions = ChangePauserOptions
  { cpoTo :: AddressOrAlias
  }
  deriving stock Show

data SetTransferlistOptions = SetTransferlistOptions
  { ssoTransferlist :: Maybe AddressOrAlias
  }
  deriving stock Show

data GetMintingAllowanceOptions = GetMintingAllowanceOptions
  { gmaoMinter :: AddressOrAlias
  }
  deriving stock Show

data ContractMetadataOptions
  = OpCurrentContract -- ^ Indicate that we should store metadata in the main contract itself.
      (Maybe Text)    -- ^ An optional contract description to add to the contract's metadata.
                      -- If none is given, a default description will be used.
  | OpRemoteContract  -- ^ Indicate we should originate a dedicated contract to store metadata.
      (Maybe Text)    -- ^ An optional contract description to add to the contract's metadata.
                      -- If none is given, a default description will be used.
  | OpRaw Text        -- ^ Indicate we should use the URI provided by the user as such.
  deriving stock (Show, Eq)

clientArgsParser :: Opt.Parser ClientArgs
clientArgsParser =
  ClientArgs
    <$> clientConfigParser (pure Nothing)
    <*> globalOptionsParser
    <*> clientArgsRawParser

globalOptionsParser :: Opt.Parser GlobalOptions
globalOptionsParser =
  GlobalOptions
    <$> addressOrAliasOption
      (Just $ AddressAlias (Alias "stablecoin-user"))
      (#name .! "user")
      (#help .! "User to send operations as")
    <*> addressOrAliasOption
      (Just $ AddressAlias (Alias "stablecoin"))
      (#name .! "contract")
      (#help .! "The stablecoin contract address/alias to use")

clientArgsRawParser :: Opt.Parser ClientArgsRaw
clientArgsRawParser = Opt.subparser $
  deployCmd
  <> transferCmd
  <> getBalanceOfCmd
  <> updateOperatorsCmd
  <> isOperatorCmd
  <> pauseCmd
  <> unpauseCmd
  <> configureMinterCmd
  <> removeMinterCmd
  <> mintCmd
  <> burnCmd
  <> transferOwnershipCmd
  <> acceptOwnershipCmd
  <> changeMasterMinterCmd
  <> changePauserCmd
  <> setTransferlistCmd
  <> getBalanceCmd
  <> getPausedCmd
  <> getContractOwnerCmd
  <> getPendingContractOwnerCmd
  <> getMasterMinterCmd
  <> getPauserCmd
  <> getTransferlistCmd
  <> getMintingAllowanceCmd
  <> getTokenMetadataCmd
  where
    deployCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    deployCmd =
      mkCommandParser
        "deploy"
        (CmdDeployContract <$> deployContractOptions)
        "Deploy Stablecoin contract to the chain. An alias 'stablecoin' and \
        \'stablecoin-tzip16-metadata' will be created by default."

    deployContractOptions :: Opt.Parser DeployContractOptions
    deployContractOptions = do
      dcoMasterMinter <-
        addressOrAliasOption
          Nothing
          (#name .! "master-minter")
          (#help .! "Address or alias of the 'Master Minter' role")
      dcoContractOwner <-
        addressOrAliasOption
          Nothing
          (#name .! "contract-owner")
          (#help .! "Address or alias of the 'Contract Owner' role")
      dcoPauser <-
        addressOrAliasOption
          Nothing
          (#name .! "pauser")
          (#help .! "Address or alias of the 'Pauser' role")
      dcoTransferlist <-
        optional $ addressOrAliasOption
          Nothing
          (#name .! "transferlist")
          (#help .! "Address or alias of the Transferlist contract")
      dcoTokenSymbol <-
        mkCLOptionParser
          (Just "TEST")
          (#name .! "token-symbol")
          (#help .! "Token symbol")
      dcoTokenName <-
        mkCLOptionParser
          (Just "Test")
          (#name .! "token-name")
          (#help .! "Token name")
      dcoTokenDecimals <-
        naturalOption
          (Just 8)
          (#name .! "token-decimals")
          (#help .! ("Number of digits to use after the decimal point " <>
                     "when displaying the token amounts"))
      dcoReplaceAlias <-
        Opt.switch $
          Opt.long "replace-alias" <>
          Opt.help
            "When this switch is set, and if the contract alias supplied \
            \by '--contract' already exists, the alias will be silently \
            \replaced. If it's not set, the user will be asked whether they \
            \want to replace it."
      dcoDefaultExpiry <-
        naturalOption
          Nothing
          (#name .! "default-expiry")
          (#help .! "Number of seconds it takes for a permit to expire")

      -- Where to place metadata, while keeping it embedded in
      -- contract being the default
      dcoContractMetadata <- fromMaybe (OpRemoteContract Nothing) <$> contractMetadataOptParser
      pure $ DeployContractOptions {..}

    contractMetadataOptParser :: Opt.Parser (Maybe ContractMetadataOptions)
    contractMetadataOptParser =
        optional (rawOptParser <|> remoteContractOptParser <|> inplaceOptParser)
      where
        rawOptParser :: Opt.Parser ContractMetadataOptions
        rawOptParser = OpRaw <$> (mkCLOptionParser
          Nothing
          (#name .! "contract-metadata-off-chain")
          (#help .! "Use the provided off-chain URI for metadata URI"))

        inplaceOptParser :: Opt.Parser ContractMetadataOptions
        inplaceOptParser = do
          _ <-
            Opt.flag' ()
              (Opt.long "contract-metadata-in-place"
              <> Opt.help
                "Embed the metadata for the contract in the contract's storage itself.")
          desc <- contractDescriptionParser
          pure $ OpCurrentContract desc

        remoteContractOptParser :: Opt.Parser ContractMetadataOptions
        remoteContractOptParser = OpRemoteContract <$> contractDescriptionParser

        contractDescriptionParser :: Opt.Parser (Maybe Text)
        contractDescriptionParser =
          optional $ mkCLOptionParser @Text
            Nothing
            (#name .! "description")
            (#help .! "An optional contract description to add to the contract's metadata. \
                      \If none is specified, a generic default description will be used. \
                      \When the '--contract-metadata-in-place' option is used, the \
                      \'--description' option must come after it.")

    transferCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    transferCmd =
      mkCommandParser
        "transfer"
        (CmdTransfer <$> transferOptions) $
        "Transfer the specified amount of tokens to the given account"

    transferOptions :: Opt.Parser TransferOptions
    transferOptions = do
      toFrom <-
        addressOrAliasOption
          Nothing
          (#name .! "from")
          (#help .! "Address of the account to take tokens from")
      toTo <-
        addressOrAliasOption
          Nothing
          (#name .! "to")
          (#help .! "Address of the account to given tokens to")
      toAmount <-
        naturalOption
          Nothing
          (#name .! "amount")
          (#help .! "Amount of tokens to transfer")
      pure $ TransferOptions {..}

    getBalanceOfCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getBalanceOfCmd =
      mkCommandParser
        "get-balance-of"
        (CmdGetBalanceOf <$> getBalanceOfOptions) $
        "Get the balance of the given accounts."

    getBalanceOfOptions :: Opt.Parser GetBalanceOfOptions
    getBalanceOfOptions = do
      gbooOwner <-
        addressOrAliasOption
          Nothing
          (#name .! "owner")
          (#help .! "Address of the account whose balance will be retrieved.")
      pure $ GetBalanceOfOptions {..}

    updateOperatorsCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    updateOperatorsCmd =
      mkCommandParser
        "update-operators"
        (CmdUpdateOperators <$> updateOperatorsOptions) $
        "Add or remove operators to the account indicated by '--user'"

    updateOperatorsOptions :: Opt.Parser UpdateOperatorsOptions
    updateOperatorsOptions = do
      uooUpdateOperators <-
        nonEmptyParser $
          (AddOperator <$> addressOrAliasOption
            Nothing
            (#name .! "add")
            (#help .! ("Address of the operator to add to the account indicated by '--user'. " <>
                      "This option can be repeated many times.")))
          <|>
          (RemoveOperator <$> addressOrAliasOption
            Nothing
            (#name .! "remove")
            (#help .! ("Address of the operator to remove from the account indicated by '--user'. " <>
                      "This option can be repeated many times.")))
      pure $ UpdateOperatorsOptions {..}

    isOperatorCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    isOperatorCmd =
      mkCommandParser
        "is-operator"
        (CmdIsOperator <$> isOperatorOptions) $
        "Check if a given address is an operator for a given account"

    isOperatorOptions :: Opt.Parser IsOperatorOptions
    isOperatorOptions = do
      iooOwner <-
        addressOrAliasOption
          Nothing
          (#name .! "owner")
          (#help .! "Address of the account whose operators will be checked")
      iooOperator <-
        addressOrAliasOption
          Nothing
          (#name .! "operator")
          (#help .! "Address of the potential operator")
      pure $ IsOperatorOptions {..}

    pauseCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    pauseCmd =
      mkCommandParser
        "pause"
        (pure CmdPause) $
        "Pauses transferring, burning and minting operations so that they " <>
        "cannot be performed. All other operations remain unaffected."

    unpauseCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    unpauseCmd =
      mkCommandParser
        "unpause"
        (pure CmdUnpause) $
        "Unpauses the contract so that transferring, burning and minting " <>
        "operations can be performed by users with corresponding roles."

    configureMinterCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    configureMinterCmd =
      mkCommandParser
        "configure-minter"
        (CmdConfigureMinter <$> configureMinterOptions) $
        "Adds a minter to the minter list to allow them to mint tokens " <>
        "(if minter is not in the list already)."

    configureMinterOptions :: Opt.Parser ConfigureMinterOptions
    configureMinterOptions = do
      cmoMinter <-
        addressOrAliasOption
          Nothing
          (#name .! "minter")
          (#help .! "Address or alias of the minter to add")
      cmoCurrentMintingAllowance <-
        optional $ naturalOption
          Nothing
          (#name .! "current-minting-allowance")
          (#help .! ("The current minting allowance. If it does not match " <>
                     "the actual minting allowance, the operation fails."))
      cmoNewMintingAllowance <-
        naturalOption
          Nothing
          (#name .! "new-minting-allowance")
          (#help .! "The minting allowance for this minter")
      pure $ ConfigureMinterOptions {..}

    removeMinterCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    removeMinterCmd =
      mkCommandParser
        "remove-minter"
        (CmdRemoveMinter <$> removeMinterOptions) $
        "Removes a minter from the minter list and sets its minting allowance to 0. " <>
        "Once minter is removed it will no longer be able to mint or burn tokens."

    removeMinterOptions :: Opt.Parser RemoveMinterOptions
    removeMinterOptions = do
      rmoMinter <-
        addressOrAliasOption
          Nothing
          (#name .! "minter")
          (#help .! "Address or alias of the minter to remove")
      pure $ RemoveMinterOptions {..}

    mintCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    mintCmd =
      mkCommandParser
        "mint"
        (CmdMint <$> mintOptions) $
        "Produces the given amounts of tokens to the wallets associated " <>
        "with the given addresses."

    mintOptions :: Opt.Parser MintOptions
    mintOptions = do
      moTo <-
        addressOrAliasOption
          Nothing
          (#name .! "to")
          (#help .! "Address or alias to which the tokens will be transferred")
      moAmount <-
        naturalOption
          Nothing
          (#name .! "amount")
          (#help .! "The amount of tokens to produce")
      pure $ MintOptions {..}

    burnCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    burnCmd =
      mkCommandParser
        "burn"
        (CmdBurn <$> burnOptions) $
        "Decreases balance for sender and the total supply of tokens by " <>
        "the sum of given amounts."

    burnOptions :: Opt.Parser BurnOptions
    burnOptions = do
      boAmounts <-
        nonEmptyParser $
          naturalOption
            Nothing
            (#name .! "amount")
            (#help .! "Amount of tokens to burn. This option can be repeated many times.")
      pure $ BurnOptions {..}

    mkCommandParser
      :: String
      -> Opt.Parser a
      -> String
      -> Opt.Mod Opt.CommandFields a
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    transferOwnershipCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    transferOwnershipCmd =
      mkCommandParser
        "transfer-ownership"
        (CmdTransferOwnership <$> transferOwnershipOptions)
        "Initiate transfer of contract ownership to a new address."

    transferOwnershipOptions :: Opt.Parser TransferOwnershipOptions
    transferOwnershipOptions = do
      tooTo <-
        addressOrAliasOption
          Nothing
          (#name .! "to")
          (#help .! "Address or alias to which ownership will be transferred")
      pure $ TransferOwnershipOptions {..}

    acceptOwnershipCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    acceptOwnershipCmd =
      mkCommandParser
        "accept-ownership"
        (pure CmdAcceptOwnership)
        "Accept contract ownership privileges."

    changeMasterMinterCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    changeMasterMinterCmd =
      mkCommandParser
        "change-master-minter"
        (CmdChangeMasterMinter <$> changeMasterMinterOptions)
        "Set master minter to a new address."

    changeMasterMinterOptions :: Opt.Parser ChangeMasterMinterOptions
    changeMasterMinterOptions = do
      cmmoTo <-
        addressOrAliasOption
          Nothing
          (#name .! "to")
          (#help .! "Address or alias to which the 'master minter' role will be transferred")
      pure $ ChangeMasterMinterOptions {..}

    changePauserCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    changePauserCmd =
      mkCommandParser
        "change-pauser"
        (CmdChangePauser <$> changePauserOptions)
        "Set pauser to a new address."

    changePauserOptions :: Opt.Parser ChangePauserOptions
    changePauserOptions = do
      cpoTo <-
        addressOrAliasOption
          Nothing
          (#name .! "to")
          (#help .! "Address or alias to which the 'pauser' role will be transferred")
      pure $ ChangePauserOptions {..}

    setTransferlistCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    setTransferlistCmd =
      mkCommandParser
        "set-transferlist"
        (CmdSetTransferlist <$> setTransferlistOptions)
        "Set the stored (optional) transferlist address to the new one."

    setTransferlistOptions :: Opt.Parser SetTransferlistOptions
    setTransferlistOptions = do
      ssoTransferlist <-
        optional $ addressOrAliasOption
          Nothing
          (#name .! "transferlist")
          (#help .! "Address or alias of the new transferlist")
      pure $ SetTransferlistOptions {..}

    getBalanceCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getBalanceCmd =
      mkCommandParser
        "get-balance"
        (pure CmdGetBalance)
        "Get the contract's balance"

    getPausedCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getPausedCmd =
      mkCommandParser
        "get-paused"
        (pure CmdGetPaused)
        "Check whether the contract is paused"

    getContractOwnerCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getContractOwnerCmd =
      mkCommandParser
        "get-contract-owner"
        (pure CmdGetContractOwner)
        "Get the contract owner's address"

    getPendingContractOwnerCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getPendingContractOwnerCmd =
      mkCommandParser
        "get-pending-contract-owner"
        (pure CmdGetPendingContractOwner)
        "Check if there's a pending contract owner"

    getMasterMinterCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getMasterMinterCmd =
      mkCommandParser
        "get-master-minter"
        (pure CmdGetMasterMinter)
        "Get the master minter's address"

    getPauserCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getPauserCmd =
      mkCommandParser
        "get-pauser"
        (pure CmdGetPauser)
        "Get the pauser's address"

    getTransferlistCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTransferlistCmd =
      mkCommandParser
        "get-transferlist"
        (pure CmdGetTransferlist)
        "Get the transferlist's address"

    getMintingAllowanceCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getMintingAllowanceCmd =
      mkCommandParser
        "get-minting-allowance"
        (CmdGetMintingAllowance <$> getMintingAllowanceOptions)
        "Get the minting allowance for a minter"

    getMintingAllowanceOptions :: Opt.Parser GetMintingAllowanceOptions
    getMintingAllowanceOptions = do
      gmaoMinter <-
        addressOrAliasOption
          Nothing
          (#name .! "minter")
          (#help .! "Address or alias of the minter whose allowance will be retrieved")
      pure $ GetMintingAllowanceOptions {..}

    getTokenMetadataCmd :: Opt.Mod Opt.CommandFields ClientArgsRaw
    getTokenMetadataCmd =
      mkCommandParser
        "get-token-metadata"
        (pure CmdGetTokenMetadata)
        "Get the token metadata"

stablecoinClientInfo :: Opt.ParserInfo ClientArgs
stablecoinClientInfo =
  Opt.info (Opt.helper <*> versionOption <*> clientArgsParser) $
  mconcat
    [ Opt.fullDesc
    , Opt.progDesc $
        "Tezos Stablecoin Client: Command line interface for interacting " <>
        "with the Tezos Stablecoin contract"
    , Opt.header "Stablecoin Client"
    , Opt.footerDoc $ usageDoc
    ]

  where
    versionOption =
      Opt.infoOption
        ("stablecoin-" <> showVersion version)
        (Opt.long "version" <> Opt.help "Show version.")

    usageDoc :: Maybe Doc
    usageDoc =
      Just $ mconcat
      [ "You can use help for specific COMMAND", linebreak
      , "EXAMPLE:", linebreak
      , "  stablecoin-client mint --help", linebreak
      , "USAGE EXAMPLE:", linebreak
      , "  stablecoin-client mint --to tz1U1h1YzBJixXmaTgpwDpZnbrYHX3fMSpvb --amount 100500", linebreak
      , linebreak
      , "  This command will produce the given amounts of tokens to", linebreak
      , "  the wallets associated with the given address.", linebreak
      ]

addressOrAliasOption ::
  Maybe AddressOrAlias -> "name" :! String -> "help" :! String -> Opt.Parser AddressOrAlias
addressOrAliasOption = mkCLOptionParser

naturalOption ::
  Maybe Natural -> "name" :! String -> "help" :! String -> Opt.Parser Natural
naturalOption = mkCLOptionParser

nonEmptyParser :: Opt.Parser a -> Opt.Parser (NonEmpty a)
nonEmptyParser p =
  -- Note: `(:|) <$> p <*> many p` does not work here, because then optparse-applicative
  -- will display the option's info twice when --help is invoked.
  -- For this reason, we have to use `some` and the "unsafe" `NonEmpty.fromList`.
  --
  -- TODO: When the next version of `optparse-applicative` is released,
  -- we'll be able to delete this function and use `some1` instead.
  -- See:
  --   * https://github.com/pcapriotti/optparse-applicative/issues/376
  --   * https://github.com/pcapriotti/optparse-applicative/pull/382
  NonEmpty.fromList <$> some p
