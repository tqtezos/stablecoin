-- SPDX-FileCopyrightText: 2022 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{-# LANGUAGE InstanceSigs #-}

-- TODO [morley#889]:
-- Delete this module when https://gitlab.com/morley-framework/morley/-/issues/889 is merged.
module Stablecoin.Client.L1AddressOrAlias
  ( L1AddressOrAlias(..)
  , ImplicitAddressOrAlias
  , ContractAddressOrAlias
  , KindedAddressOrAlias(..)
  -- * Parsers
  , l1AddressOrAliasOption
  , implicitAddressOrAliasOption
  , contractAddressOrAliasOption
  -- * Resolve address
  , ResolveAddress(..)
  , resolveAddress
  -- * Helpers
  , convertImplicitAddressOrAlias
  ) where

import Colourista (formatWith)
import Colourista.Pure (red)
import Control.Exception (IOException, throwIO)
import Data.ByteArray (ScrubbedBytes)
import Data.Char qualified as Char
import Data.Constraint
import Data.Map qualified as Map
import Data.Singletons (demote)
import Data.Text qualified as T
import Data.Typeable (type (:~:)(..))
import Fmt
import Morley.Client (MorleyClientM)
import Morley.Client.Logging (WithClientLog, logDebug)
import Morley.Client.TezosClient
  (HasTezosClientEnv(..), TezosClientEnv(..), TezosClientError(..), toCmdArg)
import Morley.Client.Util (scrubbedBytesToString)
import Morley.Michelson.Typed (SingI, sing)
import Morley.Tezos.Address
import Morley.Tezos.Address.Alias hiding
  (AddressOrAlias, ContractAddressOrAlias, ImplicitAddressOrAlias)
import Morley.Tezos.Address.Alias qualified as Morley
import Morley.Tezos.Address.Kinds
import Morley.Util.CLI (HasCLReader(..), maybeAddDefault, mkCLOptionParser)
import Morley.Util.Interpolate (itu)
import Morley.Util.Named (arg, type (:!))
import Morley.Util.Sing (eqI)
import Options.Applicative qualified as Opt
import Options.Applicative.Help qualified as Pretty
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Cleveland (toL1Address)
import Unsafe qualified

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

{- This type is meant to be used to parse CLI options where either an address or an alias
of either a contract or an implicit account can be accepted.

Example inputs:
  * "KT1STb2aG7NpoBBNRggvummqsxNQZmuAVFvG": an address belonging to a contract
  * "tz1hZ7o4bhFTo6AXpWZsXzbnddEK3dSCv1S8": an address belonging to an implicit account
  * "contract:some-alias": an alias that is expected to be associated with a contract.
      * If it's associated with an implicit account, `resolveAddress` will fail.
  * "implicit:some-alias": an alias that is expected to be associated with an implicit account.
      * If it's associated with a contract, `resolveAddress` will fail.
  * "some-alias": an alias that is expected to be associated with either a contract or an implicit account.
      * If it's associated with **both** a contract and an implicit account, `resolveAddress` will fail.

Refer to the `HasCLReader` and `ResolveAddress` instances for implementation details.
-}
data L1AddressOrAlias where
  L1AOAKindSpecified :: KindedAddressOrAlias kind -> L1AddressOrAlias
  L1AOAKindUnspecified :: Text -> L1AddressOrAlias

deriving stock instance Show L1AddressOrAlias

-- | The output of `build` should be parseable by the `HasCLReader` instance.
instance Buildable L1AddressOrAlias where
  build = \case
    L1AOAKindUnspecified alias -> build alias
    L1AOAKindSpecified aoa -> build aoa

data KindedAddressOrAlias (kind :: AddressKind) where
  KAOAAddress :: L1AddressKind kind => KindedAddress kind -> KindedAddressOrAlias kind
  KAOAAlias :: Alias kind -> KindedAddressOrAlias kind

deriving stock instance Show (KindedAddressOrAlias kind)

-- | The output of `build` should be parseable by the `HasCLReader` instance.
instance Buildable (KindedAddressOrAlias kind) where
  build = \case
    KAOAAddress address -> build address
    KAOAAlias alias@ImplicitAlias{} -> "implicit:" <> build alias
    KAOAAlias alias@ContractAlias{} -> "contract:" <> build alias

{- This type is meant to be used to parse CLI options where either an address or an alias
of a contract can be accepted.

Example inputs:
  * "KT1STb2aG7NpoBBNRggvummqsxNQZmuAVFvG": an address belonging to a contract.
  * "contract:some-alias" or "some-alias": an alias that is expected to be associated with a contract.
      * If it's associated with an implicit account, `resolveAddress` will fail.
      * If it's associated with **both** a contract and an implicit account,
        `resolveAddress` will return the contract address.

Parsing will fail on these inputs:
  * "tz1hZ7o4bhFTo6AXpWZsXzbnddEK3dSCv1S8"
  * "implicit:some-alias"

Refer to the `HasCLReader` and `ResolveAddress` instances for implementation details.
-}
type ContractAddressOrAlias = KindedAddressOrAlias 'AddressKindContract

{- This type is meant to be used to parse CLI options where either an address or an alias
of an implicit account can be accepted.

Example inputs:
  * "tz1hZ7o4bhFTo6AXpWZsXzbnddEK3dSCv1S8": an address belonging to an implicit account.
  * "implicit:some-alias" or "some-alias": an alias that is expected to be associated with an implicit account.
      * If it's associated with a contract, `resolveAddress` will fail.
      * If it's associated with **both** a contract and an implicit account,
        `resolveAddress` will return the implicit account address.

Parsing will fail on these inputs:
  * "KT1STb2aG7NpoBBNRggvummqsxNQZmuAVFvG"
  * "contract:some-alias"

Refer to the `HasCLReader` and `ResolveAddress` instances for implementation details.
-}
type ImplicitAddressOrAlias = KindedAddressOrAlias 'AddressKindImplicit

----------------------------------------------------------------------------
-- Reader
----------------------------------------------------------------------------

addressOrAliasKindSanity :: forall kind. KindedAddressOrAlias kind -> Dict (L1AddressKind kind, SingI kind)
addressOrAliasKindSanity = \case
  KAOAAddress addr -> Dict \\ addressKindSanity addr
  KAOAAlias alias -> aliasKindSanity alias

instance forall kind. (SingI kind, L1AddressKind kind) => HasCLReader (KindedAddressOrAlias kind) where
  getMetavar = (Char.toUpper <$> pretty (demote @kind)) <> " ADDRESS OR ALIAS"
  getReader =
    getReader @L1AddressOrAlias >>= \case
      L1AOAKindSpecified (aoa :: KindedAddressOrAlias kind') ->
        withDict (addressOrAliasKindSanity aoa)
          case eqI @kind @kind' of
            Just Refl -> pure aoa
            Nothing -> Opt.readerError $ pretty $ nameF "Unexpected address kind" $
              "expected " +| demote @kind |+ " address or alias, but got: '" +| aoa |+ "'"
      L1AOAKindUnspecified aliasText ->
          case sing @kind of
            SAddressKindContract -> pure $ KAOAAlias $ ContractAlias aliasText
            SAddressKindImplicit -> pure $ KAOAAlias $ ImplicitAlias aliasText

instance HasCLReader L1AddressOrAlias where
  getMetavar = "L1 ADDRESS OR ALIAS"
  getReader =
    Opt.str >>= \str ->
      case parseAddress str of
        Right (MkConstrainedAddress addr) ->
          case addr of
            ImplicitAddress{} -> pure $ L1AOAKindSpecified $ KAOAAddress addr
            ContractAddress{} -> pure $ L1AOAKindSpecified $ KAOAAddress addr
            TxRollupAddress{} -> Opt.readerError $ "Unexpected transaction rollup address: " <> pretty addr
        Left _ ->
          pure $
            parseImplicitAlias str <|> parseContractAlias str
              & fromMaybe (L1AOAKindUnspecified str)
      where
        parseImplicitAlias :: Text -> Maybe L1AddressOrAlias
        parseImplicitAlias str =
          T.stripPrefix "implicit:" str <&> \implicitAlias ->
            L1AOAKindSpecified $ KAOAAlias $ ImplicitAlias implicitAlias

        parseContractAlias :: Text -> Maybe L1AddressOrAlias
        parseContractAlias str =
          T.stripPrefix "contract:" str <&> \contractAlias ->
            L1AOAKindSpecified $ KAOAAlias $ ContractAlias contractAlias

-- | Generic parser to read an option of 'L1AddressOrAlias' type.
l1AddressOrAliasOption
  :: Maybe L1AddressOrAlias
  -> "name" :! String
  -> "help" :! String
  -> Opt.Parser L1AddressOrAlias
l1AddressOrAliasOption defValue (arg #name -> name) (arg #help -> hInfo) =
  Opt.option (getReader @L1AddressOrAlias) $ mconcat
    [ Opt.metavar (getMetavar @L1AddressOrAlias)
    , Opt.long name
    -- Note: We use `paragraph` to word-wrap the help text.
    , Opt.helpDoc $ Just $ Pretty.vsep $ (Pretty.extractChunk . Pretty.paragraph) <$>
      [ hInfo
      , "When using an alias that is assigned to both a contract and an implicit account,\
        \ use the prefix 'contract:' or 'implicit:' to disambiguate."
      ]
    , maybeAddDefault pretty defValue
    ]

implicitAddressOrAliasOption
  :: Maybe ImplicitAddressOrAlias
  -> "name" :! String
  -> "help" :! String
  -> Opt.Parser ImplicitAddressOrAlias
implicitAddressOrAliasOption = mkCLOptionParser

contractAddressOrAliasOption
  :: Maybe ContractAddressOrAlias
  -> "name" :! String
  -> "help" :! String
  -> Opt.Parser ContractAddressOrAlias
contractAddressOrAliasOption = mkCLOptionParser

-- TODO: delete this workaround
convertImplicitAddressOrAlias :: ImplicitAddressOrAlias -> MorleyClientM Morley.ImplicitAddressOrAlias
convertImplicitAddressOrAlias aoa = do
  resolveAddress aoa >>= \case
    addr@ImplicitAddress{} -> pure $ AddressResolved addr

----------------------------------------------------------------------------
-- Resolve AddressOrAlias
----------------------------------------------------------------------------

class ResolveAddress addressOrAlias where
  type Resolved addressOrAlias :: Type
  resolveAddressMaybe :: addressOrAlias -> MorleyClientM (Maybe (Resolved addressOrAlias))

instance ResolveAddress (KindedAddressOrAlias kind) where
  type Resolved (KindedAddressOrAlias kind) = KindedAddress kind

  resolveAddressMaybe :: KindedAddressOrAlias kind -> MorleyClientM (Maybe (KindedAddress kind))
  resolveAddressMaybe = \case
    KAOAAddress addr -> pure $ Just addr
    aoa@(KAOAAlias alias) -> do
      findAlias (unAlias alias) >>= pickKind @kind aoa \\ aliasKindSanity alias

instance ResolveAddress L1AddressOrAlias where
  type Resolved L1AddressOrAlias = L1Address

  resolveAddressMaybe :: L1AddressOrAlias -> MorleyClientM (Maybe L1Address)
  resolveAddressMaybe = \case
    L1AOAKindUnspecified aliasText -> do
      findAlias aliasText >>= failOnAmbiguousAlias aliasText
    L1AOAKindSpecified aoa ->
      fmap toL1Address <$> resolveAddressMaybe aoa \\ addressOrAliasKindSanity aoa

resolveAddress
  :: (ResolveAddress addressOrAlias, Buildable addressOrAlias)
  => addressOrAlias
  -> MorleyClientM (Resolved addressOrAlias)
resolveAddress aoa =
  resolveAddressMaybe aoa >>= \case
    Nothing -> throwM $ UnknownAddressAlias (pretty aoa)
    Just existingAddress -> return existingAddress

----------------------------------------------------------------------------
-- "Find alias" helpers
----------------------------------------------------------------------------

data FindAliasResult
  = FARUnambiguous L1Address
  | FARAmbiguous ContractAddress ImplicitAddress
  | FARNone

{- Finds the implicit/contract addresses assigned to the given alias.

Note that an alias can be ambiguous: it can refer to BOTH a contract and an implicit account.
When an alias "abc" is ambiguous, "tezos-client list known contracts" will return
two entries with the following format:

> abc: KT1...
> key:abc: tz1...

So, in order to check whether the alias is ambiguous, we check whether
both "abc" and "key:abc" are present in the output.

If only "abc" is present, then we know it's not ambiguous (and it refers to EITHER a contract or an implicit account).
-}
findAlias :: Text -> MorleyClientM FindAliasResult
findAlias aliasText = do
  logDebug $ "Resolving " +| aliasText |+ ""
  aliasesAndAddresses <- parseOutput <$> callListKnown "contracts"

  case Map.lookup aliasText aliasesAndAddresses of
    Nothing -> pure FARNone
    Just firstMatch ->
      case Map.lookup ("key:" <> aliasText) aliasesAndAddresses of
        Nothing -> FARUnambiguous <$> parseL1Address firstMatch
        Just secondMatchImplicit -> do
          contractAddr <-
            either (throwM . TezosClientParseAddressError firstMatch) pure $
              parseKindedAddress @'AddressKindContract firstMatch
          implicitAddr <-
            either (throwM . TezosClientParseAddressError secondMatchImplicit) pure $
              parseKindedAddress @'AddressKindImplicit secondMatchImplicit
          pure $ FARAmbiguous contractAddr implicitAddr
  where
    parseOutput :: Text -> Map Text Text
    parseOutput str =
      str
        & lines
        <&> parseLine
        & Map.fromList

    -- Note: each line has the format "<alias>: <address>"
    parseLine :: Text -> (Text, Text)
    parseLine = first (T.dropEnd 2) . T.breakOnEnd ": "

    parseL1Address :: Text -> MorleyClientM L1Address
    parseL1Address addrText =
      case parseAddress addrText of
        Left parseErr -> throwM $ TezosClientParseAddressError addrText parseErr
        Right (MkAddress addr@ContractAddress{}) -> pure $ toL1Address addr
        Right (MkAddress addr@ImplicitAddress{}) -> pure $ toL1Address addr
        Right (MkAddress addr) ->
          -- TODO: add a constructor to `TezosClientError`, and use `throwM`.
          liftIO $ fail
            [itu|
              Expected the alias '#{aliasText}' to be assigned to either a contract or an implicit account,
              but it's assigned to a transaction rollup address: #{addr}.
              |]

failOnAmbiguousAlias :: Text -> FindAliasResult -> MorleyClientM (Maybe L1Address)
failOnAmbiguousAlias aliasText = \case
  FARNone -> pure Nothing
  FARUnambiguous addr -> pure $ Just addr
  FARAmbiguous contractAddr implicitAddr ->
    -- TODO: add a constructor to `TezosClientError`, and use `throwM`.
    liftIO $ fail
      [itu|
        The alias '#{aliasText}' is assigned to both:
          * a contract address: #{contractAddr}
          * and an implicit address: #{implicitAddr}
        Use 'contract:#{aliasText}' or 'implicit:#{aliasText}' to disambiguate.
        |]

{- If the given alias is associated with an address of the requested @kind@, return it.

Otherwise, if it's associated with an address of some other kind, throw an error.
If it's not associated with any address at all, return `Nothing`.
-}
pickKind
  :: forall kind addressOrAlias
   . (L1AddressKind kind, SingI kind, Buildable addressOrAlias)
  => addressOrAlias
  -> FindAliasResult
  -> MorleyClientM (Maybe (KindedAddress kind))
pickKind aoa = \case
  FARNone -> pure Nothing
  FARAmbiguous contractAddr implicitAddr ->
    usingImplicitOrContractKind @kind
      case sing @kind of
        SAddressKindContract -> pure $ Just contractAddr
        SAddressKindImplicit -> pure $ Just implicitAddr
  FARUnambiguous (MkConstrainedAddress (addr :: KindedAddress actualKind)) ->
    withDict (addressKindSanity addr) $
      case eqI @actualKind @kind of
        Just Refl -> pure $ Just addr
        Nothing -> do
          let demotedKind = demote @kind
          let demotedActualKind = demote @actualKind
          liftIO $ fail
            [itu|
              Expected the alias '#{aoa}' to be assigned to an address of kind '#{demotedKind}',
              but it's assigned to an address of kind '#{demotedActualKind}': #{addr}.
              |]

----------------------------------------------------------------------------
-- Copy-pasted from morley-client
----------------------------------------------------------------------------

-- | Call tezos-client to list known addresses or contracts
callListKnown
  :: (WithClientLog env m, HasTezosClientEnv env, MonadIO m, MonadCatch m)
  => String -> m Text
callListKnown objects =
  callTezosClientStrict ["list", "known", objects] MockupMode Nothing

-- | Datatype that represents modes for calling node from @tezos-client@.
data CallMode
  = MockupMode
  -- ^ Mode in which @tezos-client@ doesn't perform any actual RPC calls to the node
  -- and use mock instead.
  | ClientMode
  -- ^ Normal mode in which @tezos-client@ performs all necessary RPC calls to the node.

-- | Call @tezos-client@ with given arguments. Arguments defined by
-- config are added automatically. The second argument specifies what
-- should be done in failure case. It takes stdout and stderr
-- output. Possible handling:
--
-- 1. Parse a specific error and throw it.
-- 2. Parse an expected error that shouldn't cause a failure.
-- Return @True@ in this case.
-- 3. Detect an unexpected error, return @False@.
-- In this case 'UnexpectedClientFailure' will be throw.
callTezosClient
  :: forall env m. (WithClientLog env m, HasTezosClientEnv env, MonadIO m, MonadCatch m)
  => (Text -> Text -> IO Bool) -> [String] -> CallMode -> Maybe ScrubbedBytes -> m Text
callTezosClient errHandler args mode mbInput = retryEConnreset mode $ do
  TezosClientEnv {..} <- view tezosClientEnvL
  let
    extraArgs :: [String]
    extraArgs = mconcat
      [ ["-E", toCmdArg tceEndpointUrl]
      , maybe [] (\dir -> ["-d", dir]) tceMbTezosClientDataDir
      , ["--mode", case mode of
            MockupMode -> "mockup"
            ClientMode -> "client"
        ]
      ]

    allArgs = extraArgs ++ args
  logDebug $ "Running: " <> unwords (toText <$> tceTezosClientPath:allArgs)
  let
    ifNotEmpty prefix output
      | null output = ""
      | otherwise = prefix <> ":\n" <> output
    logOutput :: Text -> Text -> m ()
    logOutput output errOutput = logDebug $
      ifNotEmpty "stdout" output <>
      ifNotEmpty "stderr" errOutput

  liftIO (readProcessWithExitCode' tceTezosClientPath allArgs
          (maybe "" scrubbedBytesToString mbInput)) >>= \case
    (ExitSuccess, toText -> output, toText -> errOutput) ->
      output <$ logOutput output errOutput
    (ExitFailure errCode, toText -> output, toText -> errOutput) -> do
      checkCounterError errOutput
      checkEConnreset errOutput
      liftIO $ unlessM (errHandler output errOutput) $
        throwM $ UnexpectedClientFailure errCode output errOutput

      output <$ logOutput output errOutput
  where
    checkCounterError
      :: Text -> m ()
    checkCounterError errOutput |
      "Counter" `T.isPrefixOf` errOutput && "already used for contract" `T.isInfixOf` errOutput = do
        let splittedErrOutput = words errOutput
        liftIO $ throwM $
          CounterIsAlreadyUsed (splittedErrOutput Unsafe.!! 1) (splittedErrOutput Unsafe.!! 5)
    checkCounterError _ = pass
    checkEConnreset :: Text -> m ()
    checkEConnreset errOutput
      | "Unix.ECONNRESET" `T.isInfixOf` errOutput = throwM EConnreset
    checkEConnreset _ = pass

    -- Helper function that retries @tezos-client@ call action in case of @ECONNRESET@.
    -- Note that this error cannot appear in case of 'MockupMode' call.
    retryEConnreset :: CallMode -> m a -> m a
    retryEConnreset MockupMode action = action
    retryEConnreset ClientMode action = retryEConnresetImpl 0 action

    retryEConnresetImpl :: Integer -> m a -> m a
    retryEConnresetImpl attempt action = action `catch` \err -> do
      case err of
        EConnreset ->
          if attempt >= maxRetryAmount then throwM err
          else retryEConnresetImpl (attempt + 1) action
        anotherErr -> throwM anotherErr

    maxRetryAmount = 5

-- | Call tezos-client and expect success.
callTezosClientStrict
  :: (WithClientLog env m, HasTezosClientEnv env, MonadIO m, MonadCatch m)
  => [String] -> CallMode -> Maybe ScrubbedBytes -> m Text
callTezosClientStrict args mode mbInput = callTezosClient errHandler args mode mbInput
  where
    errHandler _ _ = pure False

-- | Variant of @readProcessWithExitCode@ that prints a better error in case of
-- an exception in the inner @readProcessWithExitCode@ call.
readProcessWithExitCode'
  :: FilePath
  -> [String]
  -> String
  -> IO (ExitCode, String, String)
readProcessWithExitCode' fp args inp =
  catch
    (readProcessWithExitCode fp args inp) handler
  where
    handler :: IOException -> IO (ExitCode, String, String)
    handler e = do
      hPutStrLn @Text stderr $ formatWith [red] errorMsg
      throwIO e

    errorMsg =
      "ERROR!! There was an error in executing `" <> toText fp <> "` program. Is the \
      \ executable available in PATH ?"
