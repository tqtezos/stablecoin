-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Stablecoin
  ( ConfigureMinterParam(..)
  , MetadataRegistryStorage
  , ChangeMasterMinterParam
  , ChangePauserParam
  , SetTransferlistParam
  , RemoveMinterParam
  , MetadataUri(..)
  , mkContractMetadataRegistryStorage
  , defaultContractMetadataStorage
  , MintParams
  , MintParam(..)
  , BurnParams
  , ParameterC
  , Parameter (..)
  , Storage'(..)
  , Storage
  , StorageView
  , UserPermits(..)
  , UpdateOperatorData(..)
  , PermitInfo(..)
  , Roles(..)

  , TransferOwnershipParam
  , Expiry
  , PermitHash(..)
  , mkPermitHash
  , PermitParam(..)
  , SetExpiryParam(..)
  , minterLimit

  -- * TZIP-16
  , ParsedMetadataUri(..)
  , metadataJSON
  , metadataMap
  , parseMetadataUri

  -- * Embedded LIGO contracts
  , contractMetadataContract
  , stablecoinContract
  ) where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed (embedStringFile)
import qualified Data.Map as Map
import Data.Version (showVersion)
import Fmt
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (string')
import qualified Text.Show

import Lorentz as L
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Spec.TZIP16Interface
  (Error(..), License(..), Metadata(..), MetadataMap, Source(..), ViewImplementation(..))
import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZ
import Michelson.Test.Import (readContract)
import Michelson.Typed (Notes(..))
import qualified Michelson.Typed as T
import Michelson.Untyped (noAnn)
import Morley.Client (AddressOrAlias(..), BigMapId(..))
import Morley.Metadata (ViewCode(..), mkMichelsonStorageView)
import Morley.Micheline (ToExpression(toExpression))
import Tezos.Address (formatAddress, parseAddress)
import qualified Tezos.Crypto as Hash

import Lorentz.Contracts.StablecoinPath (metadataRegistryContractPath, stablecoinPath)
import Paths_stablecoin (version)

------------------------------------------------------------------
-- Parameter

-- | Data needed to add or remove an operator.
data UpdateOperatorData
  = AddOperator AddressOrAlias
  | RemoveOperator AddressOrAlias
  deriving stock Show

data ConfigureMinterParam = ConfigureMinterParam
 { cmpMinter :: Address
 , cmpCurrentMintingAllowance :: Maybe Natural
 , cmpNewMintingAllowance :: Natural
 }
 deriving stock (Generic, Show)
 deriving anyclass (IsoValue, HasAnnotation)

instance Buildable ConfigureMinterParam where
  build = genericF

type RemoveMinterParam = Address

data MintParam = MintParam
  { mpTo :: Address
  , mpAmount :: Natural
  }
  deriving stock (Show, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable MintParam where
  build = genericF

type MintParams = [MintParam]

type BurnParams = List Natural

type TransferOwnershipParam = Address
type ChangeMasterMinterParam = Address
type ChangePauserParam = Address
type SetTransferlistParam = Maybe Address

newtype PermitHash = PermitHash ByteString
  deriving stock (Generic, Show)
  deriving newtype (IsoValue, L.HasAnnotation, Eq, Ord)

instance Buildable PermitHash where
  build (PermitHash bs) = base64F bs

data PermitParam = PermitParam
  { ppKey :: PublicKey
  , ppSignature :: Signature
  , ppPermitHash :: PermitHash
  }
 deriving stock (Generic, Show)
 deriving anyclass (IsoValue, HasAnnotation)

instance Buildable PermitParam where
  build = genericF

type Expiry = Natural

data SetExpiryParam = SetExpiryParam
  { sepOwner :: Address
  , sepExpiry :: Expiry
  , sepPermitHash :: Maybe PermitHash
  }
  deriving stock (Show, Generic)

deriving anyclass instance IsoValue SetExpiryParam
deriving anyclass instance HasAnnotation SetExpiryParam
instance Buildable SetExpiryParam where
  build = genericF

instance HasAnnotation FA2.Parameter where
  getAnnotation f =
    NTOr noAnn noAnn noAnn
      (NTOr noAnn noAnn noAnn
        (getAnnotation @FA2.BalanceRequestParams f) (getAnnotation @FA2.TransferParams f))
      (getAnnotation @FA2.UpdateOperatorsParam f)

$(customGeneric "FA2.Parameter" $ withDepths
    [ cstr @2 [fld @0]
    , cstr @2 [fld @0]
    , cstr @1 [fld @0]
    ]
  )

deriving anyclass instance IsoValue FA2.Parameter

instance ParameterHasEntrypoints FA2.Parameter where
  type ParameterEntrypointsDerivation FA2.Parameter = EpdPlain

instance TypeHasDoc FA2.Parameter where
  typeDocMdDescription = "Describes the FA2 operations."

-- | Parameter of Stablecoin contract
data Parameter
  = Accept_ownership
  | Burn BurnParams
  | Call_FA2 FA2.Parameter
  | Change_master_minter ChangeMasterMinterParam
  | Change_pauser ChangePauserParam
  | Configure_minter ConfigureMinterParam
  | Mint MintParams
  | Pause
  | Permit PermitParam
  | Remove_minter RemoveMinterParam
  | Set_expiry SetExpiryParam
  | Set_transferlist SetTransferlistParam
  | Transfer_ownership TransferOwnershipParam
  | Unpause

-- | In order to be able to construct valid permits,
-- the shape of the @or@ tree generated
-- the Michelson representation of 'Parameter' must be an exact match
-- of the Michelson parameter generated by Ligo in @stablecoin.tz@.
--
-- To see the the Michelson representation of 'Parameter':
--
-- >>> import Data.Singletons (demote)
-- >>> import Michelson.Typed (ToT)
-- >>> import Fmt (pretty)
-- >>>
-- >>> pretty $ demote @(ToT Parameter)
$(customGeneric "Parameter" $ withDepths
    [ cstr @4 []       -- %accept_ownership
    , cstr @4 [fld @0] -- %burn
    , cstr @4 [fld @0] -- %call_FA2
    , cstr @4 [fld @0] -- %change_master_minter
    , cstr @4 [fld @0] -- %change_pauser
    , cstr @4 [fld @0] -- %configure_minter
    , cstr @4 [fld @0] -- %mint
    , cstr @4 []       -- %pause
    , cstr @4 [fld @0] -- %permit
    , cstr @4 [fld @0] -- %remove_minter
    , cstr @4 [fld @0] -- %set_expiry
    , cstr @4 [fld @0] -- %set_transferlist
    , cstr @3 [fld @0] -- %transfer_ownership
    , cstr @3 []       -- %unpause
    ]
  )

deriving anyclass instance IsoValue Parameter

instance Buildable Parameter where
  build = genericF

instance Show Parameter where
  show = pretty

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

type ManagementMichelsonEntrypoints =
    [ "Pause" :> ()
    , "Unpause" :> ()
    , "Configure_minter" :> ConfigureMinterParam
    , "Remove_minter" :> RemoveMinterParam
    , "Mint" :> MintParams
    , "Burn" :> BurnParams
    , "Transfer_ownership" :> TransferOwnershipParam
    , "Accept_ownership" :> ()
    , "Change_master_minter" :> ChangeMasterMinterParam
    , "Change_pauser" :> ChangePauserParam
    , "Set_transferlist" :> SetTransferlistParam
    ]

type PermitEntrypoints =
    [ "Permit" :> PermitParam
    , "Set_expiry" :> SetExpiryParam
    ]

type ParameterC param =
  ( FA2.ParameterC param
  , ParameterContainsEntrypoints param ManagementMichelsonEntrypoints
  , ParameterContainsEntrypoints param PermitEntrypoints
  )

-- | Crates a permit that can be issued via the @Permit@ entrypoint, allowing other users
-- to call the contract with the given 'Parameter' value.
mkPermitHash :: Parameter -> PermitHash
mkPermitHash = PermitHash . Hash.blake2b . lPackValue

------------------------------------------------------------------
--- Storage
------------------------------------------------------------------

data UserPermits = UserPermits
  { upExpiry :: Maybe Expiry
  , upPermits :: Map PermitHash PermitInfo
  }
  deriving stock (Show, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

data PermitInfo = PermitInfo
  { piCreatedAt :: Timestamp
  , piExpiry :: Maybe Expiry
  }
  deriving stock (Show, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

data Roles = Roles
  { rMasterMinter :: Address
  , rOwner :: Address
  , rPauser :: Address
  , rPendingOwner :: Maybe Address
  }
  deriving stock (Show)

deriving anyclass instance IsoValue Roles
deriving anyclass instance HasAnnotation Roles
$(customGeneric "Roles" $ withDepths
    [ cstr @0
      [ fld @2
      , fld @2
      , fld @2
      , fld @2
      ]
    ]
  )

data Storage' big_map = Storage
  { sDefaultExpiry :: Expiry
  , sLedger :: big_map Address Natural
  , sMetadata :: MetadataMap big_map
  , sMintingAllowances :: Map Address Natural
  , sOperators :: big_map (Address, Address) ()
  , sIsPaused :: Bool
  , sPermitCounter :: Natural
  , sPermits :: big_map Address UserPermits
  , sRoles :: Roles
  , sTotalSupply :: Natural
  , sTransferlistContract :: Maybe Address
  }

$(customGeneric "Storage'" $ withDepths
    [ cstr @0
      [ fld @4 -- sDefaultExpiry
      , fld @4 -- sLedger
      , fld @4 -- sMetadata
      , fld @4 -- sMintingAllowances
      , fld @4 -- sOperators
      , fld @4 -- sIsPaused
      , fld @4 -- sPermitCounter
      , fld @4 -- sPermits
      , fld @3 -- sRoles
      , fld @3 -- sTotalSupply
      , fld @2 -- sTransferlistContract
      ]
    ]
  )

type Storage = Storage' BigMap
deriving stock instance Show Storage
deriving anyclass instance IsoValue Storage
deriving anyclass instance HasAnnotation Storage

-- | Represents a storage value retrieved using the Tezos RPC.
--
-- 'StorageView' is very similar to 'Storage',
-- except 'BigMap's have been replaced by 'BigMapId'.
-- This is because, when a contract's storage is queried, the Tezos RPC returns
-- big_maps' IDs instead of their contents.
type StorageView = Storage' BigMapId
deriving stock instance Show StorageView
deriving anyclass instance IsoValue StorageView

-- Currently the contract allows to add up to 12 minters.
minterLimit :: Int
minterLimit = 12

data MetadataRegistryStorage' big_map = MetadataRegistryStorage
  { cmrsDummyField :: () -- Dummy field to match the ligo contract's storage.
  , cmrsMetadata:: MetadataMap big_map
  }
  deriving stock Generic

type MetadataRegistryStorage = MetadataRegistryStorage' BigMap

deriving anyclass instance IsoValue MetadataRegistryStorage

-- | Construct the storage for the Metadata contract.
mkContractMetadataRegistryStorage
  :: MetadataMap BigMap
  -> MetadataRegistryStorage
mkContractMetadataRegistryStorage m = MetadataRegistryStorage
  { cmrsDummyField = ()
  , cmrsMetadata = m
  }

-- The default storage of metadata contract with some hardcoded token metadata.
-- Useful for testing.
defaultContractMetadataStorage :: MetadataRegistryStorage
defaultContractMetadataStorage =
  mkContractMetadataRegistryStorage
    (BigMap $ Map.fromList [([mt|TEST|], "TEST"), ([mt|Test|], "Test")])

-- This empty splice lets us workaround the GHC stage restriction, and refer to `Storage`
-- in the TH splices below.
$(pure [])

stablecoinContract :: T.Contract (ToT Parameter) (ToT Storage)
stablecoinContract =
  -- This TemplateHaskell splice is here to ensure the Michelson representation
  -- (i.e., the generated tree of `or` and `pair`) of these Haskell data
  -- types matches exactly the Michelson representation of the Ligo data types.
  --
  -- For example, if a Ligo data type generates a Michelson balanced tuple like
  -- ((a, b), (c, d)), but the corresponding Haskell data type generates a Michelson
  -- tuple like (a, (b, (c, d))), compilation should fail.
  --
  -- The reason they need to match is because of the way permits work.
  -- If we issue a permit for an entrypoint and the tree of `or`s is incorrect,
  -- then the permit will be unusable.
  -- See TZIP-017 for more info.
  --
  -- The splice attempts to parse the stablecoin.tz contract at compile-time.
  -- If, for example, the Michelson representation of Haskell's
  -- `Lorentz.Contracts.Stablecoin.Parameter` is different from Ligo's `parameter`,
  -- then this splice will raise a compilation error.
  --
  -- This can usually be fixed by writing a custom instance of Generic using `customGeneric`,
  -- and making sure the data type's layout matches the layout in stablecoin.tz.
  -- See examples in this module.
  $(case readContract @(ToT Parameter) @(ToT Storage) stablecoinPath $(embedStringFile stablecoinPath) of
      Left e ->
        -- Emit a compiler error if the contract cannot be read.
        fail (pretty e)
      Right _ ->
        -- Emit a haskell expression that reads the contract.
        [|
          -- Note: it's ok to use `error` here, because we just proved that the contract
          -- can be parsed+typechecked.
          either (error . pretty) snd $
            readContract
              stablecoinPath
              $(embedStringFile stablecoinPath)
        |]
  )

-- | Parse the contract-metadata contract.
contractMetadataContract :: T.Contract (ToT ()) (ToT MetadataRegistryStorage)
contractMetadataContract =
  $(case readContract @(ToT ()) @(ToT MetadataRegistryStorage) metadataRegistryContractPath $(embedStringFile metadataRegistryContractPath) of
      Left e -> fail (pretty e)
      Right _ ->
        [|
          -- Note: it's ok to use `error` here, because we just proved that the contract
          -- can be parsed+typechecked.
          either (error . pretty) snd $
            readContract
              metadataRegistryContractPath
              $(embedStringFile metadataRegistryContractPath)
        |]
  )

----------------------------------------------------------------------------
-- TZIP-16 Metadata
----------------------------------------------------------------------------

jfield :: MText
jfield = [mt|metadataJSON|]

metadataMap :: J.ToJSON metadata => MetadataUri metadata -> MetadataMap BigMap
metadataMap mdata = BigMap $ Map.fromList $
  -- One might reasonable expect that the URI would be stored as packed Michelson strings,
  -- but the TZIP-16 spec is explicit about that not being the case.
  --
  -- > Unless otherwise-specified, the encoding of the values must be the direct stream
  -- > of bytes of the data being stored. (...)
  -- > There is no implicit conversion to Michelson's binary format (PACK) nor
  -- > quoting mechanism.
  --
  -- See: <https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md#contract-storage>
  --
  -- So, instead, we encode it as UTF-8 byte sequences.
  case mdata of
    CurrentContract md includeUri ->
      if includeUri
          then [ (mempty, TZ.encodeURI $ TZ.tezosStorageUri (TZ.ContractHost Nothing) jfield)
               , (jfield, BSL.toStrict (J.encode md))
               ]
          else [ (jfield, BSL.toStrict (J.encode md)) ]
    RemoteContract addr ->
      [ (mempty, TZ.encodeURI $ TZ.tezosStorageUri (TZ.ContractHost (Just $ formatAddress addr)) jfield)
      ]
    Raw uri ->
      [ (mempty, encodeUtf8 uri)
      ]


-- Result after parsing the metadata uri from a TZIP-16 metadata bigmap.
data ParsedMetadataUri
  = InCurrentContractUnderKey Text
  | InRemoteContractUnderKey Address Text
  | RawUri Text
  deriving stock (Eq, Show)

parseMetadataUri :: Text -> Either Text ParsedMetadataUri
parseMetadataUri t = first (fromString . P.errorBundlePretty) $ P.parse metadataUriParser "" t

metadataUriParser :: P.Parsec Void Text ParsedMetadataUri
metadataUriParser
  =   (P.try remoteContractUriParser)
  <|> (P.try currentContractUriParser)
  <|> rawUriParser

remoteContractUriParser :: P.Parsec Void Text ParsedMetadataUri
remoteContractUriParser = do
  _ <- string' (TZ.tezosStorageScheme <> "://")
  addr <- P.manyTill P.anySingle (string' "/")
  key <- P.many P.anySingle
  case parseAddress (toText addr) of
    Right paddr -> pure $ InRemoteContractUnderKey paddr (toText key)
    Left err -> fail $ show err

rawUriParser :: P.Parsec Void Text ParsedMetadataUri
rawUriParser = (RawUri . toText) <$> (P.many (P.satisfy (const True)))

currentContractUriParser :: P.Parsec Void Text ParsedMetadataUri
currentContractUriParser = do
  _ <- string' (TZ.tezosStorageScheme <>  ":")
  key_ <- P.many P.anySingle
  pure $ InCurrentContractUnderKey (toText key_)

data MetadataUri metadata
  = CurrentContract metadata Bool -- ^ Metadata and a flag to denote if URI should be included
  | RemoteContract Address
  | Raw Text

-- | Make the TZIP-16 metadata. We accept a @Maybe@ @FA2.TokenMetadata@
-- as argument here so that we can use this function to create the metadata of the
-- FA1.2 Variant as well.
metadataJSON :: Maybe FA2.TokenMetadata -> Metadata (ToT Storage)
metadataJSON mtmd  =
  TZ.name  "stablecoin" <>
  TZ.version (toText $ showVersion version) <>
  TZ.license (License { lName = "MIT", lDetails = Nothing }) <>
  TZ.authors
      [ TZ.author "Serokell" "https://serokell.io/"
      , TZ.author "TQ Tezos" "https://tqtezos.com/"
      ] <>
  TZ.homepage "https://github.com/tqtezos/stablecoin/" <>
  TZ.source Source
     { sLocation = Just $ "https://github.com/tqtezos/stablecoin/tree/v" <> toText (showVersion version) <> "/ligo/stablecoin"
     , sTools = [ "ligo " ] -- TODO: add ligo version
     } <>
  TZ.interfaces [ TZ.Interface "TZIP-012", TZ.Interface "TZIP-017" ] <>
  TZ.errors [ mkError [mt|FA2_TOKEN_UNDEFINED|]        [mt|All `token_id`s must be 0|]
            , mkError [mt|FA2_INSUFFICIENT_BALANCE|]   [mt|Cannot debit from a wallet because of insufficient amount of tokens|]
            , mkError [mt|FA2_NOT_OPERATOR|]           [mt|You're neither the owner or a permitted operator of one or more wallets from which tokens will be transferred|]
            , mkError [mt|XTZ_RECEIVED|]               [mt|Contract received a non-zero amount of tokens|]
            , mkError [mt|NOT_CONTRACT_OWNER|]         [mt|Operation can only be performed by the contract's owner|]
            , mkError [mt|NOT_PENDING_OWNER|]          [mt|Operation can only be performed by the current contract's pending owner|]
            , mkError [mt|NO_PENDING_OWNER_SET|]       [mt|There's no pending transfer of ownership|]
            , mkError [mt|NOT_PAUSER|]                 [mt|Operation can only be performed by the contract's pauser|]
            , mkError [mt|NOT_MASTER_MINTER|]          [mt|Operation can only be performed by the contract's master minter|]
            , mkError [mt|NOT_MINTER|]                 [mt|Operation can only be performed by registered minters|]
            , mkError [mt|CONTRACT_PAUSED|]            [mt|Operation cannot be performed while the contract is paused|]
            , mkError [mt|CONTRACT_NOT_PAUSED|]        [mt|Operation cannot be performed while the contract is not paused|]
            , mkError [mt|NOT_TOKEN_OWNER|]            [mt|You cannot configure another user's operators|]
            , mkError [mt|CURRENT_ALLOWANCE_REQUIRED|] [mt|The given address is already a minter, you must specify its current minting allowance|]
            , mkError [mt|ALLOWANCE_MISMATCH|]         [mt|The given current minting allowance does not match the minter's actual current minting allowance|]
            , mkError [mt|ADDR_NOT_MINTER|]            [mt|This address is not a registered minter|]
            , mkError [mt|ALLOWANCE_EXCEEDED|]         [mt|The amount of tokens to be minted exceeds your current minting allowance|]
            , mkError [mt|BAD_TRANSFERLIST|]           [mt|The given address is a not a smart contract complying with the transferlist interface|]
            , mkError [mt|MINTER_LIMIT_REACHED|]       [mt|Cannot add new minter because the number of minters is already at the limit|]
            , mkError [mt|MISSIGNED|]                  [mt|This permit's signature is invalid|]
            , mkError [mt|EXPIRED_PERMIT|]             [mt|A permit was found, but it has already expired|]
            , mkError [mt|NOT_PERMIT_ISSUER|]          [mt|You're not the issuer of the given permit|]
            , mkError [mt|DUP_PERMIT|]                 [mt|The given permit already exists|]
            , mkError [mt|EXPIRY_TOO_BIG|]             [mt|The `set_expiry` entrypoint was called with an expiry value that is too big|]
            ] <>
  TZ.views (case mtmd of
     Nothing ->
       [ getDefaultExpiryView
       , getCounterView
       ]
     Just tmd ->
       [ getDefaultExpiryView
       , getCounterView
       , getBalanceView
       , getTotalSupplyView
       , getAllTokensView
       , isOperatorView
       , tokenMetadataView tmd
       ])
  where
    mkError :: MText -> MText -> Error
    mkError err expansion =
      TZ.EStatic $ TZ.StaticError
        { seError = toExpression (toVal err)
        , seExpansion = toExpression (toVal expansion)
        , seLanguages = ["en"]
        }

type BalanceViewParam = (Natural, Address)

getBalanceView :: TZ.View (ToT Storage)
getBalanceView =
  TZ.View
    { vName = "GetBalance"
    , vDescription = Just "Access the balance of an address"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $ WithParam @BalanceViewParam $
            L.dip (L.toField #sLedger) #
            L.cdr #
            L.get #
            L.whenNone (L.push 0) -- If there is no ledger entry, return zero.
    }

getTotalSupplyView :: TZ.View (ToT Storage)
getTotalSupplyView =
  TZ.View
    { vName = "GetTotalSupply"
    , vDescription = Just "Get the total no of tokens available."
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $ WithParam $
            L.int #
            L.assertEq0 [mt|Unknown TOKEN ID|] #
            L.toField #sTotalSupply
    }

getAllTokensView :: TZ.View (ToT Storage)
getAllTokensView =
  TZ.View
    { vName = "GetAllTokens"
    , vDescription = Just "Get list of token ids supported."
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage Nothing [] $ WithoutParam $
            L.drop # L.nil # L.push (0 :: Natural) # L.cons
    }

isOperatorView :: TZ.View (ToT Storage)
isOperatorView =
  TZ.View
    { vName = "IsOperator"
    , vDescription = Just "Check if the given address is an operator"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Bool Nothing [] $ WithParam $
            L.dip (L.toField #sOperators) #
            L.unpair #
            L.int #
            L.assertEq0 [mt|Unknown TOKEN ID|] #
            L.get #
            L.ifSome (L.drop # L.push True) (L.push False)
    }

tokenMetadataView :: FA2.TokenMetadata -> TZ.View (ToT Storage)
tokenMetadataView md =
  TZ.View
    { vName = "GetTokenMetadata"
    , vDescription = Just "Get token metadata for the token id"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @(Natural, Map.Map MText ByteString) Nothing [] $ WithParam $
            L.dip L.drop #
            L.int #
            L.assertEq0 [mt|Unknown TOKEN ID|] #
            L.push (0 :: Natural, md)
    }

getDefaultExpiryView :: TZ.View (ToT Storage)
getDefaultExpiryView =
  TZ.View
    { vName = "GetDefaultExpiry"
    , vDescription = Just "Access the contract's default expiry in seconds"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage  @Natural Nothing [] $ WithoutParam $
            L.toField #sDefaultExpiry
    }

getCounterView :: TZ.View (ToT Storage)
getCounterView =
  TZ.View
    { vName = "GetCounter"
    , vDescription = Just "Access the current permit counter"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $ WithoutParam $
            L.toField #sPermitCounter
    }
