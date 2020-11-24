-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

{-# LANGUAGE PackageImports #-}

-- | Test commons for stablecoin test suite

module Lorentz.Contracts.Test.Common
  ( testOwner
  , testOwnerPK
  , testOwnerSK
  , testPauser
  , testPauserSK
  , testPauserPK
  , testMasterMinter
  , testMasterMinterSK
  , testMasterMinterPK
  , wallet1
  , wallet1SK
  , wallet1PK
  , wallet2
  , wallet2SK
  , wallet2PK
  , wallet3
  , wallet4
  , wallet5
  , commonOperator
  , commonOperators

  , OriginationFn
  , OriginationParams (..)
  , defaultOriginationParams
  , addAccount
  , addOperator
  , addMinter
  , constructTransfers
  , constructTransfersFromSender
  , constructSingleTransfer
  , withOriginated
  , mgmContractPaused
  , mkInitialStorage
  , originateMetadataRegistry
  , nettestOriginateMetadataRegistry

  , lExpectAnyMichelsonFailed
  ) where

import Data.List.NonEmpty ((!!))
import qualified Data.Map as Map

import Lorentz (arg)
import Lorentz.Test
import Lorentz.Value
import Michelson.Runtime (ExecutorError)
import Michelson.Runtime.GState (genesisSecrets)
import Michelson.Test (tOriginate)
import Michelson.Typed (convertContract, untypeValue)
import Morley.Nettest as NT
import Tezos.Crypto (SecretKey, toPublic)
import Util.Named

import qualified Indigo.Contracts.Transferlist.Internal as Transferlist
import "stablecoin" Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC

testOwner, testPauser, testMasterMinter, wallet1, wallet2, wallet3, wallet4, wallet5, commonOperator :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
wallet4 = genesisAddress4
wallet5 = genesisAddress5
commonOperator = genesisAddress6
testOwner = genesisAddresses !! 7
testPauser = genesisAddresses !! 8
testMasterMinter = genesisAddresses !! 9

wallet1SK, wallet2SK, testOwnerSK, testPauserSK, testMasterMinterSK :: SecretKey
wallet1SK = genesisSecrets !! 1
wallet2SK = genesisSecrets !! 2
testOwnerSK = genesisSecrets !! 7
testPauserSK = genesisSecrets !! 8
testMasterMinterSK = genesisSecrets !! 9

wallet1PK, wallet2PK, testOwnerPK, testPauserPK, testMasterMinterPK :: PublicKey
wallet1PK = toPublic wallet1SK
wallet2PK = toPublic wallet2SK
testOwnerPK = toPublic testOwnerSK
testPauserPK = toPublic testPauserSK
testMasterMinterPK = toPublic testMasterMinterSK

commonOperators :: [Address]
commonOperators = [commonOperator]

type LedgerType = Map Address Natural

addAccount :: (Address, ([Address], Natural)) -> OriginationParams -> OriginationParams
addAccount (addr, (operators, bal)) op = let
  withAccount = op
    { opBalances =
        Map.insert addr bal $ opBalances op
    }
  in foldl' (\oparams operator -> addOperator (addr, operator) oparams) withAccount operators

addOperator :: (Address, Address) -> OriginationParams -> OriginationParams
addOperator (owner_, operator) op = op
  { opOwnerToOperators =
      Map.alter (\case
          Just ops -> Just $ operator:ops
          Nothing -> Just [operator]) owner_ $ opOwnerToOperators op
  }
-- TODO: move to morley
lExpectAnyMichelsonFailed :: (ToAddress addr) => addr -> ExecutorError -> IntegrationalScenario
lExpectAnyMichelsonFailed = lExpectMichelsonFailed (const True)

data OriginationParams = OriginationParams
  { opBalances :: LedgerType
  , opOwnerToOperators :: Map Address [Address]
  , opOwner :: Address
  , opPauser :: Address
  , opMasterMinter :: Address
  , opPaused :: Bool
  , opMinters :: Map Address Natural
  , opPendingOwner :: Maybe Address
  , opTokenMetadataRegistry :: Maybe Address
  , opTransferlistContract :: Maybe (TAddress Transferlist.Parameter)
  , opDefaultExpiry :: Natural
  , opPermits :: Map Address UserPermits
  }

defaultOriginationParams :: OriginationParams
defaultOriginationParams = OriginationParams
  { opBalances = mempty
  , opOwner = testOwner
  , opOwnerToOperators = mempty
  , opPauser = testPauser
  , opMasterMinter = testMasterMinter
  , opPaused = False
  , opMinters = mempty
  , opPendingOwner = Nothing
  , opTokenMetadataRegistry = Nothing
  , opTransferlistContract = Nothing
  , opDefaultExpiry = 1000
  , opPermits = mempty
  }

addMinter
  :: (Address, Natural)
  -> OriginationParams
  -> OriginationParams
addMinter (minter, mintingAllowance) op@OriginationParams{ opMinters = currentMinters } =
  op { opMinters = Map.insert minter mintingAllowance currentMinters }

constructDestination
  :: ("to_" :! Address, "amount" :! Natural)
  -> TransferDestination
constructDestination (arg #to_ -> to, arg #amount -> amount) = TransferDestination
  { tdTo = to
  , tdTokenId = 0
  , tdAmount = amount
  }

constructTransfers
  :: [("from_" :! Address, [("to_" :! Address, "amount" :! Natural)])]
  -> TransferParams
constructTransfers pairs = pairs >>= uncurry constructTransfersFromSender

constructTransfersFromSender
  :: "from_" :! Address
  -> [("to_" :! Address, "amount" :! Natural)]
  -> TransferParams
constructTransfersFromSender (arg #from_ -> from) txs =
  [ TransferParam
      { tpFrom = from
      , tpTxs = constructDestination <$> txs
      }
  ]

constructSingleTransfer
  :: "from_" :! Address
  -> "to_" :! Address
  -> "amount" :! Natural
  -> TransferParams
constructSingleTransfer (arg #from_ -> from) (arg #to_ -> to) (arg #amount -> amount)
    = [TransferParam from [TransferDestination to 0 amount]]

-- | The return value of this function is a Maybe to handle the case where a contract
-- having hardcoded permission descriptor, and thus unable to initialize with a custom
-- permissions descriptor passed from the testing code.
--
-- In such cases, where the value is hard coded and is incompatible with what is required
-- for the test, this function should return a Nothing value, and the tests that depend
-- on such custom configuration will be skipped.
type OriginationFn param = (OriginationParams -> IntegrationalScenarioM (TAddress param))

withOriginated
  :: forall param. OriginationFn param
  -> OriginationParams
  -> (TAddress param -> IntegrationalScenario)
  -> IntegrationalScenario
withOriginated fn op tests = fn op >>= tests

mkInitialStorage :: OriginationParams -> Address -> Storage
mkInitialStorage OriginationParams{..} metadataRegistryAddress =
  Storage
    { sDefaultExpiry = opDefaultExpiry
    , sLedger = BigMap opBalances
    , sMintingAllowances = opMinters
    , sOperators = BigMap (Map.foldrWithKey foldFn mempty opOwnerToOperators)
    , sIsPaused = opPaused
    , sPermitCounter = 0
    , sPermits = BigMap opPermits
    , sRoles = Roles
        { rMasterMinter = opMasterMinter
        , rOwner = opOwner
        , rPauser = opPauser
        , rPendingOwner = opPendingOwner
        }
    , sTokenMetadataRegistry = fromMaybe metadataRegistryAddress opTokenMetadataRegistry
    , sTransferlistContract = unTAddress <$> opTransferlistContract
    , sMetadata = metadataMap
    }
  where
    foldFn
      :: Address
      -> [Address]
      -> Map (Address, Address) ()
      -> Map (Address, Address) ()
    foldFn ow ops m = foldr (\a b -> Map.insert (ow, a) () b) m ops

mgmContractPaused :: ExecutorError -> IntegrationalScenario
mgmContractPaused = lExpectFailWith (== [mt|CONTRACT_PAUSED|])

originateMetadataRegistry :: IntegrationalScenarioM Address
originateMetadataRegistry =
  tOriginate
    registryContract
    "Metadata Registry contract"
    (toVal defaultMetadataRegistryStorage)
    (toMutez 0)

nettestOriginateMetadataRegistry :: MonadNettest caps base m => m Address
nettestOriginateMetadataRegistry =
  originateUntypedSimple
    "nettest.MetadataRegistry"
    (untypeValue (toVal defaultMetadataRegistryStorage))
    (convertContract registryContract)
