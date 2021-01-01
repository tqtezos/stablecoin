-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | Test commons for stablecoin test suite

module Lorentz.Contracts.Test.Common
  ( oneTokenId

  , testOwner
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
  , checkView

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
  , nettestOriginateContractMetadataContract
  , testFA2TokenMetadata

  , lExpectAnyMichelsonFailed
  ) where

import Data.Aeson (ToJSON)
import Data.List.NonEmpty ((!!))
import qualified Data.Map as Map

import Lorentz (arg)
import qualified Lorentz.Contracts.Spec.TZIP16Interface as MD
import Lorentz.Test
import Lorentz.Value
import Michelson.Runtime (ExecutorError)
import Michelson.Runtime.GState (genesisSecrets)
import Michelson.Typed (convertContract, untypeValue)
import Morley.Nettest as NT
import Tezos.Crypto (SecretKey, toPublic)
import Util.Named

import qualified Indigo.Contracts.Transferlist.Internal as Transferlist
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import qualified Morley.Metadata as MD
import qualified Test.Morley.Metadata as MD

-- | A 'TokenId' with the value of `1`.
-- This is used only for tests because @stablecoin@ only supports a single token
-- with the value of 0, aka 'theTokenId'.
oneTokenId :: TokenId
oneTokenId = TokenId 1

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
  , opMetadataUri :: MetadataUri (MD.Metadata (ToT Storage))
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
  , opMetadataUri = CurrentContract (metadataJSON $ Just testFA2TokenMetadata) True
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
  , tdTokenId = FA2.theTokenId
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
  [ TransferItem
      { tiFrom = from
      , tiTxs  = constructDestination <$> txs
      }
  ]

constructSingleTransfer
  :: "from_" :! Address
  -> "to_" :! Address
  -> "amount" :! Natural
  -> TransferParams
constructSingleTransfer (arg #from_ -> from) (arg #to_ -> to) (arg #amount -> amount)
    = [TransferItem from [TransferDestination to FA2.theTokenId amount]]

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

mkInitialStorage :: OriginationParams -> Storage
mkInitialStorage OriginationParams{..} =
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
    , sTransferlistContract = unTAddress <$> opTransferlistContract
    , sMetadata = metadataMap opMetadataUri
    , sTotalSupply = sum $ Map.elems opBalances
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

nettestOriginateContractMetadataContract :: (ToJSON metadata) => MonadNettest caps base m => metadata -> m Address
nettestOriginateContractMetadataContract mdata =
  originateUntypedSimple
    "nettest.ContractMetadata"
    (untypeValue (toVal $ mkContractMetadataRegistryStorage $  metadataMap (CurrentContract mdata False)))
    (convertContract contractMetadataContract)

checkView
  :: forall viewVal parameter
   . (IsoValue viewVal, Eq viewVal, Show viewVal)
  => TAddress parameter -> Text -> MD.ViewParam -> viewVal -> IntegrationalScenario
checkView = MD.checkView sMetadata

testFA2TokenMetadata :: FA2.TokenMetadata
testFA2TokenMetadata = FA2.mkTokenMetadata "TEST" "TEST" "3"
