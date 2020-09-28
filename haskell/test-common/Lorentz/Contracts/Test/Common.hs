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
  , registryAddress

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
  , mkInitialStorage

  , lExpectAnyMichelsonFailed
  ) where

import Data.List.NonEmpty ((!!))
import qualified Data.Map as Map

import qualified Indigo.Contracts.Transferlist.Internal as Transferlist
import "stablecoin" Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Test
import Lorentz.Value
import Michelson.Runtime (ExecutorError)
import Michelson.Runtime.GState (genesisSecrets)
import Tezos.Address (detGenKeyAddress)
import Tezos.Crypto (SecretKey, toPublic)
import Util.Named

registryAddress, testOwner, testPauser, testMasterMinter, wallet1, wallet2, wallet3, wallet4, wallet5, commonOperator :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
wallet4 = genesisAddress4
wallet5 = genesisAddress5
commonOperator = genesisAddress6
testOwner = genesisAddresses !! 7
testPauser = genesisAddresses !! 8
testMasterMinter = genesisAddresses !! 9
registryAddress = detGenKeyAddress "metadata-registry"

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
  , opTokenMetadataRegistry :: Address
  , opTransferlistContract :: Maybe (TAddress Transferlist.Parameter)
  , opDefaultExpiry :: Natural
  , opPermits :: Map Address (Maybe Expiry, Map PermitHash (Timestamp, Maybe Expiry))
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
  , opTokenMetadataRegistry = registryAddress
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
constructDestination (to_, amount) = (to_, (#token_id .! 0, amount))

constructTransfers
  :: [("from_" :! Address, [("to_" :! Address, "amount" :! Natural)])]
  -> TransferParams
constructTransfers = fmap (second constructTransfer)
  where
    constructTransfer destinations = #txs .! fmap constructDestination destinations

constructTransfersFromSender
  :: "from_" :! Address
  -> [("to_" :! Address, "amount" :! Natural)]
  -> TransferParams
constructTransfersFromSender from_ txs =
  [( from_
   , ( #txs .! (constructDestination <$> txs)))]

constructSingleTransfer
  :: "from_" :! Address
  -> "to_" :! Address
  -> "amount" :! Natural
  -> TransferParams
constructSingleTransfer from to amount
    = [(from, (#txs .! [(to, (#token_id .! 0, amount))]))]

-- | The return value of this function is a Maybe to handle the case where a contract
-- having hardcoded permission descriptor, and thus unable to initialize with a custom
-- permissions descriptor passed from the testing code.
--
-- In such cases, where the value is hard coded and is incompatible with what is required
-- for the test, this function should return a Nothing value, and the tests that depend
-- on such custom configuration will be skipped.
type OriginationFn param = (OriginationParams -> IntegrationalScenarioM (Maybe (TAddress param)))

withOriginated
  :: forall param. OriginationFn param
  -> OriginationParams
  -> (TAddress param -> IntegrationalScenario)
  -> IntegrationalScenario
withOriginated fn op tests = do
  (fn op) >>= \case
    Nothing -> pass
    Just contract -> tests contract

{-# ANN module ("HLint: ignore Use bimap" :: Text) #-}
mkInitialStorage :: OriginationParams -> Maybe Storage
mkInitialStorage OriginationParams{..} = let
  ledgerMap = opBalances
  mintingAllowances = #minting_allowances .! opMinters
  operatorMap = Map.foldrWithKey foldFn mempty opOwnerToOperators
  ledger = #ledger .! (BigMap ledgerMap)
  owner_ = #owner .! opOwner
  pauser_ = #pauser .! opPauser
  masterMinter_ = #master_minter .! opMasterMinter
  roles = #roles .! ((masterMinter_, owner_), (pauser_, (#pending_owner_address .! opPendingOwner)))
  operators = #operators .! (BigMap operatorMap)
  isPaused = #paused .! opPaused
  transferlistContract = #transferlist_contract .! (unTAddress <$> opTransferlistContract)
  tokenMetadata = #token_metadata_registry .! opTokenMetadataRegistry
  permits =
    #permits .! BigMap (opPermits <&> \(userExpiry, userPermits) ->
      ( #expiry .! userExpiry
      , #permits .! (userPermits <&> \(createdAt, expiry) -> (#created_at .! createdAt, #expiry .! expiry))
      )
    )
  permitCounter = #permit_counter .! 0
  defaultExpiry = #default_expiry .! opDefaultExpiry
  in Just ((((defaultExpiry, ledger),
             (mintingAllowances, operators)),
             ((isPaused, permitCounter),
             (permits, roles))),
           (tokenMetadata, transferlistContract))
  where
    foldFn
      :: Address
      -> [Address]
      -> Map (Address, Address) ()
      -> Map (Address, Address) ()
    foldFn ow ops m = foldr (\a b -> Map.insert (ow, a) () b) m ops
