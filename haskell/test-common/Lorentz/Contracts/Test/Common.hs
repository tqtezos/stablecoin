-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

-- | Test commons for stablecoin test suite

module Lorentz.Contracts.Test.Common
  ( testOwner
  , testPauser
  , testMasterMinter
  , wallet1
  , wallet2
  , wallet3
  , wallet4
  , wallet5
  , commonOperator
  , commonOperators

  , OriginationFn
  , OriginationParams (..)
  , defaultOriginationParams
  , defaultTokenMetadataBigMap
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
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Test
import Lorentz.Value
import Michelson.Runtime (ExecutorError)
import Util.Named

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
  , opTokenMetadata :: FA2.TokenMetadata
  , opTransferlistContract :: Maybe (TAddress Transferlist.Parameter)
  }

defaultTokenMetadata :: FA2.TokenMetadata
defaultTokenMetadata =
  ( #token_id .! 0
  , #mdr .! (#symbol .! [mt|TestTokenSymbol|]
  , #mdr2 .! (#name .! [mt|TestTokenName|]
  , #mdr3 .! (#decimals .! 8
  , #extras .! (Map.fromList $
       [([mt|attr1|], [mt|val1|]), ([mt|attr2|], [mt|val2|]) ]))))
  )

defaultTokenMetadataBigMap :: BigMap FA2.TokenId SC.TokenMetadata
defaultTokenMetadataBigMap = BigMap $ Map.fromList [((0 :: Natural), mkTokenMetadata defaultTokenMetadata)]

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
  , opTokenMetadata = defaultTokenMetadata
  , opTransferlistContract = Nothing
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

mkInitialStorage :: OriginationParams -> Maybe Storage
mkInitialStorage OriginationParams{..} = let
  ledgerMap = opBalances
  mintingAllowances = #minting_allowances .! (BigMap opMinters)
  operatorMap = Map.foldrWithKey foldFn mempty opOwnerToOperators
  ledger = #ledger .! (BigMap ledgerMap)
  owner_ = #owner .! opOwner
  pauser_ = #pauser .! opPauser
  masterMinter_ = #master_minter .! opMasterMinter
  roles = #roles .! ((masterMinter_, owner_), (pauser_, (#pending_owner_address .! opPendingOwner)))
  operators = #operators .! (BigMap operatorMap)
  isPaused = #paused .! opPaused
  transferlistContract = #transferlist_contract .! (unTAddress <$> opTransferlistContract)
  tokenMetadata = #token_metadata .! (BigMap $ Map.fromList [(0, mkTokenMetadata $ opTokenMetadata)])
  in Just (((ledger, mintingAllowances), (operators, isPaused))
           , ((roles, tokenMetadata), transferlistContract))
  where
    foldFn
      :: Address
      -> [Address]
      -> Map (Address, Address) ()
      -> Map (Address, Address) ()
    foldFn ow ops m = foldr (\a b -> Map.insert (ow, a) () b) m ops
