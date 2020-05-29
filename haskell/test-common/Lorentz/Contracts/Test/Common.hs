-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

-- | Test commons for stablecoin test suite

module Lorentz.Contracts.Test.Common
  ( owner
  , pauser
  , masterMinter
  , wallet1
  , wallet2
  , wallet3
  , wallet4
  , wallet5
  , commonOperator
  , commonOperators

  , LedgerType

  , OriginationFn
  , OriginationParams (..)
  , defaultOriginationParams
  , defaultTokenMetadata
  , defaultPermissionDescriptor
  , permissionDescriptorOwnerTransfer
  , permissionDescriptorOwnerOrOperatorTransfer
  , permissionDescriptorOperatorNoTransfer
  , permissionDescriptorReqSenderHook
  , permissionDescriptorReqReceiverHook
  , permissionDescriptorNoOpReceiverHook
  , permissionDescriptorNoOpSenderHook
  , addAccount
  , addOperator
  , addMinter
  , constructTransfers
  , constructTransfersFromSender
  , constructSingleTransfer
  , skipTest
  , withOriginated
  , mkInitialStorage
  , originationRequestCompatible

  , lExpectAnyMichelsonFailed
  ) where

import Data.List.NonEmpty ((!!))
import qualified Data.Map as Map

import Lorentz (arg)
import qualified Indigo.Contracts.Safelist as Safelist
import qualified Lorentz as L
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Test
import Lorentz.Value
import Michelson.Runtime (ExecutorError)
import Tezos.Core (unsafeMkMutez)
import Util.Named


owner, pauser, masterMinter, wallet1, wallet2, wallet3, wallet4, wallet5, commonOperator :: Address
wallet1 = genesisAddress1
wallet2 = genesisAddress2
wallet3 = genesisAddress3
wallet4 = genesisAddress4
wallet5 = genesisAddress5
commonOperator = genesisAddress6
owner = genesisAddresses !! 7
pauser = genesisAddresses !! 8
masterMinter = genesisAddresses !! 9

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
lExpectAnyMichelsonFailed :: (ToAddress addr) => addr -> ExecutorError -> Bool
lExpectAnyMichelsonFailed = lExpectMichelsonFailed (const True)

data OriginationParams = OriginationParams
  { opBalances :: LedgerType
  , opOwnerToOperators :: Map Address [Address]
  , opOwner :: Address
  , opPauser :: Address
  , opMasterMinter :: Address
  , opPaused :: Bool
  , opMinters :: Map Address Natural
  , opPermissionsDescriptor :: PermissionsDescriptorMaybe
  , opTokenMetadata :: TokenMetadata
  , opSafelistContract :: Maybe (TAddress Safelist.Parameter)
  }

defaultPermissionDescriptor :: PermissionsDescriptorMaybe
defaultPermissionDescriptor =
  ( #operator .! Nothing
  , #pdr2 .! ( #receiver .! Nothing
             , #pdr3 .! (#sender .! Nothing, #custom .! Nothing)))

permissionDescriptorOperatorNoTransfer :: PermissionsDescriptorMaybe
permissionDescriptorOperatorNoTransfer =
  defaultPermissionDescriptor
   & _1 .~ (#operator .! (Just $ NoTransfer (#no_transfer .! ())))

permissionDescriptorOwnerOrOperatorTransfer :: PermissionsDescriptorMaybe
permissionDescriptorOwnerOrOperatorTransfer =
  defaultPermissionDescriptor &
    _1 .~ (#operator .! (Just $ OwnerOrOperatorTransfer (#owner_or_operator_transfer .! ())))

permissionDescriptorOwnerTransfer :: PermissionsDescriptorMaybe
permissionDescriptorOwnerTransfer =
  defaultPermissionDescriptor &
    _1 .~ (#operator .! (Just $ OwnerTransfer (#owner_transfer .! ())))

permissionDescriptorNoOpReceiverHook :: PermissionsDescriptorMaybe
permissionDescriptorNoOpReceiverHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr2._1) .~ (#receiver .! (Just $ OwnerNoOp (#owner_no_op .! ())))

permissionDescriptorReqReceiverHook :: PermissionsDescriptorMaybe
permissionDescriptorReqReceiverHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr2._1)
    .~ (#receiver .! (Just $ RequiredOwnerHook (#required_owner_hook .! ())))

permissionDescriptorNoOpSenderHook :: PermissionsDescriptorMaybe
permissionDescriptorNoOpSenderHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr2._2.namedL #pdr3._1)
    .~ (#sender .! (Just $ OwnerNoOp (#owner_no_op .! ())))

permissionDescriptorReqSenderHook :: PermissionsDescriptorMaybe
permissionDescriptorReqSenderHook =
  defaultPermissionDescriptor
    & (_2.namedL #pdr2._2.namedL #pdr3._1)
    .~ (#sender .! (Just $ RequiredOwnerHook (#required_owner_hook .! ())))

defaultTokenMetadata :: TokenMetadata
defaultTokenMetadata =
  ( #token_id .! 0
  , #mdr .! (#symbol .! [mt|TestTokenSymbol|]
  , #mdr2 .! (#name .! [mt|TestTokenName|]
  , #mdr3 .! (#decimals .! 8
  , #extras .! (Map.fromList $
       [([mt|attr1|], [mt|val1|]), ([mt|attr2|], [mt|val2|]) ]))))
  )

defaultOriginationParams :: OriginationParams
defaultOriginationParams = OriginationParams
  { opBalances = mempty
  , opOwner = owner
  , opOwnerToOperators = mempty
  , opPauser = pauser
  , opMasterMinter = masterMinter
  , opPaused = False
  , opMinters = mempty
  , opPermissionsDescriptor = defaultPermissionDescriptor
  , opTokenMetadata = defaultTokenMetadata
  , opSafelistContract = Nothing
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

-- | This is a temporary hack to workaround the inability to skip the
-- tests from an IntegrationalScenario without doing any validations.
skipTest :: IntegrationalScenario
skipTest = do
  let
    dummyContract :: L.ContractCode () ()
    dummyContract = L.unpair L.# L.drop L.# L.nil L.# L.pair
  c <- lOriginate (L.defaultContract dummyContract) "skip test dummy" () (unsafeMkMutez 0)
  lCallEP c CallDefault ()
  validate (Right expectAnySuccess)

withOriginated
  :: forall param. OriginationFn param
  -> OriginationParams
  -> (TAddress param -> IntegrationalScenario)
  -> IntegrationalScenario
withOriginated fn op tests = do
  (fn op) >>= \case
    Nothing -> skipTest
    Just contract -> tests contract

-- | This function accepts origination params with @Just@ values indicating
-- the mandatory configuration, and @Nothing@ values for permission parameters
-- that are not relavant for the test. The return flag indicate if the
-- contract's permission parameter can meet the required permission configuration.
originationRequestCompatible :: OriginationParams -> Bool
originationRequestCompatible op =
  isPermissionsCompatible (opPermissionsDescriptor op)
  where
    isPermissionsCompatible :: PermissionsDescriptorMaybe -> Bool
    isPermissionsCompatible
      (arg #operator -> ot
        , arg #pdr2 ->
          ( arg #receiver -> owtmr
          , arg #pdr3 -> (arg #sender -> otms, arg #custom -> cst))) = let
      customFlag = not $ isJust cst
      operatorTransferFlag = case ot of
        Nothing -> True
        Just (FA2.OwnerOrOperatorTransfer _) -> True
        _ -> False
      owHookSenderFlag = case otms of
        Nothing -> True
        Just (OptionalOwnerHook _) -> True
        _ -> False
      owHookReceiverFlag = case owtmr of
        Nothing -> True
        Just (OptionalOwnerHook _) -> True
        _ -> False
      in customFlag && operatorTransferFlag &&
         owHookSenderFlag && owHookReceiverFlag

mkInitialStorage :: OriginationParams -> Maybe Storage
mkInitialStorage op@OriginationParams{..} =
  if originationRequestCompatible op then let
    ledgerMap = opBalances
    mintingAllowances = #minting_allowances .! (BigMap opMinters)
    operatorMap = Map.foldrWithKey foldFn mempty opOwnerToOperators
    ledger = #ledger .! (BigMap ledgerMap)
    owner_ = #owner .! opOwner
    pauser_ = #pauser .! opPauser
    masterMinter_ = #master_minter .! opMasterMinter
    roles = #roles .! ((masterMinter_, owner_), (pauser_, (#pending_owner_address .! Nothing)))
    operators = #operators .! (BigMap operatorMap)
    isPaused = #paused .! opPaused
    safelistContract = #safelist_contract .! (unTAddress <$> opSafelistContract)
    totalSupply = #total_supply .! (sum $ Map.elems ledgerMap)
  in Just (((ledger, mintingAllowances), (operators, isPaused))
           , ((roles, safelistContract), totalSupply))
  else Nothing
  where
    foldFn
      :: Address
      -> [Address]
      -> Map (Address, Address) ()
      -> Map (Address, Address) ()
    foldFn ow ops m = foldr (\a b -> Map.insert (ow, a) () b) m ops
