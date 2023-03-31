-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

-- | Test commons for stablecoin test suite

module Lorentz.Contracts.Test.Common
  ( oneTokenId
  , OriginationParams (..)
  , defaultOriginationParams
  , addAccount
  , addOperator
  , addMinter
  , constructTransfers
  , constructTransfersFromSender
  , constructSingleTransfer
  , mgmContractPaused
  , originateStablecoin
  , mkInitialStorage
  , nettestOriginateContractMetadataContract
  , testFA2TokenMetadata
  , expectFailedWithAny
  ) where

import Data.Aeson (ToJSON)
import Data.Map qualified as Map

import Lorentz.Contracts.Spec.TZIP16Interface qualified as MD
import Lorentz.Value
import Morley.Util.Named
import Test.Cleveland as NT

import Indigo.Contracts.Transferlist.Internal qualified as Transferlist
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Test.Cleveland.Internal.Actions

-- | A 'TokenId' with the value of `1`.
-- This is used only for tests because @stablecoin@ only supports a single token
-- with the value of 0, aka 'theTokenId'.
oneTokenId :: TokenId
oneTokenId = TokenId 1

type LedgerType = Map Address Natural

addAccount
  :: (ToAddress addr1)
  => (addr1, ([ImplicitAddressWithAlias], Natural)) -> OriginationParams -> OriginationParams
addAccount (addr, (operators, bal)) op = let
  withAccount = op
    { opBalances =
        Map.insert (toAddress addr) bal $ opBalances op
    }
  in foldl' (\oparams operator -> addOperator (addr, operator) oparams) withAccount operators

addOperator
  :: (ToAddress addr1, ToAddress addr2)
  => (addr1, addr2) -> OriginationParams -> OriginationParams
addOperator (toAddress -> owner_, toAddress -> operator) op = op
  { opOwnerToOperators =
      Map.alter (\case
          Just ops -> Just $ operator:ops
          Nothing -> Just [operator]) owner_ $ opOwnerToOperators op
  }

failedWithAny :: TransferFailurePredicate
failedWithAny = failedWithPredicate "Expected contract to fail with FAILWITH" $ const True

-- TODO: seems like a good idea to move this to cleveland
expectFailedWithAny :: MonadCleveland caps m => m () -> m ()
expectFailedWithAny = expectTransferFailure failedWithAny

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
  , opTransferlistContract :: Maybe (TAddress Transferlist.Parameter ())
  , opDefaultExpiry :: Natural
  , opPermits :: Map Address UserPermits
  }

defaultOriginationParams
  :: (ToAddress owner, ToAddress pauser, ToAddress masterMinter)
  => "owner" :! owner
  -> "pauser" :! pauser
  -> "masterMinter" :! masterMinter
  -> OriginationParams
defaultOriginationParams
  (arg #owner -> owner)
  (arg #pauser -> pauser)
  (arg #masterMinter -> masterMinter) = OriginationParams
    { opBalances = mempty
    , opOwner = toAddress owner
    , opOwnerToOperators = mempty
    , opPauser = toAddress pauser
    , opMasterMinter = toAddress masterMinter
    , opPaused = False
    , opMinters = mempty
    , opPendingOwner = Nothing
    , opMetadataUri = CurrentContract metadata True
    , opTransferlistContract = Nothing
    , opDefaultExpiry = 1000
    , opPermits = mempty
    }
    where
      metadata = metadataJSON (Just testFA2TokenMetadata) Nothing


addMinter
  :: ToAddress addr
  => (addr, Natural)
  -> OriginationParams
  -> OriginationParams
addMinter (toAddress -> minter, mintingAllowance) op@OriginationParams{ opMinters = currentMinters } =
  op { opMinters = Map.insert minter mintingAllowance currentMinters }

constructDestination
  :: ToImplicitAddress to
  => ("to_" :! to, "amount" :! Natural)
  -> TransferDestination
constructDestination (arg #to_ -> to, arg #amount -> amount) = TransferDestination
  { tdTo = toAddress $ toImplicitAddress to
  , tdTokenId = FA2.theTokenId
  , tdAmount = amount
  }

constructTransfers
  :: [ ( "from_" :! ImplicitAddressWithAlias
       , [("to_" :! ImplicitAddressWithAlias, "amount" :! Natural)])
     ]
  -> TransferParams
constructTransfers pairs = pairs >>= uncurry constructTransfersFromSender

constructTransfersFromSender
  :: "from_" :! ImplicitAddressWithAlias
  -> [("to_" :! ImplicitAddressWithAlias, "amount" :! Natural)]
  -> TransferParams
constructTransfersFromSender (arg #from_ -> from) txs =
  [ TransferItem
      { tiFrom = toAddress $ toImplicitAddress from
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

originateStablecoin :: MonadCleveland caps m => OriginationParams -> m (ContractHandle SC.Parameter SC.Storage ())
originateStablecoin originationParams =
  originate
    "Stablecoin contract"
    (mkInitialStorage originationParams)
    stablecoinContract

mkInitialStorage :: OriginationParams -> Storage
mkInitialStorage OriginationParams{..} =
  Storage
    { sDefaultExpiry = opDefaultExpiry
    , sLedger = mkBigMap opBalances
    , sMintingAllowances = opMinters
    , sOperators = mkBigMap (Map.foldrWithKey foldFn mempty opOwnerToOperators)
    , sPaused = opPaused
    , sPermitCounter = 0
    , sPermits = mkBigMap opPermits
    , sRoles = Roles
        { rMasterMinter = opMasterMinter
        , rOwner = opOwner
        , rPauser = opPauser
        , rPendingOwner = opPendingOwner
        }
    , sTransferlistContract = toAddress <$> opTransferlistContract
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

mgmContractPaused :: MonadCleveland caps m => m () -> m ()
mgmContractPaused = expectFailedWith [mt|CONTRACT_PAUSED|]

nettestOriginateContractMetadataContract :: (ToJSON metadata) => MonadCleveland caps m => metadata -> m (ContractHandle () MetadataRegistryStorage ())
nettestOriginateContractMetadataContract mdata =
  originate
    "nettest.ContractMetadata"
    (mkContractMetadataRegistryStorage $  metadataMap (CurrentContract mdata False))
    contractMetadataContract

testFA2TokenMetadata :: FA2.TokenMetadata
testFA2TokenMetadata = FA2.mkTokenMetadata "TEST" "TEST" "3"
