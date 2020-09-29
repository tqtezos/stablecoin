-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( mkInitialStorage
  , mkRegistryStorage
  , InitialStorageData(..)
  ) where

import qualified Data.Map.Strict as Map
import Michelson.Text (MText)
import Michelson.Typed (BigMap(BigMap))
import Morley.Client (AddressOrAlias)
import Tezos.Address (Address)

import Lorentz.Contracts.Spec.FA2Interface (TokenMetadata(..))
import Lorentz.Contracts.Stablecoin
  (Expiry, MetadataRegistryStorage, Roles(..), Storage, Storage'(..), mkMetadataRegistryStorage)

type family ComputeRegistryAddressType a where
  ComputeRegistryAddressType Address = Address
  ComputeRegistryAddressType AddressOrAlias = Maybe AddressOrAlias

-- | The data needed in order to create the stablecoin contract's initial storage.
data InitialStorageData addr = InitialStorageData
  { isdMasterMinter :: addr
  , isdContractOwner :: addr
  , isdPauser :: addr
  , isdTransferlist :: Maybe addr
  , isdTokenSymbol :: MText
  , isdTokenName :: MText
  , isdTokenDecimals :: Natural
  , isdTokenMetadataRegistry :: ComputeRegistryAddressType addr
  , isdDefaultExpiry :: Expiry
  }

-- | Construct the stablecoin contract's initial storage in order to deploy it.
mkInitialStorage :: InitialStorageData Address -> Storage
mkInitialStorage (InitialStorageData {..}) =
  Storage
    { sDefaultExpiry = isdDefaultExpiry
    , sLedger =  mempty
    , sMintingAllowances = mempty
    , sOperators = mempty
    , sIsPaused = False
    , sPermitCounter = 0
    , sPermits = mempty
    , sRoles = Roles
        { rMasterMinter = isdMasterMinter
        , rOwner = isdContractOwner
        , rPauser = isdPauser
        , rPendingOwner = Nothing
        }
    , sTokenMetadataRegistry = isdTokenMetadataRegistry
    , sTransferlistContract = isdTransferlist
    }

-- | Constuct the stablecoin metadata
mkRegistryStorage :: MText -> MText -> Natural -> MetadataRegistryStorage
mkRegistryStorage symbol name decimals = mkMetadataRegistryStorage $ BigMap $ Map.singleton 0 $
  TokenMetadata
    { tmTokenId = 0
    , tmSymbol = symbol
    , tmName = name
    , tmDecimals = decimals
    , tmExtras = mempty
    }
