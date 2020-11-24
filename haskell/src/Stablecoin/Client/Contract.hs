-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( mkInitialStorage
  , InitialStorageData(..)
  ) where

import Michelson.Text (MText)
import Morley.Client (AddressOrAlias)
import Tezos.Address (Address)

import Lorentz.Contracts.Stablecoin (Expiry, Roles(..), Storage, Storage'(..), metadataMap)

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
    , sMetadata = metadataMap
    }
