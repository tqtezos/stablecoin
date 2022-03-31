-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( InitialStorageData(..)
  , mkInitialStorage
  ) where

import Morley.Client (AddressOrAlias)

import Lorentz as L
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import Lorentz.Contracts.Stablecoin (Expiry, Roles(..), Storage(..))
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

type family ComputeRegistryAddressType a where
  ComputeRegistryAddressType Address = Address
  ComputeRegistryAddressType AddressOrAlias = Maybe AddressOrAlias

type family ComputeContractMetadataStorageType a where
  ComputeContractMetadataStorageType Address = MetadataMap
  ComputeContractMetadataStorageType AddressOrAlias = ContractMetadataOptions

-- | The data needed in order to create the stablecoin contract's initial storage.
-- This is used in two slightly different contexts, and some field types require
-- changes accordingly.
--
-- 1. As an input to the `deploy` function when the
-- registry contract addresses only contains the configurations for metadata contracts.
--
-- 2. As an input to the function that creates the raw initial storage value
-- for the contract. There these fields will contain resolved addresses.
data InitialStorageData addr = InitialStorageData
  { isdMasterMinter :: addr
  , isdContractOwner :: addr
  , isdPauser :: addr
  , isdTransferlist :: Maybe addr
  , isdTokenSymbol :: Text
  , isdTokenName :: Text
  , isdTokenDecimals :: Natural
  , isdContractMetadataStorage :: ComputeContractMetadataStorageType addr
  , isdDefaultExpiry :: Expiry
  }

-- | Construct the stablecoin contract's initial storage in order to deploy it.
mkInitialStorage :: InitialStorageData Address -> Storage
mkInitialStorage InitialStorageData {..} =
  Storage
    { sDefaultExpiry = isdDefaultExpiry
    , sLedger = def
    , sMintingAllowances = mempty
    , sOperators = def
    , sPaused = False
    , sPermitCounter = 0
    , sPermits = def
    , sRoles = Roles
        { rMasterMinter = isdMasterMinter
        , rOwner = isdContractOwner
        , rPauser = isdPauser
        , rPendingOwner = Nothing
        }
    , sTransferlistContract = isdTransferlist
    , sMetadata = isdContractMetadataStorage
    , sTotalSupply = 0
    }
