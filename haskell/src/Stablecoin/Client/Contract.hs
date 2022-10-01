-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( InitialStorageOptions(..)
  , InitialStorageData(..)
  , mkInitialStorage
  ) where

import Lorentz as L
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import Lorentz.Contracts.Stablecoin (Expiry, Roles(..), Storage(..))
import Morley.Tezos.Address (ContractAddress, L1Address)
import Stablecoin.Client.L1AddressOrAlias (ContractAddressOrAlias, L1AddressOrAlias)
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

-- | The options needed to create the data for the contract's initial storage.
data InitialStorageOptions = InitialStorageOptions
  { isoMasterMinter :: L1AddressOrAlias
  , isoContractOwner :: L1AddressOrAlias
  , isoPauser :: L1AddressOrAlias
  , isoTransferlist :: Maybe ContractAddressOrAlias
  , isoTokenSymbol :: Text
  , isoTokenName :: Text
  , isoTokenDecimals :: Natural
  , isoContractMetadataStorage :: ContractMetadataOptions
  , isoDefaultExpiry :: Expiry
  }

-- | The data needed in order to create the stablecoin contract's initial storage.
data InitialStorageData = InitialStorageData
  { isdMasterMinter :: L1Address
  , isdContractOwner :: L1Address
  , isdPauser :: L1Address
  , isdTransferlist :: Maybe ContractAddress
  , isdTokenSymbol :: Text
  , isdTokenName :: Text
  , isdTokenDecimals :: Natural
  , isdContractMetadataStorage :: MetadataMap
  , isdDefaultExpiry :: Expiry
  }

-- | Construct the stablecoin contract's initial storage in order to deploy it.
mkInitialStorage :: InitialStorageData -> Storage
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
        { rMasterMinter = toAddress isdMasterMinter
        , rOwner = toAddress isdContractOwner
        , rPauser = toAddress isdPauser
        , rPendingOwner = Nothing
        }
    , sTransferlistContract = toAddress <$> isdTransferlist
    , sMetadata = isdContractMetadataStorage
    , sTotalSupply = 0
    }
