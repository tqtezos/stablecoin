-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( InitialStorageOptions(..)
  ) where

import Lorentz.Contracts.Stablecoin (Expiry)
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
