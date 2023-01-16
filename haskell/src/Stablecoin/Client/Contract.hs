-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( InitialStorageOptions(..)
  ) where

import Lorentz.Contracts.Stablecoin (Expiry)
import Morley.Tezos.Address.Alias (ContractAddressOrAlias, SomeAddressOrAlias)
import Stablecoin.Client.Parser (ContractMetadataOptions(..))

-- | The options needed to create the data for the contract's initial storage.
data InitialStorageOptions = InitialStorageOptions
  { isoMasterMinter :: SomeAddressOrAlias
  , isoContractOwner :: SomeAddressOrAlias
  , isoPauser :: SomeAddressOrAlias
  , isoTransferlist :: Maybe ContractAddressOrAlias
  , isoTokenSymbol :: Text
  , isoTokenName :: Text
  , isoTokenDecimals :: Natural
  , isoContractMetadataStorage :: ContractMetadataOptions
  , isoDefaultExpiry :: Expiry
  }
