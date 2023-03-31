-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Cleveland
  ( module Stablecoin.Client.Cleveland.Caps
  , Alias
  , AddressAndAlias(..)
  ) where

import Morley.Tezos.Address.Alias (Alias)

import Stablecoin.Client (AddressAndAlias(..))
import Stablecoin.Client.Cleveland.Caps
