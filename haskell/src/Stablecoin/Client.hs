-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client
  ( module Stablecoin.Client.Impl
  , InitialStorageOptions(..)
  , Alias
  ) where

import Morley.Tezos.Address.Alias (Alias)

import Stablecoin.Client.Contract
import Stablecoin.Client.Impl
