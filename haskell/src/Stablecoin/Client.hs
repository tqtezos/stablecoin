-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client
  ( module Stablecoin.Client.Impl
  , InitialStorageData(..)
  , Alias
  ) where

import Morley.Client (Alias)

import Stablecoin.Client.Contract
import Stablecoin.Client.Impl
