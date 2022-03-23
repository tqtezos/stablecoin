-- SPDX-FileCopyrightText: 2022 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Test.AsRPC
  () where

import Lorentz.Contracts.Spec.FA2Interface (BalanceResponseItem, FA2OwnerHook)
import Lorentz.Contracts.Stablecoin (Storage, Storage')

import Lorentz.Value (BigMapId)
import Test.Cleveland (AsRPC)

type instance AsRPC BalanceResponseItem = BalanceResponseItem
type instance AsRPC Storage = Storage' BigMapId
type instance AsRPC FA2OwnerHook = FA2OwnerHook
