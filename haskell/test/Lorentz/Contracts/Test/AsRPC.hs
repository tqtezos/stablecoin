-- SPDX-FileCopyrightText: 2022 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Test.AsRPC
  () where

import Lorentz.Contracts.Spec.FA2Interface (BalanceResponseItem, FA2OwnerHook)

import Test.Cleveland (HasRPCRepr(..))

instance HasRPCRepr BalanceResponseItem where
  type AsRPC BalanceResponseItem = BalanceResponseItem
instance HasRPCRepr FA2OwnerHook where
  type AsRPC FA2OwnerHook = FA2OwnerHook
