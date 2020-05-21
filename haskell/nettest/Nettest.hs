-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Nettest
  ( scTransferScenario
  ) where

import Lorentz hiding (comment, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.FA2
import Michelson.Typed (untypeValue)
import qualified Michelson.Untyped as U
import Morley.Nettest
import Util.Named

import Lorentz.Contracts.Stablecoin

scTransferScenario
  :: (OriginationParams -> Storage)
  -> U.Contract
  -> NettestScenario
scTransferScenario mkInitialStorage contract_ = uncapsNettest $ do
  admin <- resolveNettestAddress

  owner1 <- newAddress "Steve"
  owner2 <- newAddress "Megan"
  owner3 <- newAddress "Karen"

  operator <- newAddress "Some operator"

  let
    initialStorage =
        addAccount (admin , ([operator], 111))
      . addAccount (owner1, ([operator], 100))
      . addAccount (owner2, ([operator], 0))
      . addAccount (owner3, ([operator], 0))
      $ defaultOriginationParams

  scAddress <- do
    let str = mkInitialStorage initialStorage
    originateUntypedSimple "nettest.Stablecoin" (untypeValue $ toVal str) contract_

  let
    sc :: AddressOrAlias
    sc = AddressResolved scAddress

    tp :: Address -> Address -> Natural -> FA2.TransferItem
    tp from to value = (#from_ .! from, (#to_ .! to, (#token_id .! 0, #amount .! value)))

    -- In this transfer sender and @from@ address are always equal.
    callTransfer from to value =
      callFrom (AddressResolved from) sc (ep "transfer") ([tp from to value])

  comment "Transfer from admin to admin"
  callTransfer admin admin 111
  callTransfer admin owner1 20
