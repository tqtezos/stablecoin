-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Nettest
  ( scTransferScenario
  ) where

import Lorentz hiding (comment, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.Common
import Michelson.Typed (untypeValue)
import qualified Michelson.Untyped as U
import Morley.Nettest
import Util.Named

import Lorentz.Contracts.Stablecoin

scTransferScenario
  :: (OriginationParams -> Storage)
  -> U.Contract
  -> NettestScenario
scTransferScenario constructInitialStorage contract_ = uncapsNettest $ do
  superuser <- resolveNettestAddress

  owner1 <- newAddress "Steve"
  owner2 <- newAddress "Megan"
  owner3 <- newAddress "Karen"

  operator <- newAddress "Some operator"

  let
    initialStorage =
        addAccount (superuser , ([operator], 111))
      . addAccount (owner1, ([operator], 100))
      . addAccount (owner2, ([operator], 0))
      . addAccount (owner3, ([operator], 0))
      $ defaultOriginationParams

  scAddress <- do
    let str = constructInitialStorage initialStorage
    originateUntypedSimple "nettest.Stablecoin" (untypeValue $ toVal str) contract_

  let
    sc :: AddressOrAlias
    sc = AddressResolved scAddress

    tp :: Address -> Address -> Natural -> FA2.TransferParams
    tp from to value = constructSingleTransfer
      (#from_ .! from)
      (#to_ .! to)
      (#amount .! value)

    -- In this transfer sender and @from@ address are always equal.
    callTransfer from to value =
      callFrom (AddressResolved from) sc (ep "transfer") (tp from to value)

  comment "Transfer from superuser to superuser"
  callTransfer superuser superuser 111
  callTransfer superuser owner1 20
