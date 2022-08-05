-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.StablecoinFA1_2
  ( module Export

  -- * Embedded LIGO contracts
  , stablecoinFA1_2Contract
  ) where

import Lorentz
import Test.Cleveland.Lorentz.Import (embedContract)

import Lorentz.Contracts.StablecoinFA1_2.Metadata as Export
import Lorentz.Contracts.StablecoinFA1_2.Types as Export
import Lorentz.Contracts.StablecoinPath (stablecoinFA1_2Path)

-- | Parse the FA1.2 variant of the stablecoin contract.
stablecoinFA1_2Contract :: Contract Parameter Storage ()
stablecoinFA1_2Contract =
  $$(embedContract @Parameter @Storage stablecoinFA1_2Path)
