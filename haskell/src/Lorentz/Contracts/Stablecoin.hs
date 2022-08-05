-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Stablecoin
  ( module Export

  -- * Embedded LIGO contracts
  , contractMetadataContract
  , stablecoinContract
  ) where

import Test.Cleveland.Lorentz.Import (embedContract)

import Lorentz (Contract)
import Lorentz.Contracts.Stablecoin.Metadata as Export
import Lorentz.Contracts.Stablecoin.Types as Export
import Lorentz.Contracts.StablecoinPath (metadataRegistryContractPath, stablecoinPath)

stablecoinContract :: Contract Parameter Storage ()
stablecoinContract =
  -- This TemplateHaskell splice is here to ensure the Michelson representation
  -- (i.e., the generated tree of `or` and `pair`) of these Haskell data
  -- types matches exactly the Michelson representation of the Ligo data types.
  --
  -- For example, if a Ligo data type generates a Michelson balanced tuple like
  -- ((a, b), (c, d)), but the corresponding Haskell data type generates a Michelson
  -- tuple like (a, (b, (c, d))), compilation should fail.
  --
  -- The reason they need to match is because of the way permits work.
  -- If we issue a permit for an entrypoint and the tree of `or`s is incorrect,
  -- then the permit will be unusable.
  -- See TZIP-017 for more info.
  --
  -- The splice attempts to parse the stablecoin.tz contract at compile-time.
  -- If, for example, the Michelson representation of Haskell's
  -- `Lorentz.Contracts.Stablecoin.Parameter` is different from Ligo's `parameter`,
  -- then this splice will raise a compilation error.
  --
  -- This can usually be fixed by writing a custom instance of Generic using `customGeneric`,
  -- and making sure the data type's layout matches the layout in stablecoin.tz.
  -- See examples in this module.
  $$(embedContract @Parameter @Storage stablecoinPath)

-- | Parse the contract-metadata contract.
contractMetadataContract :: Contract () MetadataRegistryStorage ()
contractMetadataContract =
  $$(embedContract @() @MetadataRegistryStorage metadataRegistryContractPath)
