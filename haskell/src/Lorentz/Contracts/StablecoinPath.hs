-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains only the paths to the compiled LIGO contracts.
--
-- Due to GHC stage restriction, they have to be in their own module in order
-- to be used inside TemplateHaskell splices.
module Lorentz.Contracts.StablecoinPath
  ( stablecoinPath
  , metadataRegistryContractPath

  ) where

-- | The path to the compiled stablecoin contract.
stablecoinPath :: FilePath
stablecoinPath = "./test/resources/stablecoin.tz"

-- | The path to the compiled metadata registry.
metadataRegistryContractPath :: FilePath
metadataRegistryContractPath = "./test/resources/metadata.tz"
