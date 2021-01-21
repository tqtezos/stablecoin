-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains only the paths to the compiled LIGO contracts.
--
-- Due to GHC stage restriction, they have to be in their own module in order
-- to be used inside TemplateHaskell splices.
module Lorentz.Contracts.StablecoinPath
  ( metadataRegistryContractPath
  , stablecoinFA1_2Path
  , stablecoinPath
  , sourcesJsonPath
  ) where

-- | The path to the compiled stablecoin contract.
stablecoinPath :: FilePath
stablecoinPath = "./test/resources/stablecoin.tz"

-- | The path to the compiled stablecoin FA1.2 contract.
stablecoinFA1_2Path :: FilePath
stablecoinFA1_2Path = "./test/resources/stablecoin.fa1.2.tz"

-- | The path to the compiled metadata registry.
metadataRegistryContractPath :: FilePath
metadataRegistryContractPath = "./test/resources/metadata.tz"

-- | The path to nix's sources.json file.
sourcesJsonPath :: FilePath
sourcesJsonPath = "./test/resources/sources.json"
