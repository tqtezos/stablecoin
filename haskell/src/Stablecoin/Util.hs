-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Util
  ( aesonOptions
  , ligoVersion
  ) where

import Data.Aeson (Options(omitNothingFields), eitherDecodeFileStrict)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Map as Map
import Language.Haskell.TH.Syntax as TH (Exp, Lift(lift), Q)
import Lorentz.Contracts.StablecoinPath (sourcesJsonPath)
import System.Directory (makeAbsolute)

aesonOptions :: Options
aesonOptions = (aesonPrefix camelCase) { omitNothingFields = True}

-- | Load the @ligo@ version we use to compile the ligo contract
-- by parsing it from nix's @sources.json@ file.
ligoVersion :: Q Exp
ligoVersion = do
  path <- liftIO $ makeAbsolute sourcesJsonPath
  liftIO (eitherDecodeFileStrict @(Map Text NixPackageInfo) path) >>= \case
    Left err -> fail $ "Failed to parse '" <> path <> "': " <> err
    Right pkgs ->
      case Map.lookup "ligo" pkgs of
        Nothing -> fail "Could not find 'ligo' package in sources.json"
        Just pkgInfo ->
          TH.lift (npiRev pkgInfo)

data NixPackageInfo = NixPackageInfo
  { npiRev :: Text
  }
deriveJSON (aesonPrefix camelCase) ''NixPackageInfo
