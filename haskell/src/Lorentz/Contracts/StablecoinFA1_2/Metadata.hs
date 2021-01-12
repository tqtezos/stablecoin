-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This module contains the TZIP-16 metadata and off-chain-views for the
-- @stablecoin.fa1.2.tz@ contract.
module Lorentz.Contracts.StablecoinFA1_2.Metadata
  ( metadataJSON
  ) where

import Data.Version (showVersion)
import Lorentz as L
import Lorentz.Contracts.Spec.TZIP16Interface (Metadata(..), ViewImplementation(..))
import Morley.Metadata (ViewCode(..), compileViewCodeTH, mkMichelsonStorageView)

import qualified Lorentz.Contracts.Spec.TZIP16Interface as TZ
import Lorentz.Contracts.StablecoinFA1_2.Types (Storage)
import Paths_stablecoin (version)

metadataJSON :: Metadata (ToT Storage)
metadataJSON =
  TZ.name "stablecoin FA1.2" <>
  TZ.interfaces [ TZ.Interface "TZIP-007", TZ.Interface "TZIP-017" ] <>
  TZ.views
      [ getDefaultExpiryView
      , getCounterView
      ] <>
  TZ.source
    (TZ.Source
      (Just $ "https://github.com/tqtezos/stablecoin/tree/v" <> toText (showVersion version) <> "/ligo/stablecoin/fa1.2")
      [ "ligo " ]) <>
  TZ.homepage "https://github.com/tqtezos/stablecoin/"

getDefaultExpiryView :: TZ.View (ToT Storage)
getDefaultExpiryView =
  TZ.View
    { vName = "GetDefaultExpiry"
    , vDescription = Just "Access the contract's default expiry in seconds"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $
            $$(compileViewCodeTH $ WithoutParam $
              L.toField #sDefaultExpiry
            )
    }

getCounterView :: TZ.View (ToT Storage)
getCounterView =
  TZ.View
    { vName = "GetCounter"
    , vDescription = Just "Access the current permit counter"
    , vPure = Just True
    , vImplementations = one $
        VIMichelsonStorageView $
          mkMichelsonStorageView @Storage @Natural Nothing [] $
            $$(compileViewCodeTH $ WithoutParam $
              L.toField #sPermitCounter
            )
    }
