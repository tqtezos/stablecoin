-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Util
  ( aesonOptions
  ) where

import Data.Aeson (Options(omitNothingFields))
import Data.Aeson.Casing (aesonPrefix, camelCase)

aesonOptions :: Options
aesonOptions = (aesonPrefix camelCase) { omitNothingFields = True}
