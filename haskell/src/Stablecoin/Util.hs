-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Util
  ( aesonOptions
  , ligoVersion
  ) where

import Data.Aeson (Options(omitNothingFields))
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Language.Haskell.TH.Syntax as TH (Exp, Lift(lift), Q)
import System.Environment (lookupEnv)

aesonOptions :: Options
aesonOptions = (aesonPrefix camelCase) { omitNothingFields = True}

-- | Load the @ligo@ version we use to compile the ligo contract
-- from the environment variable which was set up during the nix build.
ligoVersion :: Q Exp
ligoVersion = do
  liftIO (lookupEnv "STABLECOIN_LIGO_VERSION") >>= \case
    Just v -> TH.lift $ toText v
    Nothing -> TH.lift ("unknown" :: Text)
