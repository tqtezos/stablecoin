-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Util
  ( ligoVersion
  ) where

import Language.Haskell.TH.Syntax as TH (Lift(lift))
import System.Environment (lookupEnv)

-- | Load the @ligo@ version we use to compile the ligo contract
-- from the environment variable which was set up during the nix build.
ligoVersion :: Text
ligoVersion =
  $(TH.lift =<< liftIO (maybe "unknown" toText <$> lookupEnv "STABLECOIN_LIGO_VERSION"))
