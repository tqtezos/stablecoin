-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Dummy
  ( unit_dummy
  ) where

import Test.HUnit (Assertion)

import Michelson.Test (importUntypedContract)

-- Check that "stablecoin.tz" exists and can be parsed.
unit_dummy :: Assertion
unit_dummy = do
  void $ importUntypedContract "test/resources/stablecoin.tz"
