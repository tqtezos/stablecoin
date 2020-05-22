-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_Stablecoin
  ) where

import Test.Hspec (Spec)

import Lorentz (TAddress(..))
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.FA2
import Michelson.Test.Import
import Michelson.Test.Integrational
import Michelson.Typed hiding (TAddress)
import qualified Michelson.Untyped as U
import Tezos.Core

import Lorentz.Contracts.Test.Common

contractPath :: FilePath
contractPath = "test/resources/stablecoin.tz"

spec_Stablecoin :: Spec
spec_Stablecoin = specWithUntypedContract contractPath specCallback

specCallback :: U.Contract -> Spec
specCallback c = fa2Spec $ \op ->
  case mkInitialStorage op of
    Just storageVal ->
      -- We just cast the stablecoin contract address as an FA2 contract
      -- as the FA2 test only call specific entrypoints and never call
      -- the contract using the full parameter.
      (Just . (TAddress @FA2.Parameter)) <$>
          originate c "Stablecoin contract" (untypeValue $ toVal storageVal) (unsafeMkMutez 0)
    Nothing -> pure Nothing
