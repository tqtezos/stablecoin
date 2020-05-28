-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_FA2
  , spec_Management
  ) where

import Test.Hspec (Spec)

import Lorentz (TAddress(..))
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Contracts.Test.Common hiding (owner, pauser, masterMinter)
import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Test.Management
import Michelson.Test.Import
import Michelson.Test.Integrational
import Michelson.Typed hiding (TAddress)
import qualified Michelson.Untyped as U
import Tezos.Core

contractPath :: FilePath
contractPath = "test/resources/stablecoin.tz"

origination :: forall param . U.Contract -> OriginationFn param
origination contract (mkInitialStorage -> Just storageVal) = pure . TAddress @param <$>
    originate contract "Stablecoin contract" (untypeValue $ toVal storageVal) (unsafeMkMutez 0)
origination _ _ = pure Nothing

spec_FA2 :: Spec
spec_FA2 = specWithUntypedContract contractPath $ fa2Spec . origination @FA2.Parameter

spec_Management :: Spec
spec_Management = specWithUntypedContract contractPath $ managementSpec . origination @SC.Parameter
