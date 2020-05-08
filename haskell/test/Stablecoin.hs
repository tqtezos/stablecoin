-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_Stablecoin
  ) where

import Test.Hspec

import Tezos.Core
import qualified Data.Map as Map
import Lorentz.Address as LA
import Michelson.Test.Import
import Michelson.Test.Integrational
import Michelson.Typed
import qualified Michelson.Untyped as U
import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Stablecoin as SC

spec_Stablecoin :: Spec
spec_Stablecoin = specWithUntypedContract "../out/stablecoin.tz" specCallback

mkInitialStorage :: OriginationParams -> Storage
mkInitialStorage op = let
  ledgerMap = snd <$> (opBalances op)
  operatorMap = Map.foldrWithKey foldFn mempty (opBalances op)
  in (((False, BigMap ledgerMap), (BigMap operatorMap , mkPermissionDescriptor $ opPermissionsDescriptor op)), (mkTokenMetadata $ opTokenMetadata op, sum $ Map.elems ledgerMap ))
  where
    foldFn :: Address -> ([Address], Natural) -> Map (Address, Address) () -> Map (Address, Address) ()
    foldFn ow (ops, _) m = foldr (\a b -> Map.insert (ow, a) () b) m ops

specCallback :: U.Contract -> Spec
specCallback c = fa2Spec $ \op ->
  let storageVal = mkInitialStorage op
  in do
    scContract  <- originate c "Stablecoin contract" (untypeValue $ toVal storageVal) (unsafeMkMutez 0)
    pure (LA.TAddress scContract :: LA.TAddress Parameter)
