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
import qualified Lorentz as L
import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Spec.FA2Interface hiding (Parameter)
import Lorentz.Contracts.Stablecoin as SC

spec_Stablecoin :: Spec
spec_Stablecoin = specWithUntypedContract "../out/stablecoin.tz" specCallback

originationRequestCompatible :: OriginationParams -> Bool
originationRequestCompatible op =
  isPermissionsCompatible (opPermissionsDescriptor op)
  where
    isPermissionsCompatible :: PermissionsDescriptorMaybe -> Bool
    isPermissionsCompatible
      (L.arg #self -> st, L.arg #pdr -> (L.arg #operator -> ot, L.arg #pdr2 -> (L.arg #receiver -> owtmr, L.arg #pdr3 -> (L.arg #sender -> otms, L.arg #custom -> cst)))) = let
      customFlag = not $ isJust cst
      selfTransferFlag = case st of
        Nothing -> True
        Just (SelfTransferPermitted _) -> True
        _ -> False
      operatorTransferFlag = case ot of
        Nothing -> True
        Just (OperatorTransferPermitted _) -> True
        _ -> False
      owHookSenderFlag = case otms of
        Nothing -> True
        Just (OptionalOwnerHook _) -> True
        _ -> False
      owHookReceiverFlag = case owtmr of
        Nothing -> True
        Just (OptionalOwnerHook _) -> True
        _ -> False
      in customFlag && selfTransferFlag && operatorTransferFlag &&
         owHookSenderFlag && owHookReceiverFlag

mkInitialStorage :: OriginationParams -> Maybe Storage
mkInitialStorage op =
  if originationRequestCompatible op then let
    ledgerMap = snd <$> (opBalances op)
    operatorMap = Map.foldrWithKey foldFn mempty (opBalances op)
    in Just (((False, BigMap ledgerMap), (BigMap operatorMap, stablecoinPermissionsDescriptor))
             , (stablecoinTokenMetadata, sum $ Map.elems ledgerMap ))
  else Nothing
  where
    foldFn :: Address -> ([Address], Natural) -> Map (Address, Address) () -> Map (Address, Address) ()
    foldFn ow (ops, _) m = foldr (\a b -> Map.insert (ow, a) () b) m ops

specCallback :: U.Contract -> Spec
specCallback c = fa2Spec $ \op ->
  case mkInitialStorage op of
    Just storageVal -> (Just . (LA.TAddress @Parameter)) <$> originate c "Stablecoin contract" (untypeValue $ toVal storageVal) (unsafeMkMutez 0)
    Nothing -> pure Nothing
