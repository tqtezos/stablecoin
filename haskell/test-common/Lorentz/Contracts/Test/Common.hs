-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT
module Lorentz.Contracts.Test.Common
  ( mkInitialStorage
  ) where

import qualified Data.Map as Map

import Lorentz (arg)
import Lorentz.Address (Address)
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Contracts.Test.FA2
import Michelson.Typed hiding (TAddress)

-- | This function accepts origination params with @Just@ values indicating
-- the mandatory configuration, and @Nothing@ values for permission parameters
-- that are not relavant for the test. The return flag indicate if the
-- contract's permission parameter can meet the required permission configuration.
originationRequestCompatible :: OriginationParams -> Bool
originationRequestCompatible op =
  isPermissionsCompatible (opPermissionsDescriptor op)
  where
    isPermissionsCompatible :: PermissionsDescriptorMaybe -> Bool
    isPermissionsCompatible
      ( arg #self -> st
      , arg #pdr ->
          ( arg #operator -> ot
          , arg #pdr2 ->
            ( arg #receiver -> owtmr
            , arg #pdr3 -> (arg #sender -> otms, arg #custom -> cst)))) = let
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
    in Just (((BigMap ledgerMap, BigMap operatorMap)
              , (stablecoinPermissionsDescriptor, stablecoinTokenMetadata))
            , sum $ Map.elems ledgerMap)
  else Nothing
  where
    foldFn
      :: Address
      -> ([Address], Natural)
      -> Map (Address, Address) ()
      -> Map (Address, Address) ()
    foldFn ow (ops, _) m = foldr (\a b -> Map.insert (ow, a) () b) m ops
