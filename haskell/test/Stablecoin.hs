-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin
  ( spec_Stablecoin
  , spec_StablecoinNettest
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Hspec (Spec, describe, it)

import Lorentz (TAddress(..), arg)
import Lorentz.Address (Address)
import Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin as SC
import Lorentz.Contracts.Test.FA2
import Michelson.Test.Import
import Michelson.Test.Integrational
import Michelson.Typed hiding (TAddress)
import qualified Michelson.Untyped as U
import qualified Morley.Nettest as NetTest
import Nettest
import Tezos.Core

contractPath :: FilePath
contractPath = "test/resources/stablecoin.tz"

spec_Stablecoin :: Spec
spec_Stablecoin = specWithUntypedContract contractPath specCallback

spec_StablecoinNettest :: Spec
spec_StablecoinNettest = specWithUntypedContract contractPath nettest_scenario

nettest_scenario :: U.Contract  -> Spec
nettest_scenario contract = describe "Stablecount nettest (integrational)" $
  it "can be originated and execute a transfer call" $
    integrationalTestExpectation $
    NetTest.nettestToIntegrational $
    scTransferScenario (fromJust . mkInitialStorage) contract

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
