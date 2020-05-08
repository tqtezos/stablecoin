-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Stablecoin
  ( Parameter
  , Storage
  , mkPermissionDescriptor
  , mkTokenMetadata
  ) where

import Text.Show (Show)
import Lorentz
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import qualified Lorentz as L
import Util.Named

-- Parameter
type Parameter = FA2.Parameter

-- Permissions descriptor
data OwHookOptReq = OptOH () | ReqOp ()
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, L.HasTypeAnn)

data OwHook =  OwNoOp () | OwOptReq OwHookOptReq
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, L.HasTypeAnn)

type PermissionsDescriptor =
  (((Maybe FA2.CustomPermissionPolicy, FA2.OperatorTransferMode), (OwHook, FA2.SelfTransferMode)), OwHook)

mkTokenMetadata :: FA2.TokenMetadata -> TokenMetadata
mkTokenMetadata
  ( L.arg #token_id -> token_id
  , L.arg #mdr -> (L.arg #symbol -> symbol
  , L.arg #mdr2 -> (L.arg #name -> name
  , L.arg #mdr3 -> (L.arg #decimals -> decimals
  , L.arg #extras -> extras)))) = (((decimals, extras), (name, symbol)), token_id)

mkPermissionDescriptor :: FA2.PermissionsDescriptor -> PermissionsDescriptor
mkPermissionDescriptor (L.arg #self -> st, L.arg #pdr -> (L.arg #operator -> ot, L.arg #pdr2 -> (L.arg #receiver -> owtmr, L.arg #pdr3 -> (L.arg #sender -> otms, L.arg #custom -> cst)))) = let
  custom = case cst of
    Just (L.arg #tag -> tag, L.arg #config_api -> config_api) -> Just (#tag .! tag, #config_api .! config_api)
    Nothing -> Nothing

  owHookRec = case owtmr of
    FA2.OptionalOwnerHook _ -> OwOptReq (OptOH ())
    FA2.OwnerNoOp _ -> OwNoOp ()
    FA2.RequiredOwnerHook _ -> OwOptReq (ReqOp ())

  owHookSend = case otms of
    FA2.OptionalOwnerHook _ -> OwOptReq (OptOH ())
    FA2.OwnerNoOp _ -> OwNoOp ()
    FA2.RequiredOwnerHook _ -> OwOptReq (ReqOp ())

  in (((custom, ot), (owHookRec, st)), owHookSend)

-- Metadata
type TokenMetadataDecimalsAndExtra = (Natural, Map L.MText L.MText)
type TokenMetadataNameAndSymbol = (L.MText, L.MText)
type TokenMetadata = ((TokenMetadataDecimalsAndExtra, TokenMetadataNameAndSymbol), FA2.TokenId)

--- Storage
type Ledger = BigMap Address Natural

type Operators = BigMap (Address, Address) ()

type TotalSupply = Natural

type Storage = (((Bool, Ledger), (Operators, PermissionsDescriptor)), (TokenMetadata, TotalSupply))

