-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

{-# OPTIONS_GHC -Wno-orphans #-}
-- | FA2 interface specification.
-- https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md

module Lorentz.Contracts.Spec.FA2Interface
  ( BalanceRequestItem
  , BalanceRequestParams
  , BalanceResponseItem
  , CustomPermissionPolicy
  , FA2OwnerHook (..)
  , IsOperatorParam
  , IsOperatorResponse
  , OperatorParam
  , OperatorTransferMode (..)
  , OwnerTransferMode (..)
  , Parameter (..)
  , ParameterC
  , PermissionsDescriptor
  , PermissionsDescriptorMaybe
  , PermissionsDescriptorParam
  , SelfTransferMode (..)
  , TokenId
  , TokenMetaDataParam
  , TokenMetadata
  , TotalSupplyRequestParams
  , TotalSupplyResponse
  , TransferItem
  , TransferDescriptor
  , TransferDescriptorParam
  , TransferParams
  , UpdateOperator (..)
  , UpdateOperatorsParam
  ) where

import Fmt (Buildable(..), genericF, (+|), (|+))
import Test.Tasty.QuickCheck (Arbitrary(..), elements)

import Lorentz
import Util.Named

-- Transfer
-- --------
-- 1. Parameter is list of transfer items.
-- 2. These items must be executed in order, and atomically.
--    If one of them cannot complete, then whole batch should be aborted.
-- 3. Tranfer should update the balances exactly as specified, and should not adjust it anyway.
-- 4. Transfer operation must apply permission policy logic.
type TokenId = Natural

type TransferItem =
  ("from_" :! Address, ("to_" :! Address, ("token_id" :! TokenId, "amount" :! Natural)))

type TransferParams = [TransferItem]

-- BalanceOf
-- ---------
-- Queries balance of one or more addresses. Callback contract should accept a list of BalanceResponse
-- Duplicates in the request should not be de-duplicated or reordered in the response.
type BalanceRequestItem = ("owner" :! Address, "token_id" :! TokenId)

instance Buildable BalanceRequestItem where
  build = genericF

type BalanceResponseItem = ("request" :! BalanceRequestItem, "balance" :! Natural)

type BalanceRequestParams = View ("requests" :! [BalanceRequestItem]) [BalanceResponseItem]

instance Buildable BalanceResponseItem where
  build = genericF

-- TotalSupply query
--
type TotalSupplyResponse = ("token_id" :! TokenId, "total_supply" :! Natural)

type TotalSupplyRequestParams = View ("token_ids" :! [TokenId]) [TotalSupplyResponse]

instance Buildable TotalSupplyResponse where
  build = genericF

-- Token MetaData query

type TokenMetadata =
  ( "token_id" :! TokenId
  , "mdr" :! ("symbol" :! MText
  , "mdr2" :! ("name" :! MText
  , "mdr3" :! ("decimals" :! Natural
  , "extras" :! Map MText MText)))
  )

type TokenMetaDataParam = View ("token_ids" :! [TokenId]) [TokenMetadata]

instance Buildable TokenMetadata where
  build
    ( arg #token_id -> token_id
    , arg #mdr -> (arg #symbol -> symbol
    , arg #mdr2 -> (arg #name -> name
    , arg #mdr3 -> (arg #decimals -> decimals
    , _)))) =
    "token_id:" +| token_id |+ ", symbol:" +| symbol |+ ", name:" +|
    name |+ ", decimals:" +| decimals |+ "."

-- Permission descriptor param components and parameter
data SelfTransferMode
  = SelfTransferPermitted ("self_transfer_permitted" :! ())
  | SelfTransferDenied ("self_transfer_denied" :! ())
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasTypeAnn)

instance Arbitrary SelfTransferMode where
  arbitrary =
    elements
      [ SelfTransferPermitted (#self_transfer_permitted .! ())
      , SelfTransferDenied (#self_transfer_denied .! ())
      ]

instance TypeHasDoc SelfTransferMode where
  typeDocMdDescription = "Describes if token owner can make transfers themselves"

data OperatorTransferMode
  = OperatorTransferPermitted ("operator_transfer_permitted" :! ())
  | OperatorTransferDenied ("operator_transfer_denied" :! ())
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, HasTypeAnn)

instance Arbitrary OperatorTransferMode where
  arbitrary =
    elements
      [ OperatorTransferPermitted (#operator_transfer_permitted .! ())
      , OperatorTransferDenied (#operator_transfer_denied .! ())
      ]

instance TypeHasDoc OperatorTransferMode where
  typeDocMdDescription = "Describes if operator can make transfer on behalf of token owner"

data OwnerTransferMode
  = OwnerNoOp ("owner_no_op" :! ())
  | OptionalOwnerHook ("optional_owner_hook" :! ())
  | RequiredOwnerHook ("required_owner_hook" :! ())
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasTypeAnn)

instance Arbitrary OwnerTransferMode where
  arbitrary =
    elements
      [ OwnerNoOp (#owner_no_op .! ())
      , OptionalOwnerHook (#optional_owner_hook .! ())
      , RequiredOwnerHook (#required_owner_hook .! ())
      ]

instance TypeHasDoc OwnerTransferMode where
  typeDocMdDescription = "Describes if owener hooks are required in sender/receiver addresses"

type CustomPermissionPolicy
  = ("tag" :! MText, "config_api" :! Maybe Address)

-- A helper type and typefamily to enable switching of fields
-- of the permission descriptor to a maybe value.
data PdTag = PdMaybe | PdFull

type family PdFld (f :: PdTag) t where
  PdFld 'PdFull t = t
  PdFld 'PdMaybe t = Maybe t

-- | The permission descriptor of the FA2 contract.
-- The dummy names for every second tuple item is a temporary
-- workaround for the strict requirement of having to name all the
-- tuple elements.
type PermissionsDescriptorPoly t =
  ( "self" :! PdFld t SelfTransferMode
  , "pdr" :! ( "operator" :! PdFld t OperatorTransferMode
  , "pdr2" :! ( "receiver" :! PdFld t OwnerTransferMode
  , "pdr3" :!
      ("sender" :! PdFld t OwnerTransferMode, "custom" :! PdFld t (Maybe CustomPermissionPolicy)))))

type PermissionsDescriptor = PermissionsDescriptorPoly 'PdFull
type PermissionsDescriptorMaybe = PermissionsDescriptorPoly 'PdMaybe

type PermissionsDescriptorParam = ContractRef PermissionsDescriptor

type OperatorParam =
  ("owner" :! Address, "operator" :! Address)

data UpdateOperator
  = Add_operator OperatorParam
  | Remove_operator OperatorParam
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasTypeAnn)

type UpdateOperatorsParam = [UpdateOperator]

-- Is operator query
type IsOperatorResponse = ("operator" :! OperatorParam, "is_operator" :! Bool)

instance Buildable OperatorParam where
  build = genericF

instance Buildable IsOperatorResponse where
  build = genericF

type IsOperatorParam = View ("operator" :! OperatorParam) IsOperatorResponse

-- | Parameter of an FA2 contract
-- Note that right now this is generated with type annotations where
-- FA2 require field annotations, because current infra is unable to
-- generate such kind of annotations. Here is an issue that aims to
-- solve it.
-- TODO : https://gitlab.com/morley-framework/morley/-/issues/159
data Parameter
  = Transfer TransferParams
  | Balance_of BalanceRequestParams
  | Total_supply TotalSupplyRequestParams
  | Token_metadata TokenMetaDataParam
  | Permissions_descriptor PermissionsDescriptorParam
  | Update_operators UpdateOperatorsParam
  | Is_operator IsOperatorParam
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

type ParameterC param =
  ParameterContainsEntryPoints param
    [ "Transfer" :> TransferParams
    , "Balance_of" :> BalanceRequestParams
    , "Total_supply" :> TotalSupplyRequestParams
    , "Token_metadata" :> TokenMetaDataParam
    , "Permissions_descriptor" :> PermissionsDescriptorParam
    , "Update_operators" :> UpdateOperatorsParam
    , "Is_operator" :> IsOperatorParam
    ]

-- | Owner hook interface
--
type TransferDescriptor =
  ( "from" :! Maybe Address
  , ("to" :! Maybe Address
    , ("token_id" :! TokenId, "amount" :! Natural)))

type TransferDescriptorParam =
  ("fa2" :! Address, ("batch" :! [TransferDescriptor], "operator" :! Address))

data FA2OwnerHook
  = Tokens_sent TransferDescriptorParam
  | Tokens_received TransferDescriptorParam
  deriving stock (Generic, Eq, Show)
  deriving anyclass (IsoValue, HasTypeAnn)

instance Buildable TransferDescriptor where
  build = genericF

instance Buildable TransferItem where
  build = genericF

instance Buildable FA2OwnerHook where
  build = genericF

instance ParameterHasEntryPoints FA2OwnerHook where
  type ParameterEntryPointsDerivation FA2OwnerHook = EpdPlain
