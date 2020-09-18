-- SPDX-FileCopyrightText: 2020 TQ Tezos
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
  , OperatorTransferPolicy (..)
  , OwnerTransferMode (..)
  , Parameter (..)
  , FA2ParameterC
  , TokenId
  , TokenMetadata(..)
  , TransferDestination
  , TransferDescriptor
  , TransferDescriptorParam
  , TransferParams
  , TransferParam
  , UpdateOperator (..)
  , UpdateOperatorsParam
  ) where

import Fmt (Buildable(..), genericF)
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

type TransferDestination =
  ("to_" :! Address, ("token_id" :! TokenId, "amount" :! Natural))

type TransferParam =
  ("from_" :! Address, "txs" :! [TransferDestination])

type TransferParams = [TransferParam]

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

-- Token MetaData query
data TokenMetadata = TokenMetadata
 { tmTokenId :: TokenId
 , tmSymbol :: MText
 , tmName :: MText
 , tmDecimals :: Natural
 , tmExtras :: Map MText MText
 }
 deriving stock Show

instance Buildable TokenMetadata where
  build = genericF

deriving anyclass instance IsoValue TokenMetadata
deriving anyclass instance HasAnnotation TokenMetadata
$(customGeneric "TokenMetadata" $ withDepths
    [ cstr @0
      [ fld @1
      , fld @2
      , fld @3
      , fld @4
      , fld @4
      ]
    ]
  )

type TokenMetadataRegistryParam = ContractRef Address

data OperatorTransferPolicy
  = OwnerTransfer ("owner_transfer" :! ())
  | NoTransfer ("no_transfer" :! ())
  | OwnerOrOperatorTransfer ("owner_or_operator_transfer" :! ())
  deriving stock (Eq, Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance Arbitrary OperatorTransferPolicy where
  arbitrary =
    elements
      [ OwnerTransfer (#owner_transfer .! ())
      , NoTransfer (#no_transfer .! ())
      , OwnerOrOperatorTransfer (#owner_or_operator_transfer .! ())
      ]

instance TypeHasDoc OperatorTransferPolicy where
  typeDocMdDescription = "Describes if operator can make transfer on behalf of token owner"

data OwnerTransferMode
  = OwnerNoHook ("owner_no_op" :! ())
  | OptionalOwnerHook ("optional_owner_hook" :! ())
  | RequiredOwnerHook ("required_owner_hook" :! ())
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance Arbitrary OwnerTransferMode where
  arbitrary =
    elements
      [ OwnerNoHook (#owner_no_op .! ())
      , OptionalOwnerHook (#optional_owner_hook .! ())
      , RequiredOwnerHook (#required_owner_hook .! ())
      ]

instance TypeHasDoc OwnerTransferMode where
  typeDocMdDescription = "Describes if owener hooks are required in sender/receiver addresses"

type CustomPermissionPolicy
  = ("tag" :! MText, "config_api" :! Maybe Address)

type OperatorParam =
  ("owner" :! Address, "operator" :! Address)

data UpdateOperator
  = Add_operator OperatorParam
  | Remove_operator OperatorParam
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable UpdateOperator where
  build = genericF

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
  = Balance_of BalanceRequestParams
  | Is_operator IsOperatorParam
  | Token_metadata_registry TokenMetadataRegistryParam
  | Transfer TransferParams
  | Update_operators UpdateOperatorsParam
  deriving stock (Show)

$(customGeneric "Parameter" $ withDepths
    [ cstr @3 [fld @0]
    , cstr @3 [fld @0]
    , cstr @3 [fld @0]
    , cstr @3 [fld @0]
    , cstr @1 [fld @0]
    ]
  )

deriving anyclass instance IsoValue Parameter

-- That missing instance for lorentz lambdas
instance Buildable (inp :-> out) where
  build = show

instance Buildable Parameter where
  build = genericF

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

type FA2ParameterMichelsonEntrypoints =
  [ "Transfer" :> TransferParams
  , "Balance_of" :> BalanceRequestParams
  , "Token_metadata_registry" :> TokenMetadataRegistryParam
  , "Update_operators" :> UpdateOperatorsParam
  , "Is_operator" :> IsOperatorParam
  ]

type FA2ParameterC param =
  ParameterContainsEntrypoints param FA2ParameterMichelsonEntrypoints

-- | Owner hook interface
--

type TransferDestinationDescriptor =
  ("to_" :! Maybe Address
  , ("token_id" :! TokenId, "amount" :! Natural))

type TransferDescriptor =
  ( "from_" :! Maybe Address
  , ("txs" :! [TransferDestinationDescriptor]))

type TransferDescriptorParam =
  ("fa2" :! Address, ("batch" :! [TransferDescriptor], "operator" :! Address))

data FA2OwnerHook
  = Tokens_sent TransferDescriptorParam
  | Tokens_received TransferDescriptorParam
  deriving stock (Generic, Eq, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable TransferDestinationDescriptor where
  build = genericF

instance Buildable TransferDescriptor where
  build = genericF

instance Buildable TransferDestination where
  build = genericF

instance Buildable FA2OwnerHook where
  build = genericF

instance ParameterHasEntrypoints FA2OwnerHook where
  type ParameterEntrypointsDerivation FA2OwnerHook = EpdPlain
