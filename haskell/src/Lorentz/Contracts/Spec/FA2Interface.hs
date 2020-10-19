-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

{-# OPTIONS_GHC -Wno-orphans #-}
-- | FA2 interface specification.
-- https://gitlab.com/tzip/tzip/-/blob/131b46dd89675bf030489ded9b0b3f5834b70eb6/proposals/tzip-12/tzip-12.md

module Lorentz.Contracts.Spec.FA2Interface
  ( BalanceRequestItem(..)
  , BalanceRequestParams
  , BalanceResponseItem(..)
  , CustomPermissionPolicy(..)
  , FA2OwnerHook (..)
  , IsOperatorParam
  , IsOperatorResponse(..)
  , OperatorParam(..)
  , OperatorTransferPolicy (..)
  , OwnerTransferMode (..)
  , Parameter (..)
  , FA2ParameterC
  , TokenId
  , TokenMetadata(..)
  , TransferDestination(..)
  , TransferDestinationDescriptor(..)
  , TransferDescriptor(..)
  , TransferDescriptorParam(..)
  , TransferParams
  , TransferParam(..)
  , UpdateOperator (..)
  , UpdateOperatorsParam
  ) where

import Fmt (Buildable(..), genericF)

import Lorentz

-- Transfer
-- --------
-- 1. Parameter is list of transfer items.
-- 2. These items must be executed in order, and atomically.
--    If one of them cannot complete, then whole batch should be aborted.
-- 3. Tranfer should update the balances exactly as specified, and should not adjust it anyway.
-- 4. Transfer operation must apply permission policy logic.
type TokenId = Natural

data TransferDestination = TransferDestination
 { tdTo :: Address
 , tdTokenId :: TokenId
 , tdAmount :: Natural
 }
 deriving stock (Generic, Show)
 deriving anyclass (IsoValue, HasAnnotation)

data TransferParam = TransferParam
 { tpFrom :: Address
 , tpTxs :: [TransferDestination]
 }
 deriving stock (Show, Generic)
 deriving anyclass (IsoValue, HasAnnotation)

instance Buildable TransferParam where
  build = genericF

type TransferParams = [TransferParam]

-- BalanceOf
-- ---------
-- Queries balance of one or more addresses. Callback contract should accept a list of BalanceResponse
-- Duplicates in the request should not be de-duplicated or reordered in the response.
data BalanceRequestItem = BalanceRequestItem
  { briOwner :: Address
  , briTokenId :: TokenId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable BalanceRequestItem where
  build = genericF

data BalanceResponseItem = BalanceResponseItem
  { briRequest :: BalanceRequestItem
  , briBalance :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

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

instance TypeHasDoc OperatorTransferPolicy where
  typeDocMdDescription = "Describes if operator can make transfer on behalf of token owner"

data OwnerTransferMode
  = OwnerNoHook ("owner_no_op" :! ())
  | OptionalOwnerHook ("optional_owner_hook" :! ())
  | RequiredOwnerHook ("required_owner_hook" :! ())
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc OwnerTransferMode where
  typeDocMdDescription = "Describes if owener hooks are required in sender/receiver addresses"

data CustomPermissionPolicy = CustomPermissionPolicy
  { cppTag :: MText
  , cppConfigApi :: Maybe Address
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (IsoValue, HasAnnotation)

data OperatorParam = OperatorParam
  { opOwner :: Address
  , opOperator :: Address
  , opTokenId :: Natural
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (IsoValue, HasAnnotation)

data UpdateOperator
  = Add_operator OperatorParam
  | Remove_operator OperatorParam
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable UpdateOperator where
  build = genericF

type UpdateOperatorsParam = [UpdateOperator]

-- Is operator query
data IsOperatorResponse = IsOperatorResponse
  { iorOperator :: OperatorParam
  , iorIsOperator :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable OperatorParam where
  build = genericF

instance Buildable IsOperatorResponse where
  build = genericF

type IsOperatorParam = View ("operator" :! OperatorParam) IsOperatorResponse

-- | Parameter of an FA2 contract
data Parameter
  = Balance_of BalanceRequestParams
  | Token_metadata_registry TokenMetadataRegistryParam
  | Transfer TransferParams
  | Update_operators UpdateOperatorsParam
  deriving stock (Show)

$(customGeneric "Parameter" $ withDepths
    [ cstr @2 [fld @0]
    , cstr @2 [fld @0]
    , cstr @2 [fld @0]
    , cstr @2 [fld @0]
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
  ]

type FA2ParameterC param =
  ParameterContainsEntrypoints param FA2ParameterMichelsonEntrypoints

-- | Owner hook interface
--

data TransferDestinationDescriptor = TransferDestinationDescriptor
  { tddTo :: Maybe Address
  , tddTokenId :: TokenId
  , tddAmount :: Natural
  }
 deriving stock (Generic, Show, Eq)
 deriving anyclass (IsoValue, HasAnnotation)

data TransferDescriptor = TransferDescriptor
  { tdFrom :: Maybe Address
  , tdTxs :: [TransferDestinationDescriptor]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

data TransferDescriptorParam = TransferDescriptorParam
  { tdpFa2 :: Address
  , tdpBatch :: [TransferDescriptor]
  , tdpOperator :: Address
  }
 deriving stock (Generic, Show, Eq)
 deriving anyclass (IsoValue, HasAnnotation)

data FA2OwnerHook
  = Tokens_sent TransferDescriptorParam
  | Tokens_received TransferDescriptorParam
  deriving stock (Generic, Eq, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance Buildable TransferDescriptorParam where
  build = genericF

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
