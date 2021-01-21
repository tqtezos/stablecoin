-- SPDX-FileCopyrightText: 2020 Globacap
-- SPDX-License-Identifier: MPL-2.0
--
-- | This is a simple implementation of a transferlist contract that is only
-- used in tests.

{-# OPTIONS_GHC -Wno-orphans #-}
module Indigo.Contracts.Transferlist.Internal
  ( Parameter
  , Storage (..)
  , mkStorage
  , transferlistContract
  ) where

import qualified Data.Set as Set

import Indigo
import Lorentz.Run (Contract)
import Prelude ()

data Storage = Storage
  { sTransfers :: Set (Address, Address)
  , sReceivers :: Set Address
  }
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

mkStorage :: Set (Address, Address) -> [Address] -> Storage
mkStorage transfers receivers = Storage
  { sTransfers = transfers
  , sReceivers = Set.fromList receivers
  }

data Parameter
  = AssertTransfers [("from" :! Address, "tos" :! [Address])]
  | AssertReceivers [Address]
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

type instance ErrorArg "assertionFailure" = ()

instance CustomErrorHasDoc "assertionFailure" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Assertion failed."

transferlistContract :: Contract Parameter Storage
transferlistContract = defaultContract $ compileIndigoContract transferlistIndigo

transferlistIndigo
  :: (HasStorage Storage)
  => Var Parameter -> IndigoProcedure
transferlistIndigo param = docGroup "Dummy transferlist" $ do
  entryCase (Proxy @PlainEntrypointsKind) param
    ( #cAssertTransfers #= \transfers -> forEach transfers $ \transfer -> do
        let fromAddr = transfer #! #from
        res <- new False
        forEach (transfer #! #tos) $ \toAddr ->
          forEach (storage #! #sTransfers) $ \it ->
            Indigo.when ((Indigo.fst it == fromAddr) && (Indigo.snd it == toAddr)) $ setVar res True
        assertCustom_ #assertionFailure res
    , #cAssertReceivers #= \receivers -> do
        forEach receivers $ \receiver -> do
          assertCustom_ #assertionFailure $ (storage #! #sReceivers) ?: receiver
    )

storage :: HasStorage Storage => Var Storage
storage = storageVar
