-- SPDX-FileCopyrightText: 2020 Globacap
-- SPDX-License-Identifier: MPL-2.0
--
-- | This is a simple implementation of a safelist contract that is only
-- used in tests.

{-# OPTIONS_GHC -Wno-orphans #-}
module Indigo.Contracts.Safelist
  ( Parameter
  , Storage (..)
  , mkStorage
  , safelistContract
  ) where

import qualified Data.Set as Set

import Indigo
import Lorentz.Run (Contract)

data Storage = Storage
  { sTransfers :: Set (Address, Address)
  , sReceivers :: Set Address
  }
  deriving stock Generic
  deriving anyclass IsoValue

mkStorage :: Set (Address, Address) -> [Address] -> Storage
mkStorage transfers receivers = Storage
  { sTransfers = transfers
  , sReceivers = Set.fromList receivers
  }

data Parameter
  = AssertTransfers [("from" :! Address, "to" :! [Address])]
  | AssertReceiver Address
  | AssertReceivers [Address]
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntryPoints Parameter where
  type ParameterEntryPointsDerivation Parameter = EpdPlain

type instance ErrorArg "assertionFailure" = ()

instance CustomErrorHasDoc "assertionFailure" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Assertion failed."

safelistContract :: Contract Parameter Storage
safelistContract = defaultContract $ compileIndigoContract safelistIndigo

safelistIndigo
  :: (HasStorage Storage)
  => Var Parameter -> IndigoProcedure
safelistIndigo param = contractName "Dummy safelist" $ do
  entryCase (Proxy @PlainEntryPointsKind) param
    ( #cAssertTransfers //-> \transfers -> forEach transfers $ \transfer -> do
        let fromAddr = transfer !. #from
        res <- new False
        forEach (transfer !. #to) $ \toAddr ->
          forEach (storage !. #sTransfers) $ \it ->
            Indigo.when ((Indigo.fst it ==. fromAddr) &&. (Indigo.snd it ==. toAddr)) $ setVar res True
        assertCustom_ #assertionFailure res
    , #cAssertReceiver //-> \receiver -> do
        assertCustom_ #assertionFailure $ (storage !. #sReceivers) #? receiver
    , #cAssertReceivers //-> \receivers -> do
        forEach receivers $ \receiver -> do
          assertCustom_ #assertionFailure $ (storage !. #sReceivers) #? receiver
    )

storage :: HasStorage Storage => Var Storage
storage = storageVar
