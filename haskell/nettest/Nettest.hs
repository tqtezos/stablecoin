-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Nettest
  ( scNettestScenario
  ) where

import qualified Data.Set as Set

import Lorentz hiding (comment, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.Common
import Michelson.Typed (untypeValue)
import qualified Michelson.Untyped as U
import Morley.Nettest
import Util.Named

import Lorentz.Contracts.Stablecoin

scNettestScenario
  :: forall m capsM.
     (Monad m, capsM ~ NettestT m)
  => (OriginationParams -> Storage)
  -> U.Contract
  -> (Set (Address, Address) -> Set Address -> capsM Address)
  -> NettestImpl m
  -> m ()
scNettestScenario constructInitialStorage stablecoinContract originateTransferlist = uncapsNettest $ do
  comment "Resolving contract managers"

  superuser <- resolveNettestAddress

  nettestOwner <- newAddress "nettestOwner"
  nettestPauser <- newAddress "nettestPauser"
  nettestMasterMinter <- newAddress "nettestMasterMinter"

  comment "Resolving owners and operators"

  owner1 <- newAddress "Steve"
  owner2 <- newAddress "Megan"
  owner3 <- newAddress "Karen"

  operator <- newAddress "Some operator"
  otherOperator <- newAddress "Other operator"

  let
    initialStorage =
        addAccount (superuser , ([operator], 1110000))
      . addAccount (owner1, ([operator], 100))
      . addAccount (owner2, ([operator], 0))
      . addAccount (owner3, ([operator], 0))
      . addAccount (nettestPauser, ([operator], 0))
      . addMinter (superuser, 200)
      $ defaultOriginationParams
          { opOwner = nettestOwner
          , opPauser = nettestPauser
          , opMasterMinter = nettestMasterMinter
          }

  comment "Originating stablecoin contract"

  scAddress <- do
    let str = constructInitialStorage initialStorage
    originateUntypedSimple "nettest.Stablecoin" (untypeValue $ toVal str) stablecoinContract

  comment "Originating transferlist contract"

  let
    transfers =
      Set.fromList
        [ (owner1, owner2)
        , (owner2, owner3)
        , (owner3, owner1)
        ]

    receivers = Set.fromList [owner1, owner2]

  sfAddress <- originateTransferlist transfers receivers

  let
    expectFailed = flip expectFailure NettestFailedWith

    sc :: AddressOrAlias
    sc = AddressResolved scAddress

    tp :: Address -> Address -> Natural -> FA2.TransferParams
    tp from to value = constructSingleTransfer
      (#from_ .! from)
      (#to_ .! to)
      (#amount .! value)

    callTransferWithOperator op from to value =
      callFrom' (AddressResolved op) sc (ep "transfer") (tp from to value)

    callTransfer = join callTransferWithOperator

    callTransfers
      :: [("from_" :! Address, [("to_" :! Address, "amount" :! Natural)])]
      -> capsM ()
    callTransfers = mapM_ $ \(from@(arg #from_ -> from_), destinations) ->
      callFrom'
        (AddressResolved from_)
        sc
        (ep "transfer")
        (constructTransfersFromSender from destinations)

    configureMinter :: Address -> Address -> Maybe Natural -> Natural -> capsM ()
    configureMinter from for expectedAllowance newAllowance =
      callFrom'
        (AddressResolved from)
        sc
        (ep "configure_minter")
        ( #minter .! for
        , ( #current_minting_allowance .! expectedAllowance
          , #new_minting_allowance .! newAllowance))

    removeMinter :: Address -> Address -> capsM ()
    removeMinter from whom =
      callFrom'
        (AddressResolved from)
        sc
        (ep "remove_minter")
        whom

    addOperatorNettest :: Address -> Address -> capsM ()
    addOperatorNettest from op =
      callFrom'
        (AddressResolved from)
        sc
        (ep "update_operators")
        [FA2.Add_operator (#owner .! from, #operator .! op)]

    removeOperator :: Address -> Address -> capsM ()
    removeOperator from op =
      callFrom'
        (AddressResolved from)
        sc
        (ep "update_operators")
        [FA2.Remove_operator (#owner .! from, #operator .! op)]

    mint :: Address -> Natural -> capsM ()
    mint to_ value =
      callFrom'
        (AddressResolved to_)
        sc
        (ep "mint")
        [(#to_ .! to_, #amount .! value)]

    burn :: Address -> Natural -> capsM ()
    burn from amount_ =
      callFrom'
        (AddressResolved from)
        sc
        (ep "burn")
        [amount_]

    pause :: Address -> capsM ()
    pause from =
      callFrom'
        (AddressResolved from)
        sc
        (ep "pause")
        ()

    unpause :: Address -> capsM ()
    unpause from =
      callFrom'
        (AddressResolved from)
        sc
        (ep "unpause")
        ()

    transferOwnership :: Address -> Address -> capsM ()
    transferOwnership from to =
      callFrom'
        (AddressResolved from)
        sc
        (ep "transfer_ownership")
        to

    acceptOwnership :: Address -> capsM ()
    acceptOwnership from =
      callFrom'
        (AddressResolved from)
        sc
        (ep "accept_ownership")
        ()

    changeMasterMinter :: Address -> Address -> capsM ()
    changeMasterMinter from to =
      callFrom'
        (AddressResolved from)
        sc
        (ep "change_master_minter")
        to

    changePauser :: Address -> Address -> capsM ()
    changePauser from to =
      callFrom'
        (AddressResolved from)
        sc
        (ep "change_pauser")
        to

    setTransferlist :: Address -> Address -> capsM ()
    setTransferlist from transferlistAddress =
      callFrom'
        (AddressResolved from)
        sc
        (ep "set_transferlist")
        (Just transferlistAddress)

    unsetTransferlist :: Address -> capsM ()
    unsetTransferlist from =
      callFrom'
        (AddressResolved from)
        sc
        (ep "set_transferlist")
        (Nothing :: Maybe Address)

  let
    transferScenario = do
      comment "Transfers from superuser"
      callTransfer superuser superuser 111
      callTransfer superuser owner1 40

      comment "Transfers between owners"
      callTransfers
        [ (#from_ .! owner1, [(#to_ .! owner2, #amount .! 10)])
        , (#from_ .! owner2, [(#to_ .! owner3, #amount .! 10)])
        , (#from_ .! owner3, [(#to_ .! owner1, #amount .! 10)])
        ]

      comment "Pausing contract for transfers"
      callTransfer superuser nettestPauser 600000 -- storage fee
      expectFailed $ pause owner1 -- Not enough permissions
      pause nettestPauser
      expectFailed $ pause nettestPauser -- Cannot be called multiple times

      expectFailed $ callTransfer owner3 owner2 10

      comment "Unpausing contract for transfers"
      expectFailed $ unpause owner3 -- Not enough permissions
      unpause nettestPauser
      expectFailed $ unpause nettestPauser -- Cannot be called multiple times

      callTransfer owner1 owner2 10
      callTransfer owner2 owner1 10

      comment $ "Updating transfer operators for owners"
      callTransfer superuser owner1 10
      addOperatorNettest owner1 otherOperator
      callTransferWithOperator otherOperator owner1 owner2 10
      expectFailed $ callTransferWithOperator otherOperator owner2 owner1 10
      removeOperator owner1 otherOperator
      expectFailed $ callTransferWithOperator otherOperator owner1 owner2 10

      callTransfer owner2 owner1 10

    mintScenario = do
      comment $ "Mint to superuser"
      mint superuser 200
      callTransfer superuser nettestMasterMinter 1200 -- Needed to pay transfer fee

      comment "Configuring minter for owner1 and owner2"
      configureMinter nettestMasterMinter owner1 Nothing 100
      expectFailed $ configureMinter nettestMasterMinter owner1 (Just 20) 10 -- Mismatched expected allowance
      configureMinter nettestMasterMinter owner1 (Just 100) 50
      expectFailed $ configureMinter nettestMasterMinter owner2 (Just 20) 10 -- Not a minter
      configureMinter nettestMasterMinter owner2 Nothing 10

      comment $ "Minting for owner1 and owner2"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      mint owner1 20
      expectFailed $ mint owner1 200 -- Allowance exceeded

      comment "Transfer between owner1 and owner2"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      callTransfer owner1 owner2 20
      mint owner2 10

      callTransfer owner2 owner1 30

      comment "Remove owner1 and owner2 from minters"
      removeMinter nettestMasterMinter owner2
      expectFailed $ removeMinter nettestMasterMinter owner2 -- Already removed
      removeMinter nettestMasterMinter owner1

    burnScenario = do
      comment $ "Burning for owner1"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      callTransfer superuser nettestMasterMinter 1000
      configureMinter nettestMasterMinter owner1 Nothing 100
      mint owner1 100
      burn owner1 20
      expectFailed $ burn owner1 2000 -- Not enough tokens to burn
      comment "Transfer rest of the tokens after burn from owner1"
      callTransfer owner1 owner2 80
      removeMinter nettestMasterMinter owner1

    permissionReassigmentScenario = do
      comment "Transferring contract ownership"
      transferOwnership nettestOwner owner1
      transferOwnership nettestOwner owner2
      expectFailed $ acceptOwnership owner1 -- Pending owner is changed
      acceptOwnership owner2
      transferOwnership owner2 nettestOwner
      acceptOwnership nettestOwner

      comment "Changing master minter"
      changeMasterMinter nettestOwner owner1
      configureMinter owner1 owner2 Nothing 10
      mint owner2 10
      changeMasterMinter nettestOwner nettestMasterMinter
      removeMinter nettestMasterMinter owner2

      comment "Changing contract pauser"
      changePauser nettestOwner owner1
      pause owner1
      expectFailed $ pause nettestPauser -- nettestPauser is not pauser
      unpause owner1
      changePauser nettestOwner nettestPauser


    transferlistScenario = do
      comment $ "Transferlist interaction"

      setTransferlist nettestOwner sfAddress

      callTransfer owner2 owner3 10
      expectFailed $ callTransfer owner3 owner2 20 -- Transfer is not in transferlist

      configureMinter nettestMasterMinter owner1 Nothing 100
      configureMinter nettestMasterMinter owner2 Nothing 100

      mint owner2 20
      expectFailed $ mint owner3 20 -- Minter is not set in transferlist

      comment "Unsetting transferlist"
      unsetTransferlist nettestOwner

  transferScenario
  mintScenario
  burnScenario
  permissionReassigmentScenario
  transferlistScenario

-- This is a temporary solution.
-- Changes made in cleveland's `callFrom` have caused some issues when calling
-- entrypoints other than the default.
-- So we've temporarily copied the old `callFrom` implementation here and
-- renamed it to `callFrom'`.
-- See discussion here: https://github.com/tqtezos/stablecoin/pull/79#discussion_r464983707
--
-- TODO: delete this after these comments have been addressed:
-- https://gitlab.com/morley-framework/morley/-/merge_requests/499#note_388448660
callFrom'
  :: (MonadNettest caps base m, NiceParameter v)
  => AddressOrAlias
  -> AddressOrAlias
  -> EpName
  -> v
  -> m ()
callFrom' from to epName param =
  transfer $
    TransferData
      { tdFrom = from
      , tdTo = to
      , tdAmount = zeroMutez
      , tdEntrypoint = epName
      , tdParameter = param
      }
