-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Nettest
  ( scNettestScenario
  , TransferlistType(..)
  ) where

import qualified Data.Set as Set
import Fmt (build)

import Lorentz hiding (comment, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.Common
import Michelson.Typed (untypeValue)
import qualified Michelson.Typed as T
import Morley.Nettest
import Util.Named

import Lorentz.Contracts.Stablecoin

-- | Indicates whether we're using the external safelist (the one parsed from @transferlist.tz@)
-- or the internal safelist ('Indigo.Contracts.Transferlist.Internal')
-- to run these tests.
--
-- Depending on this, the tests might expect different @FAILWITH@ messages.
data TransferlistType = External | Internal

scNettestScenario
  :: forall m capsM.
     (Monad m, capsM ~ NettestT m)
  => (Set (Address, Address) -> Set Address -> capsM Address)
  -> TransferlistType
  -> NettestImpl m
  -> m ()
scNettestScenario originateTransferlist transferlistType = uncapsNettest $ do
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

  comment "Originating metadata contract"
  metadata <- either (failure . build) pure $ metadataJSON Nothing Nothing
  cmrAddress <- nettestOriginateContractMetadataContract metadata
  let
    originationParams =
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
          , opMetadataUri = RemoteContract cmrAddress
          }

  comment "Originating stablecoin contract"
  scAddress <-
    originateUntypedSimple
      "nettest.Stablecoin"
      (untypeValue (toVal (mkInitialStorage originationParams)))
      (T.convertContract stablecoinContract)

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
    expectFailed :: NiceUnpackedValue t => t -> capsM a -> capsM ()
    expectFailed val = flip expectFailure (NettestFailedWith scAddress val)

    expectTransferlistFailed :: NiceUnpackedValue t => t -> capsM a -> capsM ()
    expectTransferlistFailed val = flip expectFailure (NettestFailedWith sfAddress val)

    sc :: TAddress Parameter
    sc = TAddress scAddress

    tp :: Address -> Address -> Natural -> FA2.TransferParams
    tp from to value = constructSingleTransfer
      (#from_ .! from)
      (#to_ .! to)
      (#amount .! value)

    callTransferWithOperator op from to value =
      callFrom (AddressResolved op) sc (Call @"Transfer") (tp from to value)

    callTransfer = join callTransferWithOperator

    callTransfers
      :: [("from_" :! Address, [("to_" :! Address, "amount" :! Natural)])]
      -> capsM ()
    callTransfers = mapM_ $ \(from@(arg #from_ -> from_), destinations) ->
      callFrom
        (AddressResolved from_)
        sc
        (Call @"Transfer")
        (constructTransfersFromSender from destinations)

    configureMinter :: Address -> Address -> Maybe Natural -> Natural -> capsM ()
    configureMinter = configureMinter' sc

    configureMinter' :: TAddress Parameter -> Address -> Address -> Maybe Natural -> Natural -> capsM ()
    configureMinter' sc' from for expectedAllowance newAllowance =
      callFrom
        (AddressResolved from)
        sc'
        (Call @"Configure_minter")
        (ConfigureMinterParam for expectedAllowance newAllowance)

    removeMinter :: Address -> Address -> capsM ()
    removeMinter from whom =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Remove_minter")
        whom

    addOperatorNettest :: Address -> Address -> capsM ()
    addOperatorNettest from op =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Update_operators")
        [ FA2.AddOperator
            FA2.OperatorParam { opOwner = from, opOperator = op, opTokenId = FA2.theTokenId }
        ]

    removeOperator :: Address -> Address -> capsM ()
    removeOperator from op =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Update_operators")
        [ FA2.RemoveOperator
            FA2.OperatorParam { opOwner = from, opOperator = op, opTokenId = FA2.theTokenId }
        ]

    mint :: Address -> Natural -> capsM ()
    mint to_ value =
      callFrom
        (AddressResolved to_)
        sc
        (Call @"Mint")
        [MintParam to_ value]

    burn :: Address -> Natural -> capsM ()
    burn from amount_ =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Burn")
        [amount_]

    pause :: Address -> capsM ()
    pause from =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Pause")
        ()

    unpause :: Address -> capsM ()
    unpause from =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Unpause")
        ()

    transferOwnership :: Address -> Address -> capsM ()
    transferOwnership from to =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Transfer_ownership")
        to

    acceptOwnership :: Address -> capsM ()
    acceptOwnership from =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Accept_ownership")
        ()

    changeMasterMinter :: Address -> Address -> capsM ()
    changeMasterMinter from to =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Change_master_minter")
        to

    changePauser :: Address -> Address -> capsM ()
    changePauser from to =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Change_pauser")
        to

    setTransferlist :: Address -> Address -> capsM ()
    setTransferlist from transferlistAddress =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Set_transferlist")
        (Just transferlistAddress)

    unsetTransferlist :: Address -> capsM ()
    unsetTransferlist from =
      callFrom
        (AddressResolved from)
        sc
        (Call @"Set_transferlist")
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
      expectFailed [mt|NOT_PAUSER|] $ pause owner1 -- Not enough permissions
      pause nettestPauser
      expectFailed [mt|CONTRACT_PAUSED|] $ pause nettestPauser -- Cannot be called multiple times

      expectFailed [mt|CONTRACT_PAUSED|] $ callTransfer owner3 owner2 10

      comment "Unpausing contract for transfers"
      expectFailed [mt|NOT_PAUSER|] $ unpause owner3 -- Not enough permissions
      unpause nettestPauser
      expectFailed [mt|CONTRACT_NOT_PAUSED|] $ unpause nettestPauser -- Cannot be called multiple times

      callTransfer owner1 owner2 10
      callTransfer owner2 owner1 10

      comment $ "Updating transfer operators for owners"
      callTransfer superuser owner1 10
      addOperatorNettest owner1 otherOperator
      callTransferWithOperator otherOperator owner1 owner2 10
      expectFailed [mt|FA2_NOT_OPERATOR|] $ callTransferWithOperator otherOperator owner2 owner1 10
      removeOperator owner1 otherOperator
      expectFailed [mt|FA2_NOT_OPERATOR|] $ callTransferWithOperator otherOperator owner1 owner2 10

      callTransfer owner2 owner1 10

    mintScenario = do
      comment $ "Mint to superuser"
      mint superuser 200
      callTransfer superuser nettestMasterMinter 1200 -- Needed to pay transfer fee

      comment "Configuring minter for owner1 and owner2"
      configureMinter nettestMasterMinter owner1 Nothing 100
      expectFailed [mt|ALLOWANCE_MISMATCH|] $ configureMinter nettestMasterMinter owner1 (Just 20) 10 -- Mismatched expected allowance
      configureMinter nettestMasterMinter owner1 (Just 100) 50
      expectFailed [mt|ADDR_NOT_MINTER|] $ configureMinter nettestMasterMinter owner2 (Just 20) 10 -- Not a minter
      configureMinter nettestMasterMinter owner2 Nothing 10

      comment $ "Minting for owner1 and owner2"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      mint owner1 20
      expectFailed [mt|ALLOWANCE_EXCEEDED|] $ mint owner1 200 -- Allowance exceeded

      comment "Transfer between owner1 and owner2"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      callTransfer owner1 owner2 20
      mint owner2 10

      callTransfer owner2 owner1 30

      comment "Remove owner1 and owner2 from minters"
      removeMinter nettestMasterMinter owner2
      expectFailed [mt|ADDR_NOT_MINTER|] $ removeMinter nettestMasterMinter owner2 -- Already removed
      removeMinter nettestMasterMinter owner1

    burnScenario = do
      comment $ "Burning for owner1"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      callTransfer superuser nettestMasterMinter 1000
      configureMinter nettestMasterMinter owner1 Nothing 100
      mint owner1 100
      burn owner1 20
      expectFailed [mt|FA2_INSUFFICIENT_BALANCE|] $ burn owner1 2000 -- Not enough tokens to burn
      comment "Transfer rest of the tokens after burn from owner1"
      callTransfer owner1 owner2 80
      removeMinter nettestMasterMinter owner1

    permissionReassigmentScenario = do
      comment "Transferring contract ownership"
      transferOwnership nettestOwner owner1
      transferOwnership nettestOwner owner2
      expectFailed [mt|NOT_PENDING_OWNER|] $ acceptOwnership owner1 -- Pending owner is changed
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
      expectFailed [mt|NOT_PAUSER|] $ unpause nettestPauser -- nettestPauser is not pauser
      unpause owner1
      changePauser nettestOwner nettestPauser


    transferlistScenario = do
      comment $ "Transferlist interaction"

      setTransferlist nettestOwner sfAddress

      callTransfer owner2 owner3 10

      -- transfer is not in transferlist
      let action = callTransfer owner3 owner2 9
      case transferlistType of
        Internal -> expectTransferlistFailed ([mt|AssertionFailure|], ()) action
        External -> expectTransferlistFailed [mt|outbound not transferlisted|] action

      configureMinter nettestMasterMinter owner1 Nothing 100
      configureMinter nettestMasterMinter owner2 Nothing 100

      mint owner2 20
      expectFailed [mt|NOT_MINTER|] $ mint owner3 20 -- Minter is not set in transferlist

      comment "Unsetting transferlist"
      unsetTransferlist nettestOwner

    configureMinterScenario = do
      comment $ "Adding minters to limit"

      -- We originate a new contract to make sure the minters list is empty
      scAddress' <- do
        let str = mkInitialStorage (originationParams { opMinters = mempty })
        originateUntypedSimple "nettest.Stablecoin_for_minter_test" (untypeValue $ toVal str) (T.convertContract stablecoinContract)

      let
        sc' = TAddress @Parameter scAddress'
        addMinter' ma = configureMinter' sc' nettestMasterMinter ma Nothing 100

      comment "Send some tez to master minter"
      transfer $
        TransferData
          { tdFrom = AddressResolved superuser
          , tdTo = AddressResolved nettestMasterMinter
          , tdAmount = toMutez 100000000
          , tdEntrypoint = DefEpName
          , tdParameter = ()
          }

      mapM_ (\i -> newAddress  ("minter" <> show i) >>= addMinter') [1..minterLimit]

  transferScenario
  mintScenario
  burnScenario
  permissionReassigmentScenario
  transferlistScenario
  configureMinterScenario
