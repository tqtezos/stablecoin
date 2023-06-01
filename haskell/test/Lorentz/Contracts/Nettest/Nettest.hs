-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

-- for Ord ImplicitAddressWithAlias
{-# OPTIONS_GHC -Wno-orphans #-}

module Lorentz.Contracts.Nettest.Nettest
  ( test_scenarioWithInternalTransferlist
  , test_scenarioWithExternalTransferlist
  ) where

import Data.List.NonEmpty (groupBy)
import Data.Set qualified as Set
import Data.Text.IO.Utf8 qualified as Utf8
import Test.Tasty (TestTree)

import Lorentz hiding (comment, (>>))
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Lorentz.Contracts.Test.Common
import Morley.Michelson.Parser.Types (MichelsonSource(MSFile))
import Morley.Michelson.Runtime (parseExpandContract)
import Morley.Michelson.Typed (untypeValue)
import Morley.Michelson.Untyped qualified as U (Contract)
import Morley.Util.Named
import Morley.Util.SizedList qualified as SL
import Morley.Util.SizedList.Types
import Test.Cleveland

import Indigo.Contracts.Transferlist.External qualified as External
import Indigo.Contracts.Transferlist.Internal qualified as Internal
import Lorentz.Contracts.Stablecoin

test_scenarioWithInternalTransferlist :: TestTree
test_scenarioWithInternalTransferlist =
  testScenario "Stablecoin contract nettest scenarioWithInternalTransferlist" $
    scNettestScenario
      (const originateTransferlistInternal)
      Internal

test_scenarioWithExternalTransferlist :: IO TestTree
test_scenarioWithExternalTransferlist = do
  externalTransferlistFile <- Utf8.readFile externalTransferlistContractPath
  externalTransferlistContract <- either (error "cannot parse transferlist contract") pure $
    parseExpandContract (MSFile externalTransferlistContractPath) externalTransferlistFile

  pure $ testScenario "Stablecoin contract nettest scenarioWithExternalTransferlist" $
    scNettestScenario
      (originateTransferlistExternal externalTransferlistContract)
      External

externalTransferlistContractPath :: FilePath
externalTransferlistContractPath = "test/resources/transferlist.tz"

originateTransferlistInternal :: MonadOps m => Set (ImplicitAddressWithAlias, ImplicitAddressWithAlias) -> Set ImplicitAddressWithAlias -> m ContractAddress
originateTransferlistInternal transfers receivers = toContractAddress <$>
  originate
    "nettest.transferlist_internal"
    (Internal.Storage
      (Set.map (bimap toAddress toAddress) transfers)
      (Set.map toAddress receivers)
    )
    (Internal.transferlistContract)

originateTransferlistExternal
  :: (MonadOps m, ToAddress addr)
  => U.Contract
  -> addr
  -> Set (ImplicitAddressWithAlias, ImplicitAddressWithAlias)
  -> Set ImplicitAddressWithAlias
  -> m ContractAddress
originateTransferlistExternal externalTransferlistContract testOwner transfers receivers = do
  originate
    "nettest.transferlist_internal"
    (untypeValue @(ToT External.Storage) . toVal
      $ External.convertToExternalStorage
          (Internal.Storage
            (Set.map (bimap toAddress toAddress) transfers)
            (Set.map toAddress receivers)
          )
          (toAddress testOwner) -- issuer
          (toAddress testOwner) -- admin
    )
    externalTransferlistContract

-- | Indicates whether we're using the external safelist (the one parsed from @transferlist.tz@)
-- or the internal safelist ('Indigo.Contracts.Transferlist.Internal')
-- to run these tests.
--
-- Depending on this, the tests might expect different @FAILWITH@ messages.
data TransferlistType = External | Internal

scNettestScenario
  :: Monad m
  => (forall m'. MonadOps m'
      => ImplicitAddressWithAlias
      -> Set (ImplicitAddressWithAlias, ImplicitAddressWithAlias)
      -> Set ImplicitAddressWithAlias
      -> m' ContractAddress)
  -> TransferlistType
  -> Scenario m
scNettestScenario originateTransferlist transferlistType = scenario do
  comment "Creating accounts"

  superuser
    ::< testOwner
    ::< nettestOwner
    ::< nettestPauser
    ::< nettestMasterMinter
    ::< owner1
    ::< owner2
    ::< owner3
    ::< operator
    ::< otherOperator
    ::< Nil'
    <- newAddresses $ "superuser" :< "testOwner" :< SL.replicateT auto
  void $ refillable $ pure superuser

  comment "Originating metadata contract"
  let metadata =  metadataJSON Nothing Nothing
  cmrAddress <- nettestOriginateContractMetadataContract "ContractMetadata" metadata
  let
    originationParams =
        addAccount (superuser , ([operator], 1110000))
      . addAccount (owner1, ([operator], 100))
      . addAccount (owner2, ([operator], 0))
      . addAccount (owner3, ([operator], 0))
      . addAccount (nettestPauser, ([operator], 0))
      . addMinter (superuser, 200)
      $ (defaultOriginationParams
          (#owner :! nettestOwner)
          (#pauser :! nettestPauser)
          (#masterMinter :! nettestMasterMinter)
          ){ opMetadataUri = RemoteContract $ toContractAddress cmrAddress
          }
  let
    transfers =
      Set.fromList
        [ (owner1, owner2)
        , (owner2, owner3)
        , (owner3, owner1)
        ]
    receivers = Set.fromList [owner1, owner2]

  comment "Originating stablecoin and transferlist contracts"
  (sc, sfAddress) <- inBatch $ (,)
    <$> originateStablecoin originationParams
    <*> originateTransferlist testOwner transfers receivers

  let
    tp :: Address -> Address -> Natural -> FA2.TransferParams
    tp from to value = constructSingleTransfer
      (#from_ :! from)
      (#to_ :! to)
      (#amount :! value)

    callTransfersWithOperator
      :: MonadCleveland caps m
      => ImplicitAddressWithAlias
      -> [("from" :! ImplicitAddressWithAlias, "to" :! ImplicitAddressWithAlias, Natural)]
      -> m ()
    callTransfersWithOperator op ts =
      withSender op $ inBatch $
        for_ ts \(arg #from -> from, arg #to -> to, value)
          -> transfer sc $ calling (ep @"Transfer") (tp (toAddress from) (toAddress to) value)

    callTransfer
      :: MonadCleveland caps m
      => ImplicitAddressWithAlias
      -> [(ImplicitAddressWithAlias, Natural)]
      -> m ()
    callTransfer op = callTransfersWithOperator op
      . Prelude.map \(to, val) -> (#from :! op, #to :! to, val)

    callTransfers
      :: MonadCleveland caps m
      => [("from_" :! ImplicitAddressWithAlias, [("to_" :! ImplicitAddressWithAlias, "amount" :! Natural)])]
      -> m ()
    callTransfers ts = forM_ (groupBy ((==) `on` fst) ts) $
      \((from@(arg #from_ -> from_), _):|(fmap snd -> destinations)) ->
          withSender from_ $ inBatch $
            for destinations \destinations' ->
              transfer sc $ calling (ep @"Transfer") (constructTransfersFromSender from destinations')

    configureMinters
      :: MonadCleveland caps m
      => ImplicitAddressWithAlias
      -> [(ImplicitAddressWithAlias, Maybe Natural, Natural)]
      -> m ()
    configureMinters = configureMinters' sc

    configureMinters'
      :: MonadCleveland caps m
      => ContractHandle Parameter Storage ()
      -> ImplicitAddressWithAlias
      -> [(ImplicitAddressWithAlias, Maybe Natural, Natural)]
      -> m ()
    configureMinters' sc' from minters =
      withSender from $ inBatch $
        for_ minters \(for', expectedAllowance, newAllowance) ->
          transfer sc' $ calling (ep @"Configure_minter") (ConfigureMinterParam (toAddress for') expectedAllowance newAllowance)

    removeMinter
      :: MonadCleveland caps m
      => ImplicitAddressWithAlias
      -> ImplicitAddressWithAlias
      -> m ()
    removeMinter from whom =
      withSender from $
        transfer sc $ calling (ep @"Remove_minter") (toAddress whom)

    addOperatorNettest
      :: MonadCleveland caps m
      => ImplicitAddressWithAlias
      -> ImplicitAddressWithAlias
      -> m ()
    addOperatorNettest from op =
      withSender from $
        transfer sc $ calling (ep @"Update_operators")
          [ FA2.AddOperator
            FA2.OperatorParam { opOwner = toAddress from, opOperator = toAddress op, opTokenId = FA2.theTokenId }
          ]

    removeOperator :: MonadCleveland caps m => ImplicitAddressWithAlias -> ImplicitAddressWithAlias -> m ()
    removeOperator from op =
      withSender from $
        transfer sc $ calling (ep @"Update_operators")
          [ FA2.RemoveOperator
            FA2.OperatorParam { opOwner = toAddress from, opOperator = toAddress op, opTokenId = FA2.theTokenId }
          ]

    mint :: MonadCleveland caps m => ImplicitAddressWithAlias -> Natural -> m ()
    mint to_ value =
      withSender to_ $
        transfer sc $ calling (ep @"Mint") [MintParam (toAddress to_) value]

    burn :: MonadCleveland caps m => ImplicitAddressWithAlias -> Natural -> m ()
    burn from amount_ =
      withSender from $
        transfer sc $ calling (ep @"Burn") [amount_]

    pause :: MonadCleveland caps m => ImplicitAddressWithAlias -> m ()
    pause from =
      withSender from $
        transfer sc $ calling (ep @"Pause") ()

    unpause :: MonadCleveland caps m => ImplicitAddressWithAlias -> m ()
    unpause from =
      withSender from $
        transfer sc $ calling (ep @"Unpause") ()

    transferOwnership :: MonadCleveland caps m => ImplicitAddressWithAlias -> ImplicitAddressWithAlias -> m ()
    transferOwnership from to =
      withSender from $
        transfer sc $ calling (ep @"Transfer_ownership") (toAddress to)

    acceptOwnership :: MonadCleveland caps m => ImplicitAddressWithAlias -> m ()
    acceptOwnership from =
      withSender from $
        transfer sc $ calling (ep @"Accept_ownership") ()

    changeMasterMinter :: MonadCleveland caps m => ImplicitAddressWithAlias -> ImplicitAddressWithAlias -> m ()
    changeMasterMinter from to =
      withSender from $
        transfer sc $ calling (ep @"Change_master_minter") (toAddress to)

    changePauser :: MonadCleveland caps m => ImplicitAddressWithAlias -> ImplicitAddressWithAlias -> m ()
    changePauser from to =
      withSender from $
        transfer sc $ calling (ep @"Change_pauser") (toAddress to)

    setTransferlist :: MonadCleveland caps m => ImplicitAddressWithAlias -> ContractAddress -> m ()
    setTransferlist from transferlistAddress =
      withSender from $
        transfer sc $ calling (ep @"Set_transferlist") (Just $ toAddress transferlistAddress)

    unsetTransferlist :: MonadCleveland caps m => ImplicitAddressWithAlias -> m ()
    unsetTransferlist from =
      withSender from $
        transfer sc $ calling (ep @"Set_transferlist") (Nothing :: Maybe Address)

  let
    transferScenario = do
      comment "Transfers from superuser"
      callTransfer superuser
        [ (superuser, 111)
        , (owner1, 40)
        , (nettestPauser, 600000)
        ]

      comment "Transfers between owners"
      callTransfers
        [ (#from_ :! owner1, [(#to_ :! owner2, #amount :! 10)])
        , (#from_ :! owner2, [(#to_ :! owner3, #amount :! 10)])
        , (#from_ :! owner3, [(#to_ :! owner1, #amount :! 10)])
        ]

      comment "Pausing contract for transfers"
      expectError [mt|NOT_PAUSER|] $ pause owner1 -- Not enough permissions
      pause nettestPauser
      expectError [mt|CONTRACT_PAUSED|] $ pause nettestPauser -- Cannot be called multiple times

      expectError [mt|CONTRACT_PAUSED|] $ callTransfer owner3 [(owner2, 10)]

      comment "Unpausing contract for transfers"
      expectError [mt|NOT_PAUSER|] $ unpause owner3 -- Not enough permissions
      unpause nettestPauser
      expectError [mt|CONTRACT_NOT_PAUSED|] $ unpause nettestPauser -- Cannot be called multiple times

      callTransfer owner1 [(owner2, 10)]
      callTransfer owner2 [(owner1, 10)]

      comment $ "Updating transfer operators for owners"
      callTransfer superuser [(owner1, 10)]
      addOperatorNettest owner1 otherOperator
      callTransfersWithOperator otherOperator [(#from :! owner1, #to :! owner2, 10)]
      callTransfersWithOperator otherOperator [(#from :! owner2, #to :! owner1, 10)]
        & expectError [mt|FA2_NOT_OPERATOR|]
      removeOperator owner1 otherOperator
      callTransfersWithOperator otherOperator [(#from :! owner1, #to :! owner2, 10)]
        & expectError [mt|FA2_NOT_OPERATOR|]

      callTransfer owner2 [(owner1, 10)]

    mintScenario = do
      comment $ "Mint to superuser"
      mint superuser 200
      callTransfer superuser
        [ (nettestMasterMinter, 1200)
        , (owner1, 20)
        ]
        -- Needed to pay transfer fee

      comment "Configuring minter for owner1 and owner2"
      configureMinters nettestMasterMinter [(owner1, Nothing, 100)]
      configureMinters nettestMasterMinter [(owner1, Just 20, 10)] -- Mismatched expected allowance
        & expectError [mt|ALLOWANCE_MISMATCH|]
      configureMinters nettestMasterMinter [(owner1, Just 100, 50)]
      configureMinters nettestMasterMinter [(owner2, Just 20, 10)] -- Not a minter
        & expectError [mt|ADDR_NOT_MINTER|]
      configureMinters nettestMasterMinter [(owner2, Nothing, 10)]

      comment $ "Minting for owner1 and owner2"
      mint owner1 20
      expectError [mt|ALLOWANCE_EXCEEDED|] $ mint owner1 200 -- Allowance exceeded

      comment "Transfer between owner1 and owner2"
      callTransfer owner1 [(owner2, 20)]
      mint owner2 10

      callTransfer owner2 [(owner1, 30)]

      comment "Remove owner1 and owner2 from minters"
      removeMinter nettestMasterMinter owner2
      expectError [mt|ADDR_NOT_MINTER|] $ removeMinter nettestMasterMinter owner2 -- Already removed
      removeMinter nettestMasterMinter owner1

    burnScenario = do
      comment $ "Burning for owner1"
      callTransfer superuser
        [ (owner1, 10)  -- Needed to pay transfer fee
        , (nettestMasterMinter, 1000)
        ]
      configureMinters nettestMasterMinter [(owner1, Nothing, 100)]
      mint owner1 100
      burn owner1 20
      burn owner1 2000 -- Not enough tokens to burn
        & expectError [mt|FA2_INSUFFICIENT_BALANCE|]
      comment "Transfer rest of the tokens after burn from owner1"
      callTransfer owner1 [(owner2, 80)]
      removeMinter nettestMasterMinter owner1

    permissionReassigmentScenario = do
      comment "Transferring contract ownership"
      transferOwnership nettestOwner owner1
      transferOwnership nettestOwner owner2
      expectError [mt|NOT_PENDING_OWNER|] $ acceptOwnership owner1 -- Pending owner is changed
      acceptOwnership owner2
      transferOwnership owner2 nettestOwner
      acceptOwnership nettestOwner

      comment "Changing master minter"
      changeMasterMinter nettestOwner owner1
      configureMinters owner1 [(owner2, Nothing, 10)]
      mint owner2 10
      changeMasterMinter nettestOwner nettestMasterMinter
      removeMinter nettestMasterMinter owner2

      comment "Changing contract pauser"
      changePauser nettestOwner owner1
      pause owner1
      expectError [mt|NOT_PAUSER|] $ unpause nettestPauser -- nettestPauser is not pauser
      unpause owner1
      changePauser nettestOwner nettestPauser


    transferlistScenario = do
      comment $ "Transferlist interaction"

      setTransferlist nettestOwner sfAddress

      callTransfer owner2 [(owner3, 10)]

      -- transfer is not in transferlist
      let action = callTransfer owner3 [(owner2, 9)]
      case transferlistType of
        Internal -> expectCustomError_ #assertionFailure action
        External -> expectError [mt|outbound not transferlisted|] action

      configureMinters nettestMasterMinter
        [ (owner1, Nothing, 100)
        , (owner2, Nothing, 100)
        ]

      mint owner2 20
      expectError [mt|NOT_MINTER|] $ mint owner3 20 -- Minter is not set in transferlist

      comment "Unsetting transferlist"
      unsetTransferlist nettestOwner

    configureMinterScenario = do
      comment $ "Adding minters to limit"

      -- We originate a new contract to make sure the minters list is empty
      sc' <- originateStablecoin $ originationParams { opMinters = mempty }

      comment "Send some tez to master minter"
      withSender superuser $ transfer nettestMasterMinter [tz|100|]

      addrs <- traverse newFreshAddress $ replicate minterLimit auto

      configureMinters' sc' nettestMasterMinter $
        (, Nothing, 100) <$> addrs

  transferScenario
  mintScenario
  burnScenario
  permissionReassigmentScenario
  transferlistScenario
  configureMinterScenario
