-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Nettest.Nettest
  ( test_scenarioWithInternalTransferlist
  , test_scenarioWithExternalTransferlist
  ) where

import qualified Data.Set as Set
import qualified Data.Text.IO.Utf8 as Utf8
import Test.Tasty (TestTree)

import Lorentz hiding (comment, (>>))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.Common
import Morley.Michelson.Parser.Types (MichelsonSource(MSFile))
import Morley.Michelson.Runtime (parseExpandContract)
import Morley.Michelson.Typed (untypeValue)
import qualified Morley.Michelson.Typed as T
import qualified Morley.Michelson.Untyped as U (Contract)
import Morley.Util.Named
import Test.Cleveland

import qualified Indigo.Contracts.Transferlist.External as External
import qualified Indigo.Contracts.Transferlist.Internal as Internal
import Lorentz.Contracts.Stablecoin

test_scenarioWithInternalTransferlist :: TestTree
test_scenarioWithInternalTransferlist =
  testScenario "Stablecoin contract nettest scenarioWithInternalTransferlist" $
    scNettestScenario
      originateTransferlistInternal
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

originateTransferlistInternal :: MonadOps f => Set (Address, Address) -> Set Address -> f Address
originateTransferlistInternal transfers receivers = chAddress <$>
  originateSimple
    "nettest.transferlist_internal"
    (Internal.Storage transfers receivers)
    (Internal.transferlistContract)

originateTransferlistExternal
  :: MonadCleveland caps m
  => U.Contract -> Set (Address, Address) -> Set Address -> m Address
originateTransferlistExternal externalTransferlistContract transfers receivers = do
  testOwner <- newAddress "testOwner"
  originateUntypedSimple
    "nettest.transferlist_internal"
    (untypeValue @(ToT External.Storage) . toVal
      $ External.convertToExternalStorage
          (Internal.Storage transfers receivers)
          testOwner -- issuer
          testOwner -- admin
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
  => (forall m' caps. MonadCleveland caps m' => Set (Address, Address) -> Set Address -> m' Address)
  -> TransferlistType
  -> Scenario m
scNettestScenario originateTransferlist transferlistType = scenario do
  comment "Resolving contract managers"

  superuser <- refillable $ newAddress "superuser"

  nettestOwner <- newAddress auto
  nettestPauser <- newAddress auto
  nettestMasterMinter <- newAddress auto

  comment "Resolving owners and operators"

  owner1 <- newAddress auto
  owner2 <- newAddress auto
  owner3 <- newAddress auto

  operator <- newAddress auto
  otherOperator <- newAddress auto

  comment "Originating metadata contract"
  let metadata =  metadataJSON Nothing Nothing
  cmrAddress <- nettestOriginateContractMetadataContract metadata
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
          ){ opMetadataUri = RemoteContract cmrAddress
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
    sc :: TAddress Parameter ()
    sc = TAddress scAddress

    tp :: Address -> Address -> Natural -> FA2.TransferParams
    tp from to value = constructSingleTransfer
      (#from_ :! from)
      (#to_ :! to)
      (#amount :! value)

    callTransferWithOperator op from to value =
      withSender op $ call sc (Call @"Transfer") (tp from to value)

    callTransfer = join callTransferWithOperator

    callTransfers
      :: MonadCleveland caps m
      => [("from_" :! Address, [("to_" :! Address, "amount" :! Natural)])]
      -> m ()
    callTransfers = mapM_ $ \(from@(arg #from_ -> from_), destinations) ->
      withSender from_ $
        call sc (Call @"Transfer") (constructTransfersFromSender from destinations)

    configureMinter :: MonadCleveland caps m => Address -> Address -> Maybe Natural -> Natural -> m ()
    configureMinter = configureMinter' sc

    configureMinter'
      :: MonadCleveland caps m
      => TAddress Parameter () -> Address -> Address -> Maybe Natural -> Natural -> m ()
    configureMinter' sc' from for' expectedAllowance newAllowance =
      withSender from $
        call sc' (Call @"Configure_minter") (ConfigureMinterParam for' expectedAllowance newAllowance)

    removeMinter :: MonadCleveland caps m => Address -> Address -> m ()
    removeMinter from whom =
      withSender from $
        call sc (Call @"Remove_minter") whom

    addOperatorNettest :: MonadCleveland caps m => Address -> Address -> m ()
    addOperatorNettest from op =
      withSender from $
        call sc (Call @"Update_operators")
          [ FA2.AddOperator
            FA2.OperatorParam { opOwner = from, opOperator = op, opTokenId = FA2.theTokenId }
          ]

    removeOperator :: MonadCleveland caps m => Address -> Address -> m ()
    removeOperator from op =
      withSender from $
        call sc (Call @"Update_operators")
          [ FA2.RemoveOperator
            FA2.OperatorParam { opOwner = from, opOperator = op, opTokenId = FA2.theTokenId }
          ]

    mint :: MonadCleveland caps m => Address -> Natural -> m ()
    mint to_ value =
      withSender to_ $
        call sc (Call @"Mint") [MintParam to_ value]

    burn :: MonadCleveland caps m => Address -> Natural -> m ()
    burn from amount_ =
      withSender from $
        call sc (Call @"Burn") [amount_]

    pause :: MonadCleveland caps m => Address -> m ()
    pause from =
      withSender from $
        call sc (Call @"Pause") ()

    unpause :: MonadCleveland caps m => Address -> m ()
    unpause from =
      withSender from $
        call sc (Call @"Unpause") ()

    transferOwnership :: MonadCleveland caps m => Address -> Address -> m ()
    transferOwnership from to =
      withSender from $
        call sc (Call @"Transfer_ownership") to

    acceptOwnership :: MonadCleveland caps m => Address -> m ()
    acceptOwnership from =
      withSender from $
        call sc (Call @"Accept_ownership") ()

    changeMasterMinter :: MonadCleveland caps m => Address -> Address -> m ()
    changeMasterMinter from to =
      withSender from $
        call sc (Call @"Change_master_minter") to

    changePauser :: MonadCleveland caps m => Address -> Address -> m ()
    changePauser from to =
      withSender from $
        call sc (Call @"Change_pauser") to

    setTransferlist :: MonadCleveland caps m => Address -> Address -> m ()
    setTransferlist from transferlistAddress =
      withSender from $
        call sc (Call @"Set_transferlist") (Just transferlistAddress)

    unsetTransferlist :: MonadCleveland caps m => Address -> m ()
    unsetTransferlist from =
      withSender from $
        call sc (Call @"Set_transferlist") (Nothing :: Maybe Address)

  let
    transferScenario = do
      comment "Transfers from superuser"
      callTransfer superuser superuser 111
      callTransfer superuser owner1 40

      comment "Transfers between owners"
      callTransfers
        [ (#from_ :! owner1, [(#to_ :! owner2, #amount :! 10)])
        , (#from_ :! owner2, [(#to_ :! owner3, #amount :! 10)])
        , (#from_ :! owner3, [(#to_ :! owner1, #amount :! 10)])
        ]

      comment "Pausing contract for transfers"
      callTransfer superuser nettestPauser 600000 -- storage fee
      expectError [mt|NOT_PAUSER|] $ pause owner1 -- Not enough permissions
      pause nettestPauser
      expectError [mt|CONTRACT_PAUSED|] $ pause nettestPauser -- Cannot be called multiple times

      expectError [mt|CONTRACT_PAUSED|] $ callTransfer owner3 owner2 10

      comment "Unpausing contract for transfers"
      expectError [mt|NOT_PAUSER|] $ unpause owner3 -- Not enough permissions
      unpause nettestPauser
      expectError [mt|CONTRACT_NOT_PAUSED|] $ unpause nettestPauser -- Cannot be called multiple times

      callTransfer owner1 owner2 10
      callTransfer owner2 owner1 10

      comment $ "Updating transfer operators for owners"
      callTransfer superuser owner1 10
      addOperatorNettest owner1 otherOperator
      callTransferWithOperator otherOperator owner1 owner2 10
      expectError [mt|FA2_NOT_OPERATOR|] $ callTransferWithOperator otherOperator owner2 owner1 10
      removeOperator owner1 otherOperator
      expectError [mt|FA2_NOT_OPERATOR|] $ callTransferWithOperator otherOperator owner1 owner2 10

      callTransfer owner2 owner1 10

    mintScenario = do
      comment $ "Mint to superuser"
      mint superuser 200
      callTransfer superuser nettestMasterMinter 1200 -- Needed to pay transfer fee

      comment "Configuring minter for owner1 and owner2"
      configureMinter nettestMasterMinter owner1 Nothing 100
      expectError [mt|ALLOWANCE_MISMATCH|] $ configureMinter nettestMasterMinter owner1 (Just 20) 10 -- Mismatched expected allowance
      configureMinter nettestMasterMinter owner1 (Just 100) 50
      expectError [mt|ADDR_NOT_MINTER|] $ configureMinter nettestMasterMinter owner2 (Just 20) 10 -- Not a minter
      configureMinter nettestMasterMinter owner2 Nothing 10

      comment $ "Minting for owner1 and owner2"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      mint owner1 20
      expectError [mt|ALLOWANCE_EXCEEDED|] $ mint owner1 200 -- Allowance exceeded

      comment "Transfer between owner1 and owner2"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      callTransfer owner1 owner2 20
      mint owner2 10

      callTransfer owner2 owner1 30

      comment "Remove owner1 and owner2 from minters"
      removeMinter nettestMasterMinter owner2
      expectError [mt|ADDR_NOT_MINTER|] $ removeMinter nettestMasterMinter owner2 -- Already removed
      removeMinter nettestMasterMinter owner1

    burnScenario = do
      comment $ "Burning for owner1"
      callTransfer superuser owner1 10 -- Needed to pay transfer fee
      callTransfer superuser nettestMasterMinter 1000
      configureMinter nettestMasterMinter owner1 Nothing 100
      mint owner1 100
      burn owner1 20
      expectError [mt|FA2_INSUFFICIENT_BALANCE|] $ burn owner1 2000 -- Not enough tokens to burn
      comment "Transfer rest of the tokens after burn from owner1"
      callTransfer owner1 owner2 80
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
      configureMinter owner1 owner2 Nothing 10
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

      callTransfer owner2 owner3 10

      -- transfer is not in transferlist
      let action = callTransfer owner3 owner2 9
      case transferlistType of
        Internal -> expectCustomError_ #assertionFailure action
        External -> expectError [mt|outbound not transferlisted|] action

      configureMinter nettestMasterMinter owner1 Nothing 100
      configureMinter nettestMasterMinter owner2 Nothing 100

      mint owner2 20
      expectError [mt|NOT_MINTER|] $ mint owner3 20 -- Minter is not set in transferlist

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
      withSender superuser $ transfer $
        TransferData
          { tdTo = nettestMasterMinter
          , tdAmount = [tz|100|]
          , tdEntrypoint = DefEpName
          , tdParameter = ()
          }
      replicateM_ minterLimit (newAddress auto >>= addMinter')

  transferScenario
  mintScenario
  burnScenario
  permissionReassigmentScenario
  transferlistScenario
  configureMinterScenario
