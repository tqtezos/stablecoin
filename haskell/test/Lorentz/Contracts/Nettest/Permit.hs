-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Lorentz.Contracts.Nettest.Permit
  ( test_permitScenario
  ) where

import Lorentz (Packed(..), lPackValue)

import Test.Tasty (TestTree)

import Lorentz.Contracts.Spec.FA2Interface (TransferDestination(..), TransferItem(..))
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Test.Cleveland

import Lorentz.Contracts.Stablecoin
  (ConfigureMinterParam(..), FA2Parameter(..), MetadataUri(..), Parameter(..), PermitParam(..),
  metadataJSON, mkPermitHash)
import Lorentz.Contracts.Test.Common
  (OriginationParams(..), addAccount, defaultOriginationParams,
  nettestOriginateContractMetadataContract, originateStablecoin, testFA2TokenMetadata)
import Test.Cleveland.Lorentz (toContractAddress)

test_permitScenario :: TestTree
test_permitScenario = testScenario "permitScenario" permitScenario

permitScenario :: Scenario m
permitScenario = scenario do
  comment "-- Permits tests --"
  comment "Creating accounts"
  owner1 <- newAddress "nettestOwner1"
  owner2 <- newAddress "nettestOwner2"
  pauser <- newAddress "nettestPauser"
  masterMinter <- newAddress "nettestMasterMinter"

  -- user with no privileges
  user1 <- newAddress auto

  comment "Originating contracts"
  let metadata = metadataJSON (Just testFA2TokenMetadata) Nothing
  cmrAddress <- nettestOriginateContractMetadataContract metadata
  let originationParams =
        addAccount (owner1 , ([], 1000)) $
          (defaultOriginationParams
            (#owner :! owner1)
            (#pauser :! pauser)
            (#masterMinter :! masterMinter))
            { opMetadataUri = RemoteContract $ toContractAddress cmrAddress
            }
  contract <- originateStablecoin originationParams
  chainId <- getChainId

  let
    issuePermit :: MonadCleveland caps m => Natural -> ImplicitAddress -> Parameter -> m ()
    issuePermit counter addr param = do
      -- NOTE: A couples of changes were introduced in #124. Crucially:
      --
      -- 1. The TZIP-17 views have been moved off-chain
      -- 2. In order to run them, you must first know the full storage value.
      --
      -- The current "Morley.Nettest" interface does not allow us to retrieve the
      -- contract's storage if it contains big maps and, in this case, it does.
      -- Therefore, we cannot run the "GetCounter" view using "Morley.Nettest".
      --
      -- TODO: when morley/#346 is merged, we can go back to using
      -- the "GetCounter" view.
      -- https://gitlab.com/morley-framework/morley/-/issues/346
      --
      -- counter <- getCounter

      let permitHash = mkPermitHash param
      let Packed bytes = lPackValue ((toAddress contract, chainId), (counter, permitHash))
      sig <- signBytes bytes addr
      pk <- getPublicKey addr
      transfer contract $ calling (ep @"Permit") (PermitParam pk sig permitHash)

  comment "Issue a permit for Pause"
  issuePermit 0 pauser Pause
  withSender user1 $ transfer contract $ calling (ep @"Pause") ()

  comment "Issue a permit for Unpause"
  issuePermit 1 pauser Unpause
  withSender user1 $ transfer contract $ calling (ep @"Unpause") ()

  comment "Issue a permit for Transfer"
  let transferParam =
        [ TransferItem (toAddress owner1) [TransferDestination (toAddress user1) FA2.theTokenId 1] ]
  issuePermit 2 owner1 (Call_FA2 $ Transfer transferParam)
  withSender user1 $ transfer contract $ calling (ep @"Transfer") transferParam

  comment "Issue a permit for Update_operators"
  let param =
        [ FA2.AddOperator
            FA2.OperatorParam { opOwner = toAddress owner1, opOperator = toAddress user1, opTokenId = FA2.theTokenId }
        ]
  issuePermit 3 owner1 (Call_FA2 $ Update_operators param)
  withSender user1 $ transfer contract $ calling (ep @"Update_operators") param

  comment "Issue a permit for Configure_minter"
  let configureMinterParam = ConfigureMinterParam (toAddress user1) Nothing 30
  issuePermit 4 masterMinter (Configure_minter configureMinterParam)
  withSender user1 $ transfer contract $ calling (ep @"Configure_minter") configureMinterParam

  comment "Issue a permit for Remove_minter"
  let removeMinterParam = user1
  issuePermit 5 masterMinter (Remove_minter $ toAddress removeMinterParam)
  withSender user1 $ transfer contract $ calling (ep @"Remove_minter") $ toAddress removeMinterParam

  comment "Issue a permit for Set_transferlist"
  issuePermit 6 owner1 (Set_transferlist Nothing)
  withSender user1 $ transfer contract $ calling (ep @"Set_transferlist") Nothing

  comment "Issue a permit for Change_pauser"
  issuePermit 7 owner1 (Change_pauser $ toAddress owner2)
  withSender user1 $ transfer contract $ calling (ep @"Change_pauser") $ toAddress owner2

  comment "Issue a permit for Change_master_minter"
  issuePermit 8 owner1 (Change_master_minter $ toAddress owner2)
  withSender user1 $ transfer contract $ calling (ep @"Change_master_minter") $ toAddress owner2

  comment "Issue a permit for Transfer_ownership"
  issuePermit 9 owner1 (Transfer_ownership $ toAddress owner2)
  withSender user1 $ transfer contract $ calling (ep @"Transfer_ownership") $ toAddress owner2

  comment "Issue a permit for Accept_ownership"
  issuePermit 10 owner2 Accept_ownership
  withSender user1 $ transfer contract $ calling (ep @"Accept_ownership") ()
