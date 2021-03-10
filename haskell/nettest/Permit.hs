-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

module Permit
  ( permitScenario
  ) where

import Fmt (build)

import Lorentz (Address, Packed(..), TAddress(..), lPackValue, toVal)
import Michelson.Typed (convertContract, untypeValue)
import Morley.Nettest

import Lorentz.Contracts.Spec.FA2Interface (TransferDestination(..), TransferItem(..))
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin
  (ConfigureMinterParam(..), FA2Parameter(..), MetadataUri(..), Parameter(..), PermitParam(..),
  metadataJSON, mkPermitHash, stablecoinContract)

import Lorentz.Contracts.Test.Common
  (OriginationParams(..), addAccount, defaultOriginationParams, mkInitialStorage,
  nettestOriginateContractMetadataContract, testFA2TokenMetadata)

permitScenario :: NettestScenario m
permitScenario = uncapsNettest $ do
  comment "-- Permits tests --"
  comment "Creating accounts"
  owner1 <- newAddress "nettestOwner1"
  owner2 <- newAddress "nettestOwner2"
  pauser <- newAddress "nettestPauser"
  masterMinter <- newAddress "nettestMasterMinter"

  -- user with no privileges
  user1 <- newAddress auto

  comment "Originating contracts"
  metadata <- either (failure . build) pure $ metadataJSON (Just testFA2TokenMetadata) Nothing
  cmrAddress <- nettestOriginateContractMetadataContract metadata
  let originationParams =
        addAccount (owner1 , ([], 1000)) $
          defaultOriginationParams
            { opOwner = owner1
            , opPauser = pauser
            , opMasterMinter = masterMinter
            , opMetadataUri = RemoteContract cmrAddress
            }
      storage = mkInitialStorage originationParams
  contract <- TAddress @Parameter <$> originateUntypedSimple
    "nettest.Stablecoin"
    (untypeValue (toVal storage))
    (convertContract stablecoinContract)
  chainId <- getChainId

  let
    issuePermit :: MonadNettest caps base m => Natural -> Address -> Parameter -> m ()
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
      let Packed bytes = lPackValue ((contract, chainId), (counter, permitHash))
      sig <- signBytes bytes addr
      pk <- getPublicKey addr
      call contract (Call @"Permit") (PermitParam pk sig permitHash)

  comment "Issue a permit for Pause"
  issuePermit 0 pauser Pause
  withSender user1 $ call contract (Call @"Pause") ()

  comment "Issue a permit for Unpause"
  issuePermit 1 pauser Unpause
  withSender user1 $ call contract (Call @"Unpause") ()

  comment "Issue a permit for Transfer"
  let transferParam =
        [ TransferItem owner1 [TransferDestination user1 FA2.theTokenId 1] ]
  issuePermit 2 owner1 (Call_FA2 $ Transfer transferParam)
  withSender user1 $ call contract (Call @"Transfer") transferParam

  comment "Issue a permit for Update_operators"
  let param =
        [ FA2.AddOperator
            FA2.OperatorParam { opOwner = owner1, opOperator = user1, opTokenId = FA2.theTokenId }
        ]
  issuePermit 3 owner1 (Call_FA2 $ Update_operators param)
  withSender user1 $ call contract (Call @"Update_operators") param

  comment "Issue a permit for Configure_minter"
  let configureMinterParam = ConfigureMinterParam user1 Nothing 30
  issuePermit 4 masterMinter (Configure_minter configureMinterParam)
  withSender user1 $ call contract (Call @"Configure_minter") configureMinterParam

  comment "Issue a permit for Remove_minter"
  let removeMinterParam = user1
  issuePermit 5 masterMinter (Remove_minter removeMinterParam)
  withSender user1 $ call contract (Call @"Remove_minter") removeMinterParam

  comment "Issue a permit for Set_transferlist"
  issuePermit 6 owner1 (Set_transferlist Nothing)
  withSender user1 $ call contract (Call @"Set_transferlist") Nothing

  comment "Issue a permit for Change_pauser"
  issuePermit 7 owner1 (Change_pauser owner2)
  withSender user1 $ call contract (Call @"Change_pauser") owner2

  comment "Issue a permit for Change_master_minter"
  issuePermit 8 owner1 (Change_master_minter owner2)
  withSender user1 $ call contract (Call @"Change_master_minter") owner2

  comment "Issue a permit for Transfer_ownership"
  issuePermit 9 owner1 (Transfer_ownership owner2)
  withSender user1 $ call contract (Call @"Transfer_ownership") owner2

  comment "Issue a permit for Accept_ownership"
  issuePermit 10 owner2 Accept_ownership
  withSender user1 $ call contract (Call @"Accept_ownership") ()
