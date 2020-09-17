-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT
{-# LANGUAGE PartialTypeSignatures #-}

module Permit
  ( permitScenario
  ) where

import Fmt (pretty)
import Lorentz
  (Address, ChainId, Contract, EntrypointRef(Call, CallDefault), TAddress(..), ToT,
  defaultContract, fromVal, lPackValue, lUnpackValue, mkView, mt, toVal, ( # ))
import qualified Lorentz as L
import Lorentz.Test.Consumer (contractConsumer)
import Michelson.Typed (convertContract, untypeValue)
import qualified Michelson.Typed as T
import Morley.Client (Alias)
import Morley.Nettest
import qualified Unsafe as Unsafe
import Util.Named

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin (Parameter(..), Storage, mkPermitHash)

import Lorentz.Contracts.Test.Common
  (OriginationParams(..), addAccount, defaultOriginationParams, mkInitialStorage)


permitScenario :: T.Contract (ToT Parameter) (ToT Storage) -> NettestScenario m
permitScenario stablecoinContract = uncapsNettest $ do
  comment "-- Permits tests --"
  comment "Creating accounts"
  (owner1Alias, owner1) <- createUser "nettestOwner1"
  (owner2Alias, owner2) <- createUser "nettestOwner2"
  (pauserAlias, pauser) <- createUser "nettestPauser"
  (masterMinterAlias, masterMinter) <- createUser "nettestMasterMinter"

  -- user with no privileges
  (_, user1) <- createUser "Steve"

  comment "Originating contracts"
  let storage = Unsafe.fromJust $ mkInitialStorage $
        addAccount (owner1 , ([], 1000)) $
          defaultOriginationParams
            { opOwner = owner1
            , opPauser = pauser
            , opMasterMinter = masterMinter
            }
  contract <- TAddress @Parameter <$> originateUntypedSimple
    "nettest.Stablecoin"
    (untypeValue (toVal storage))
    (convertContract stablecoinContract)
  permitCounterConsumer <- originateSimple "permitCounterConsumer" [] (contractConsumer @Natural)
  chainId <- getChainId

  let
    getCounter :: MonadNettest caps base m => m Natural
    getCounter = do
      call contract (Call @"Get_counter") (mkView () permitCounterConsumer)
      counters <- fromVal @[Natural] <$> getStorage (addressResolved permitCounterConsumer)
      pure $ Unsafe.fromJust $ safeHead counters

    issuePermit :: MonadNettest caps base m => Alias -> Parameter -> m ()
    issuePermit alias param = do
      counter <- getCounter
      let permitHash = mkPermitHash param
      let bytes = lPackValue ((contract, chainId), (counter, permitHash))
      sig <- signBytes bytes alias
      pk <- getPublicKey (AddressAlias alias)
      call contract (Call @"Permit") (pk, (sig, permitHash))

  comment "Issue a permit for Pause"
  issuePermit pauserAlias Pause
  callFrom (addressResolved user1) contract (Call @"Pause") ()

  comment "Issue a permit for Unpause"
  issuePermit pauserAlias Unpause
  callFrom (addressResolved user1) contract (Call @"Unpause") ()

  comment "Issue a permit for Revoke"
  issuePermit pauserAlias Pause
  let revokeParam = [(mkPermitHash Pause, pauser)]
  issuePermit pauserAlias (Revoke revokeParam)
  callFrom (addressResolved user1) contract (Call @"Revoke") revokeParam
  callFrom (addressResolved user1) contract (Call @"Pause") ()
    `expectFailure` NettestFailedWith contract [mt|NOT_PAUSER|]

  comment "Issue a permit for Transfer"
  let transferParam = [ (#from_ .! owner1, #txs .! [(#to_ .! user1, (#token_id .! 0, #amount .! 1))]) ]
  issuePermit owner1Alias (Call_FA2 $ FA2.Transfer transferParam)
  callFrom (addressResolved user1) contract (Call @"Transfer") transferParam

  comment "Issue a permit for Update_operators"
  let param = [ FA2.Add_operator (#owner .! owner1, #operator user1 ) ]
  issuePermit owner1Alias (Call_FA2 $ FA2.Update_operators param)
  callFrom (addressResolved user1) contract (Call @"Update_operators") param

  comment "Issue a permit for Configure_minter"
  let configureMinterParam =
        ( #minter .! user1
        , ( #current_minting_allowance .! Nothing
          , #new_minting_allowance .! 30
          ))
  issuePermit masterMinterAlias (Configure_minter configureMinterParam)
  callFrom (addressResolved user1) contract (Call @"Configure_minter") configureMinterParam

  comment "Issue a permit for Remove_minter"
  let removeMinterParam = user1
  issuePermit masterMinterAlias (Remove_minter removeMinterParam)
  callFrom (addressResolved user1) contract (Call @"Remove_minter") removeMinterParam

  comment "Issue a permit for Set_transferlist"
  issuePermit owner1Alias (Set_transferlist Nothing)
  callFrom (addressResolved user1) contract (Call @"Set_transferlist") Nothing

  comment "Issue a permit for Change_pauser"
  issuePermit owner1Alias (Change_pauser owner2)
  callFrom (addressResolved user1) contract (Call @"Change_pauser") owner2

  comment "Issue a permit for Change_master_minter"
  issuePermit owner1Alias (Change_master_minter owner2)
  callFrom (addressResolved user1) contract (Call @"Change_master_minter") owner2

  comment "Issue a permit for Transfer_ownership"
  issuePermit owner1Alias (Transfer_ownership owner2)
  callFrom (addressResolved user1) contract (Call @"Transfer_ownership") owner2

  comment "Issue a permit for Accept_ownership"
  issuePermit owner2Alias Accept_ownership
  callFrom (addressResolved user1) contract (Call @"Accept_ownership") ()

  where
    createUser :: MonadNettest caps base m => Alias -> m (Alias, Address)
    createUser alias = do
      addr <- newAddress alias
      -- Note: `newAddress` prepends the alias with the prefix from the
      -- `MorleyClientConfig`.
      -- So we use `getAlias` to get the actual alias (prefix included)
      -- associated with this address.
      actualAlias <- getAlias (AddressResolved addr)
      pure (actualAlias, addr)

    getChainId :: MonadNettest caps base m => m ChainId
    getChainId = do
      addr <- originateSimple "chainIdContract" "" chainIdContract
      call addr CallDefault ()

      storage <- fromVal @ByteString <$> getStorage @(ToT ByteString) (addressResolved addr)
      case lUnpackValue @ChainId storage of
        Left err -> error $ "Invalid ChainId retrieved from 'chainIdContract':\n" <> pretty err
        Right chainId -> pure chainId


-- | A contract that stores the current ChainId in its storage.
--
-- NOTE: `compileLorentzContract` requires the contract's parameter to have an instance for
-- `HasAnnotation`, and ChainId doesn't.
-- So, instead, we store it as a ByteString and pack/unpack as needed.
chainIdContract :: Contract () ByteString
chainIdContract = defaultContract $
  L.drop # L.chainId # L.pack # L.nil # L.pair
