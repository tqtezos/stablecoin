-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Tests for permit functionality of stablecoin smart-contract
module Lorentz.Contracts.Test.Permit
  ( test_Permit
  ) where

import Lorentz (Packed(..), lPackValue, mt, toTAddress)

import Test.Tasty (TestTree, testGroup)
import Time (sec)

import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Tezos.Crypto (Signature(..))
import Morley.Tezos.Crypto.Hash qualified as Hash
import Test.Cleveland
import Test.Morley.Metadata

import Lorentz.Contracts.Stablecoin hiding (metadataJSON, stablecoinContract)
import Lorentz.Contracts.Test.Common
import Lorentz.Contracts.Test.FA2 (fa2NotOperator, fa2NotOwner)
import Lorentz.Contracts.Test.Management
  (mgmNotContractOwner, mgmNotMasterMinter, mgmNotPauser, mgmNotPendingOwner)

-- NB: Some of these tests are too timing-sensitive to run on network successfully,
-- and otherwise they would take way too long. So for the time being we only run
-- these on the emulator.
testScenario' :: String -> Scenario PureM -> TestTree
testScenario' = testScenarioOnEmulator

mkPermit
  :: MonadCleveland caps m
  => ContractHandle Parameter Storage ()
  -> ImplicitAddress
  -> Natural
  -> Parameter
  -> m (PermitHash, ByteString, Signature)
mkPermit contractAddr signer counter param = do
  chainId <- getChainId
  let permitHash = mkPermitHash param
      Packed toSign = lPackValue ((toTAddress contractAddr, chainId), (counter, permitHash))
  sig <- signBytes toSign signer
  pure (permitHash, toSign, sig)

callPermit
  :: (HasCallStack, MonadCleveland caps m)
  => ContractHandle Parameter Storage ()
  -> ImplicitAddress
  -> Natural
  -> Parameter
  -> m PermitHash
callPermit contractAddr signer counter param = do
  pk <- getPublicKey signer
  (permitHash, _, sig) <- mkPermit contractAddr signer counter param
  transfer contractAddr $ calling (ep @"Permit") $ PermitParam pk sig permitHash
  pure permitHash

errExpiredPermit
  , errDupPermit
  , errNotPermitIssuer
  , errExpiryTooBig
  :: (HasCallStack, MonadCleveland caps m) => m () -> m ()
errExpiredPermit = expectFailedWith [mt|EXPIRED_PERMIT|]
errDupPermit = expectFailedWith [mt|DUP_PERMIT|]
errNotPermitIssuer = expectFailedWith [mt|NOT_PERMIT_ISSUER|]
errExpiryTooBig = expectFailedWith [mt|EXPIRY_TOO_BIG|]

errMissignedPermit :: (HasCallStack, MonadCleveland caps m) => ByteString -> m () -> m ()
errMissignedPermit signedBytes = expectFailedWith ([mt|MISSIGNED|], signedBytes)

-- | Assert that there are n permits left in the storage
assertPermitCount :: MonadCleveland caps m => ContractHandle Parameter Storage () -> Int -> m ()
assertPermitCount contractAddr expectedCount = do
  storage <- getStorage contractAddr
  count <- sum . map (length . upPermits) <$> getAllBigMapValues (sPermitsRPC storage)
  unless (count == expectedCount) $
    failure $
      "Expected there to be " <> show expectedCount
        <> " permits left in the storage, but there were "
        <> show count

test_Permit :: [TestTree]
test_Permit =
  [ testGroup
      "Permits"
      [ testScenario "The counter used to sign the permit must match the contract's counter" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          (permitHash, _signedBytes, sig) <- mkPermit stablecoinContract testPauser 999 Pause
          testPauserPK <- getPublicKey testPauser
          chainId <- getChainId
          let Packed expectedSignedBytes = lPackValue ((toAddress stablecoinContract, chainId), ((0 :: Natural), permitHash))
          withSender testPauser $
            errMissignedPermit expectedSignedBytes $
              transfer stablecoinContract $ calling (ep @"Permit") (PermitParam testPauserPK sig permitHash)
      , testScenario "The public key must match the private key used to sign the permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          wallet1PK <- getPublicKey wallet1
          (permitHash, signedBytes, sig) <- mkPermit stablecoinContract testPauser 0 Pause
          withSender testPauser $
            errMissignedPermit signedBytes $
              transfer stablecoinContract $ calling (ep @"Permit") (PermitParam wallet1PK sig permitHash)
      , testScenario "The permit can be sent to the contract by a user other than the signer" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $
            callPermit stablecoinContract testPauser 0 Pause

          withSender wallet2 $
            transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "Admins do not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)

          withSender testPauser $ do
            callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Pause") ()
            transfer stablecoinContract $ calling (ep @"Unpause") ()
          withSender wallet1 $
            transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "Counter is increased every time a permit is issued" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let issuePermit counter =
                void $ callPermit stablecoinContract testPauser counter Pause

          let consumePermitAndUnpause = do
                transfer stablecoinContract $ calling (ep @"Pause") ()
                withSender testPauser $ transfer stablecoinContract $ calling (ep @"Unpause") ()

          issuePermit 0 >> consumePermitAndUnpause
          issuePermit 1 >> consumePermitAndUnpause
          issuePermit 2 >> consumePermitAndUnpause
          issuePermit 3 >> consumePermitAndUnpause
      , testScenario "`Unpause` permit cannot be used to pause" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          -- More generally, we want to assert that if two entrypoints X and Y have the same
          -- parameter type (in this case, both `Pause` and `Unpause` are of type `Unit`),
          -- then a permit issued for X cannot be used to access entrypoint Y.
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet2 $ do
            callPermit stablecoinContract testPauser 0 Unpause
            mgmNotPauser $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "Permits expire after some time (set by the contract)" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          let originationParams = (defaultOriginationParams
                (#owner :! testOwner)
                (#pauser :! testPauser)
                (#masterMinter :! testMasterMinter))
                { opDefaultExpiry = 10 }
              defaultExpiry = opDefaultExpiry originationParams
          stablecoinContract <- originateStablecoin originationParams

          withSender testPauser $ do
            callPermit stablecoinContract testPauser 0 Pause
          withSender wallet1 $ do
            advanceTime (sec $ fromIntegralOverflowing defaultExpiry + 1)
            errExpiredPermit $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "Permits cannot be re-uploaded if they haven't expired" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            callPermit stablecoinContract testPauser 0 Pause
            callPermit stablecoinContract testPauser 1 Pause
              & void
              & errDupPermit
      , testScenario' "Permits can be re-uploaded after they've expired" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          let defaultExpiry = 1
          let originationParams = (defaultOriginationParams
                (#owner :! testOwner)
                (#pauser :! testPauser)
                (#masterMinter :! testMasterMinter))
                {opDefaultExpiry = defaultExpiry}
          stablecoinContract <- originateStablecoin originationParams
          withSender wallet1 $ do
            callPermit stablecoinContract testPauser 0 Pause
            -- Advance enough time for the permit to expire
            advanceTime $ sec 2
            -- Re-upload permit hash
            void $ callPermit stablecoinContract testPauser 1 Pause
            transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "Re-uploading an expired permit hash resets its `created_at`/`expiry` settings" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          let defaultExpiry = 5
          let originationParams = (defaultOriginationParams
                (#owner :! testOwner)
                (#pauser :! testPauser)
                (#masterMinter :! testMasterMinter))
                {opDefaultExpiry = defaultExpiry}
          stablecoinContract <- originateStablecoin originationParams
          withSender testPauser $ do
            hash <- callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 1 $ Just hash

            -- Advance enough time for the permit to expire
            advanceTime $ sec 2

            -- Re-upload permit hash
            void $ callPermit stablecoinContract testPauser 1 Pause

            -- If the permit's `created_at`/`expiry` settings were reset (as expected),
            -- we should be able to advance up to 5 seconds and consume the permit.
            advanceTime $ sec 3
          transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "When a permit is issued, the issuer's expired permits are purged" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          let originationParams = (defaultOriginationParams
                (#owner :! testOwner)
                (#pauser :! testPauser)
                (#masterMinter :! testMasterMinter))
                { opDefaultExpiry = 10 }
              defaultExpiry = opDefaultExpiry originationParams
          stablecoinContract <- originateStablecoin originationParams
          withSender testPauser $ do
            callPermit stablecoinContract testPauser 0 Pause
            advanceTime (sec $ fromIntegralOverflowing defaultExpiry + 1)
            callPermit stablecoinContract testPauser 1 Unpause
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Set_expiry"
      [ testScenario "does not fail when permit does not exist" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let hash = PermitHash (Hash.blake2b "abc")
          withSender testPauser $
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 1 $ Just hash
      , testScenario "does not allow too big expiry" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let hash = PermitHash (Hash.blake2b "abc")
          withSender testPauser $
            errExpiryTooBig $
              transfer stablecoinContract $
                calling (ep @"Set_expiry") (SetExpiryParam (toAddress testPauser) 31557600000 $ Just hash)
      , testScenario' "a user can set a default expiry for all permits signed with their secret key" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 1 $ Nothing
          withSender wallet1 $ do
            callPermit stablecoinContract testPauser 0 Pause
            advanceTime $ sec 2
            errExpiredPermit $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "a permit's expiry takes precedence over a user's default expiry" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            hash <- callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 5 $ Just hash
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 10 $ Nothing
          withSender wallet1 $ do
            advanceTime $ sec 6
            errExpiredPermit $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "overrides permit's previous expiry" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            hash <- callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 5 $ Just hash
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 3 $ Just hash
          withSender wallet1 $ do
            advanceTime $ sec 4
            errExpiredPermit $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "setting expiry of an expired permit does not prolong its life" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            hash <- callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 3 $ Just hash
            advanceTime $ sec 5
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 20 $ Just hash
          withSender wallet1 $ do
            advanceTime $ sec 10
            errExpiredPermit $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "can only be accessed by the issuer of the permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            pausePermitHash <- callPermit stablecoinContract testPauser 0 Pause
            errNotPermitIssuer $
              transfer stablecoinContract $
                calling (ep @"Set_expiry") (SetExpiryParam (toAddress testPauser) 10 $ Just pausePermitHash)
      , testScenario' "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          let defaultExpiry = 1
          let originationParams = (defaultOriginationParams
                (#owner :! testOwner)
                (#pauser :! testPauser)
                (#masterMinter :! testMasterMinter))
                {opDefaultExpiry = defaultExpiry}
          stablecoinContract <- originateStablecoin originationParams
          withSender wallet1 $ do
            pausePermitHash <- callPermit stablecoinContract testPauser 0 Pause
            callPermit stablecoinContract testPauser 1 $
              Set_expiry $ SetExpiryParam (toAddress testPauser) 5 $ Just pausePermitHash
            transfer stablecoinContract $ calling (ep @"Set_expiry") $
              SetExpiryParam (toAddress testPauser) 5 $ Just pausePermitHash
            advanceTime $ sec 3
            transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "should revoke the permit when set to zero" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            pausePermitHash <- callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Set_expiry") $
              SetExpiryParam (toAddress testPauser) 0 $ Just pausePermitHash
          mgmNotPauser $
            transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario' "overrides user's previous expiry" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 5 $ Nothing
            transfer stablecoinContract $ calling (ep @"Set_expiry") $ SetExpiryParam (toAddress testPauser) 3 $ Nothing
            callPermit stablecoinContract testPauser 0 Pause
          withSender wallet1 $ do
            advanceTime $ sec 4
            errExpiredPermit $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      ]
  , testGroup
      "GetDefaultExpiry"
      [ testScenarioOnEmulator "retrieves the contract's default expiry" $ scenarioEmulated do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          let originationParams = defaultOriginationParams
                (#owner :! testOwner)
                (#pauser :! testPauser)
                (#masterMinter :! testMasterMinter)
              defaultExpiry = opDefaultExpiry originationParams
          stablecoinContract <- originateStablecoin originationParams
          callOffChainView @Natural stablecoinContract "GetDefaultExpiry" NoParam @@== defaultExpiry
      ]
  , testGroup
      "GetCounter"
      [ testScenarioOnEmulator "retrieves the contract's current counter" $ scenarioEmulated do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)

          callOffChainView @Natural stablecoinContract "GetCounter" NoParam @@== 0
          callPermit stablecoinContract testPauser 0 Pause
          callOffChainView @Natural stablecoinContract "GetCounter" NoParam @@== 1
          callPermit stablecoinContract testPauser 1 Unpause
          callOffChainView @Natural stablecoinContract "GetCounter" NoParam @@== 2
      ]
  , testGroup
      "Pause"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            mgmNotPauser $
              transfer stablecoinContract $ calling (ep @"Pause") ()
            callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Pause") ()
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the pauser" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 Pause
            mgmNotPauser $
              transfer stablecoinContract $ calling (ep @"Pause") ()
      , testScenario "pauser does not consume 'pause' permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            callPermit stablecoinContract testPauser 0 Pause
            transfer stablecoinContract $ calling (ep @"Pause") ()
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Unpause"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            transfer stablecoinContract $ calling (ep @"Pause") ()
          withSender wallet1 $ do
            mgmNotPauser $
              transfer stablecoinContract $ calling (ep @"Unpause") ()
            callPermit stablecoinContract testPauser 0 Unpause
            transfer stablecoinContract $ calling (ep @"Unpause") ()
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the pauser" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $
            transfer stablecoinContract $ calling (ep @"Pause") ()

          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 Unpause
            mgmNotPauser $
              transfer stablecoinContract $ calling (ep @"Unpause") ()
      , testScenario "pauser does not consume 'unpause' permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testPauser $ do
            callPermit stablecoinContract testPauser 0 Unpause
            transfer stablecoinContract $ calling (ep @"Pause") ()
            transfer stablecoinContract $ calling (ep @"Unpause") ()
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Transfer_ownership"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
            callPermit stablecoinContract testOwner 0 (Transfer_ownership $ toAddress wallet2)
            transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the contract owner" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 (Transfer_ownership $ toAddress wallet2)
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
      , testScenario "contract owner does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $ do
            callPermit stablecoinContract testOwner 0 (Transfer_ownership $ toAddress wallet2)
            transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Accept_ownership"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $
            transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
          withSender wallet1 $ do
            mgmNotPendingOwner $
              transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
            callPermit stablecoinContract wallet2 0 Accept_ownership
            transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the pending owner" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $
            transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 Accept_ownership
            mgmNotPendingOwner $
              transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
      , testScenario "pending owner does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $
            transfer stablecoinContract $ calling (ep @"Transfer_ownership") $ toAddress wallet2
          withSender wallet2 $ do
            callPermit stablecoinContract wallet2 0 Accept_ownership
            transfer stablecoinContract $ calling (ep @"Accept_ownership") ()
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Change_master_minter"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet2
            callPermit stablecoinContract testOwner 0 (Change_master_minter $ toAddress wallet2)
            transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet2
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the contract owner" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 (Change_master_minter $ toAddress wallet2)
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet2
      , testScenario "contract owner does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $ do
            callPermit stablecoinContract testOwner 0 (Change_master_minter $ toAddress wallet2)
            transfer stablecoinContract $ calling (ep @"Change_master_minter") $ toAddress wallet2
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Change_pauser"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet2
            callPermit stablecoinContract testOwner 0 (Change_pauser $ toAddress wallet2)
            transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet2
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the contract owner" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 (Change_pauser $ toAddress wallet2)
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet2
      , testScenario "contract owner does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $ do
            callPermit stablecoinContract testOwner 0 (Change_pauser $ toAddress wallet2)
            transfer stablecoinContract $ calling (ep @"Change_pauser") $ toAddress wallet2
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Set_transferlist"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Set_transferlist") Nothing
            callPermit stablecoinContract testOwner 0 (Set_transferlist Nothing)
            transfer stablecoinContract $ calling (ep @"Set_transferlist") Nothing
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the contract owner" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 (Set_transferlist Nothing)
            mgmNotContractOwner $
              transfer stablecoinContract $ calling (ep @"Set_transferlist") Nothing
      , testScenario "contract owner does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          withSender testOwner $ do
            callPermit stablecoinContract testOwner 0 (Set_transferlist Nothing)
            transfer stablecoinContract $ calling (ep @"Set_transferlist") Nothing
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Configure_minter"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let param = ConfigureMinterParam (toAddress wallet1) Nothing 30
          withSender wallet1 $ do
            mgmNotMasterMinter $
              transfer stablecoinContract $ calling (ep @"Configure_minter") param
            callPermit stablecoinContract testMasterMinter 0 (Configure_minter param)
            transfer stablecoinContract $ calling (ep @"Configure_minter") param
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the master minter" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let param = ConfigureMinterParam (toAddress wallet1) Nothing 30
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 (Configure_minter param)
            mgmNotMasterMinter $
              transfer stablecoinContract $ calling (ep @"Configure_minter") param
      , testScenario "master minter does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let param = ConfigureMinterParam (toAddress wallet1) Nothing 30
          withSender testMasterMinter $ do
            callPermit stablecoinContract testMasterMinter 0 (Configure_minter param)
            transfer stablecoinContract $ calling (ep @"Configure_minter") param
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Remove_minter"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let minter = wallet2
          withSender testMasterMinter $
            transfer stablecoinContract $ calling (ep @"Configure_minter") $ ConfigureMinterParam (toAddress minter) Nothing 30

          withSender wallet1 $ do
            mgmNotMasterMinter $
              transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress minter
            callPermit stablecoinContract testMasterMinter 0 (Remove_minter $ toAddress minter)
            transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress minter
            assertPermitCount stablecoinContract 0
      , testScenario "cannot be accessed when permit is not signed by the master minter" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let minter = wallet2
          withSender wallet1 $ do
            callPermit stablecoinContract wallet1 0 (Remove_minter $ toAddress minter)
            mgmNotMasterMinter $
              transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress minter
      , testScenario "master minter does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let minter = wallet2
          withSender testMasterMinter $
            transfer stablecoinContract $ calling (ep @"Configure_minter") $ ConfigureMinterParam (toAddress minter) Nothing 30

          withSender testMasterMinter $ do
            callPermit stablecoinContract testMasterMinter 0 (Remove_minter $ toAddress minter)
            transfer stablecoinContract $ calling (ep @"Remove_minter") $ toAddress minter
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Transfer"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let transferParams =
                [ FA2.TransferItem (toAddress wallet2) []
                , FA2.TransferItem (toAddress wallet2) []
                ]

          withSender wallet1 $ do
            fa2NotOperator $
              transfer stablecoinContract $ calling (ep @"Transfer") transferParams
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Transfer transferParams)
            transfer stablecoinContract $ calling (ep @"Transfer") transferParams
            assertPermitCount stablecoinContract 0
      , testScenario "A user X cannot sign a permit allowing other users to transfer tokens from user Y's account" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          wallet3 <- newAddress "wallet3"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let transferParams =
                [ FA2.TransferItem (toAddress wallet2) []
                , FA2.TransferItem (toAddress wallet3) []
                ]

          withSender wallet1 $ do
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Transfer transferParams)
            fa2NotOperator $
              transfer stablecoinContract $ calling (ep @"Transfer") transferParams
      , testScenario "transferring from own account does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let transferParams = [FA2.TransferItem (toAddress wallet2) []]
          withSender wallet2 $ do
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Transfer transferParams)
            transfer stablecoinContract $ calling (ep @"Transfer") transferParams
            assertPermitCount stablecoinContract 1
      , testScenario "operators do not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          wallet3 <- newAddress "wallet3"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let transferParams = [FA2.TransferItem (toAddress wallet2) []]
          withSender wallet2 $ do
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Transfer transferParams)
            transfer stablecoinContract $
              calling (ep @"Update_operators")
                [FA2.AddOperator FA2.OperatorParam {opOwner = toAddress wallet2, opOperator = toAddress wallet3, opTokenId = FA2.theTokenId}]
          withSender wallet3 $ do
            transfer stablecoinContract $ calling (ep @"Transfer") transferParams
            assertPermitCount stablecoinContract 1
      ]
  , testGroup
      "Update_operators"
      [ testScenario "can be accessed via a permit" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          wallet3 <- newAddress "wallet3"
          wallet4 <- newAddress "wallet4"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let params =
                [ FA2.AddOperator FA2.OperatorParam {opOwner = toAddress wallet2, opOperator = toAddress wallet3, opTokenId = FA2.theTokenId}
                , FA2.RemoveOperator FA2.OperatorParam {opOwner = toAddress wallet2, opOperator = toAddress wallet4, opTokenId = FA2.theTokenId}
                ]

          withSender wallet1 $ do
            fa2NotOwner $
              transfer stablecoinContract $ calling (ep @"Update_operators") params
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Update_operators params)
            transfer stablecoinContract $ calling (ep @"Update_operators") params
            assertPermitCount stablecoinContract 0
      , testScenario "A user X cannot sign a permit allowing other users to modify user Y's operators" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet1 <- newAddress "wallet1"
          wallet2 <- newAddress "wallet2"
          wallet3 <- newAddress "wallet3"
          wallet4 <- newAddress "wallet4"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let params =
                [ FA2.AddOperator FA2.OperatorParam {opOwner = toAddress wallet2, opOperator = toAddress wallet3, opTokenId = FA2.theTokenId}
                , FA2.RemoveOperator FA2.OperatorParam {opOwner = toAddress wallet3, opOperator = toAddress wallet4, opTokenId = FA2.theTokenId}
                ]

          withSender wallet1 $ do
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Update_operators params)
            fa2NotOwner $
              transfer stablecoinContract $ calling (ep @"Update_operators") params
      , testScenario "updating own operators does not consume permits" $ scenario do
          testOwner <- newAddress "testOwner"
          testPauser <- newAddress "testPauser"
          testMasterMinter <- newAddress "testMasterMinter"
          wallet2 <- newAddress "wallet2"
          wallet3 <- newAddress "wallet3"
          stablecoinContract <- originateStablecoin $ defaultOriginationParams
            (#owner :! testOwner)
            (#pauser :! testPauser)
            (#masterMinter :! testMasterMinter)
          let params =
                [FA2.AddOperator FA2.OperatorParam {opOwner = toAddress wallet2, opOperator = toAddress wallet3, opTokenId = FA2.theTokenId}]

          withSender wallet2 $ do
            callPermit
              stablecoinContract
              wallet2
              0
              (Call_FA2 $ Update_operators params)
            transfer stablecoinContract $ calling (ep @"Update_operators") params
            assertPermitCount stablecoinContract 1
      ]
  ]
