-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

-- TODO: switch to `advanceTime` and remove this pragma when #350 is merged
-- https://gitlab.com/morley-framework/morley/-/issues/350
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- | Tests for permit functionality of stablecoin smart-contract

module Lorentz.Contracts.Test.Permit
  ( permitSpec
  ) where

import Test.Hspec (Spec, describe, it, specify)

import Lorentz (BigMap(..), TAddress, lPackValue, mt)
import Lorentz.Test
import Michelson.Runtime (ExecutorError)
import Michelson.Runtime.GState (GState(gsChainId), initGState)
import Morley.Metadata (ViewParam(..))
import Tezos.Address (Address)
import Tezos.Crypto (PublicKey, SecretKey(..), Signature(..))
import qualified Tezos.Crypto.Ed25519 as Ed25519
import qualified Tezos.Crypto.Hash as Hash
import qualified Tezos.Crypto.P256 as P256
import qualified Tezos.Crypto.Secp256k1 as Secp256k1
import Tezos.Crypto.Util (deterministic)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin hiding (metadataJSON, stablecoinContract)
import Lorentz.Contracts.Test.Common
import Lorentz.Contracts.Test.FA2 (fa2NotOperator, fa2NotOwner)
import Lorentz.Contracts.Test.Management
  (mgmNotContractOwner, mgmNotMasterMinter, mgmNotPauser, mgmNotPendingOwner)

sign :: SecretKey -> ByteString -> Signature
sign sk bs =
  case sk of
    SecretKeyEd25519 sk' -> SignatureEd25519 $ Ed25519.sign sk' bs
    SecretKeyP256 sk' -> SignatureP256 $ deterministic seed $ P256.sign sk' bs
    SecretKeySecp256k1 sk' -> SignatureSecp256k1 $ deterministic seed $ Secp256k1.sign sk' bs
  where
    seed = "abc"

mkPermit :: TAddress Parameter -> SecretKey -> Natural -> Parameter -> (PermitHash, ByteString, Signature)
mkPermit contractAddr sk counter param =
  let permitHash = mkPermitHash param
      toSign = lPackValue ((contractAddr, gsChainId initGState), (counter, permitHash))
      sig = sign sk toSign
  in  (permitHash, toSign, sig)

callPermit :: TAddress Parameter -> PublicKey -> SecretKey -> Natural -> Parameter -> IntegrationalScenarioM PermitHash
callPermit contractAddr pk sk counter param = do
  let (permitHash, _, sig) = mkPermit contractAddr sk counter param
  lCallEP contractAddr (Call @"Permit") $ PermitParam pk sig permitHash
  pure permitHash

errExpiredPermit, errDupPermit, errNotPermitIssuer, errExpiryTooBig :: ExecutorError -> IntegrationalScenario
errExpiredPermit = lExpectFailWith (== [mt|EXPIRED_PERMIT|])
errDupPermit = lExpectFailWith (== [mt|DUP_PERMIT|])
errNotPermitIssuer = lExpectFailWith (== [mt|NOT_PERMIT_ISSUER|])
errExpiryTooBig = lExpectFailWith (== [mt|EXPIRY_TOO_BIG|])

errMissignedPermit :: ByteString -> ExecutorError -> IntegrationalScenario
errMissignedPermit signedBytes = lExpectFailWith (== ([mt|MISSIGNED|], signedBytes))

-- | Assert that there are n permits left in the storage
assertPermitCount :: TAddress Parameter -> Int -> IntegrationalScenario
assertPermitCount contractAddr expectedCount =
  lExpectStorage contractAddr $ \storage ->
    let count = permitCount (sPermits storage)
    in  if count == expectedCount
          then Right ()
          else Left $ CustomTestError $
                "Expected there to be "
                <> show expectedCount
                <> " permits left in the storage, but there were "
                <> show count
  where
    permitCount :: BigMap Address UserPermits -> Int
    permitCount (BigMap permits) =
      sum $
        permits <&> \userPermits -> length (upPermits userPermits)

permitSpec :: OriginationFn Parameter -> Spec
permitSpec originate = do
  describe "Permits" $ do
    specify "The counter used to sign the permit must match the contract's counter" $
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          withSender testPauser $
            let
              (permitHash, _signedBytes, sig) = mkPermit stablecoinContract testPauserSK 999 Pause
              expectedSignedBytes = lPackValue ((stablecoinContract, gsChainId initGState), (0 :: Natural, permitHash))
            in
              lCallEP stablecoinContract (Call @"Permit") (PermitParam testPauserPK sig permitHash)
                `catchExpectedError` errMissignedPermit expectedSignedBytes

    specify "The public key must match the private key used to sign the permit" $
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          withSender testPauser $
            let
              (permitHash, signedBytes, sig) = mkPermit stablecoinContract testPauserSK 0 Pause
            in
              lCallEP stablecoinContract (Call @"Permit") (PermitParam wallet1PK sig permitHash)
                `catchExpectedError` errMissignedPermit signedBytes

    specify "The permit can be sent to the contract by a user other than the signer" $
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          withSender wallet1 $
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause

          withSender wallet2 $
            lCallEP stablecoinContract (Call @"Pause") ()

    specify "Admins do not consume permits" $
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do

          withSender testPauser $ do
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            lCallEP stablecoinContract (Call @"Pause") ()
            lCallEP stablecoinContract (Call @"Unpause") ()
          withSender wallet1 $
            lCallEP stablecoinContract (Call @"Pause") ()

    specify "Counter is increased every time a permit is issued" $ do
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          let issuePermit counter =
                void $ callPermit stablecoinContract testPauserPK testPauserSK counter Pause

          let consumePermitAndUnpause = do
                lCallEP stablecoinContract (Call @"Pause") ()
                withSender testPauser $ lCallEP stablecoinContract (Call @"Unpause") ()

          issuePermit 0 >> consumePermitAndUnpause
          issuePermit 1 >> consumePermitAndUnpause
          issuePermit 2 >> consumePermitAndUnpause
          issuePermit 3 >> consumePermitAndUnpause

    specify "`Unpause` permit cannot be used to pause" $
      -- More generally, we want to assert that if two entrypoints X and Y have the same
      -- parameter type (in this case, both `Pause` and `Unpause` are of type `Unit`),
      -- then a permit issued for X cannot be used to access entrypoint Y.
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          withSender wallet2 $ do
            callPermit stablecoinContract testPauserPK testPauserSK 0 Unpause
            lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` mgmNotPauser

    specify "Permits expire after some time (set by the contract)" $ do
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          let defaultExpiry = opDefaultExpiry defaultOriginationParams

          withSender testPauser $ do
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
          withSender wallet1 $ do
            rewindTime (fromIntegral defaultExpiry + 1)
            lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` errExpiredPermit

    specify "Permits cannot be re-uploaded if they haven't expired" $ do
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          withSender wallet1 $ do
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            callPermit stablecoinContract testPauserPK testPauserSK 1 Pause `catchExpectedError` errDupPermit

    specify "Permits can be re-uploaded after they've expired" $ do
      integrationalTestExpectation $ do
        let defaultExpiry = 1
        let originationParams = defaultOriginationParams { opDefaultExpiry = defaultExpiry }
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender wallet1 $ do
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            -- Advance enough time for the permit to expire
            rewindTime 2
            -- Re-upload permit hash
            void $ callPermit stablecoinContract testPauserPK testPauserSK 1 Pause
            lCallEP stablecoinContract (Call @"Pause") ()


    specify "Re-uploading an expired permit hash resets its `created_at`/`expiry` settings" $ do
      integrationalTestExpectation $ do
        let defaultExpiry = 5
        let originationParams = defaultOriginationParams { opDefaultExpiry = defaultExpiry }
        withOriginated originate originationParams $ \stablecoinContract -> do
          withSender testPauser $ do
            hash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 1 $ Just hash

            -- Advance enough time for the permit to expire
            rewindTime 2

            -- Re-upload permit hash
            void $ callPermit stablecoinContract testPauserPK testPauserSK 1 Pause

            -- If the permit's `created_at`/`expiry` settings were reset (as expected),
            -- we should be able to advance up to 5 seconds and consume the permit.
            rewindTime 3
          lCallEP stablecoinContract (Call @"Pause") ()

    specify "When a permit is issued, the issuer's expired permits are purged" $
      integrationalTestExpectation $ do
        withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
          let defaultExpiry = opDefaultExpiry defaultOriginationParams
          withSender testPauser $ do
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            rewindTime (fromIntegral defaultExpiry + 1)
            callPermit stablecoinContract testPauserPK testPauserSK 1 Unpause
            assertPermitCount stablecoinContract 1

    describe "Set_expiry" $ do
      it "does not fail when permit does not exist" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let hash = PermitHash (Hash.blake2b "abc")
            withSender testPauser $
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 1 $ Just hash

      it "does not allow too big expiry" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let hash = PermitHash (Hash.blake2b "abc")
            withSender testPauser $
              lCallEP stablecoinContract (Call @"Set_expiry")
                (SetExpiryParam testPauser 31557600000 $ Just hash) `catchExpectedError` errExpiryTooBig

      it "a user can set a default expiry for all permits signed with their secret key" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 1 $ Nothing
            withSender wallet1 $ do
              callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              rewindTime 2
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` errExpiredPermit

      it "a permit's expiry takes precedence over a user's default expiry" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              hash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 5 $ Just hash
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 10 $ Nothing
            withSender wallet1 $ do
              rewindTime 6
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` errExpiredPermit

      it "overrides permit's previous expiry" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              hash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 5 $ Just hash
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 3 $ Just hash
            withSender wallet1 $ do
              rewindTime 4
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` errExpiredPermit

      it "setting expiry of an expired permit does not prolong its life" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              hash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 3 $ Just hash
              rewindTime 5
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 20 $ Just hash
            withSender wallet1 $ do
              rewindTime 10
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` errExpiredPermit

      it "can only be accessed by the issuer of the permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              pausePermitHash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract
                (Call @"Set_expiry") (SetExpiryParam testPauser 10 $ Just pausePermitHash)
                `catchExpectedError` errNotPermitIssuer

      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          let defaultExpiry = 1
          let originationParams = defaultOriginationParams { opDefaultExpiry = defaultExpiry }
          withOriginated originate originationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              pausePermitHash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              callPermit stablecoinContract testPauserPK testPauserSK 1 $
                Set_expiry $ SetExpiryParam testPauser 5 $ Just pausePermitHash
              lCallEP stablecoinContract (Call @"Set_expiry") $
                SetExpiryParam testPauser 5  $ Just pausePermitHash
              rewindTime 3
              lCallEP stablecoinContract (Call @"Pause") ()

      it "should revoke the permit when set to zero" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              pausePermitHash <- callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract (Call @"Set_expiry") $
                SetExpiryParam testPauser 0  $ Just pausePermitHash
            lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` mgmNotPauser

      it "overrides user's previous expiry" $ do
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 5 $ Nothing
              lCallEP stablecoinContract (Call @"Set_expiry") $ SetExpiryParam testPauser 3 $ Nothing
              callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            withSender wallet1 $ do
              rewindTime 4
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` errExpiredPermit

    describe "GetDefaultExpiry" $
      it "retrieves the contract's default expiry" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let defaultExpiry = opDefaultExpiry defaultOriginationParams
            checkView stablecoinContract "GetDefaultExpiry" NoParam defaultExpiry

    describe "GetCounter" $
      it "retrieves the contract's current counter" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do

            checkView stablecoinContract "GetCounter" NoParam (0 :: Natural)
            callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
            checkView stablecoinContract "GetCounter" NoParam (1 :: Natural)
            callPermit stablecoinContract testPauserPK testPauserSK 1 Unpause
            checkView stablecoinContract "GetCounter" NoParam (2 :: Natural)

    describe "Pause" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` mgmNotPauser
              callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract (Call @"Pause") ()
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the pauser" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 Pause
              lCallEP stablecoinContract (Call @"Pause") () `catchExpectedError` mgmNotPauser

      specify "pauser does not consume 'pause' permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              callPermit stablecoinContract testPauserPK testPauserSK 0 Pause
              lCallEP stablecoinContract (Call @"Pause") ()
              assertPermitCount stablecoinContract 1

    describe "Unpause" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              lCallEP stablecoinContract (Call @"Pause") ()
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Unpause") () `catchExpectedError` mgmNotPauser
              callPermit stablecoinContract testPauserPK testPauserSK 0 Unpause
              lCallEP stablecoinContract (Call @"Unpause") ()
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the pauser" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $
              lCallEP stablecoinContract (Call @"Pause") ()

            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 Unpause
              lCallEP stablecoinContract (Call @"Unpause") () `catchExpectedError` mgmNotPauser

      specify "pauser does not consume 'unpause' permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testPauser $ do
              callPermit stablecoinContract testPauserPK testPauserSK 0 Unpause
              lCallEP stablecoinContract (Call @"Pause") ()
              lCallEP stablecoinContract (Call @"Unpause") ()
              assertPermitCount stablecoinContract 1

    describe "Transfer_ownership" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2 `catchExpectedError` mgmNotContractOwner
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Transfer_ownership wallet2)
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the contract owner" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 (Transfer_ownership wallet2)
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2 `catchExpectedError` mgmNotContractOwner

      specify "contract owner does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $ do
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Transfer_ownership wallet2)
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
              assertPermitCount stablecoinContract 1

    describe "Accept_ownership" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Accept_ownership") () `catchExpectedError` mgmNotPendingOwner
              callPermit stablecoinContract wallet2PK wallet2SK 0 Accept_ownership
              lCallEP stablecoinContract (Call @"Accept_ownership") ()
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the pending owner" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 Accept_ownership
              lCallEP stablecoinContract (Call @"Accept_ownership") () `catchExpectedError` mgmNotPendingOwner

      specify "pending owner does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $
              lCallEP stablecoinContract (Call @"Transfer_ownership") wallet2
            withSender wallet2 $ do
              callPermit stablecoinContract wallet2PK wallet2SK 0 Accept_ownership
              lCallEP stablecoinContract (Call @"Accept_ownership") ()
              assertPermitCount stablecoinContract 1

    describe "Change_master_minter" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Change_master_minter") wallet2 `catchExpectedError` mgmNotContractOwner
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Change_master_minter wallet2)
              lCallEP stablecoinContract (Call @"Change_master_minter") wallet2
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the contract owner" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 (Change_master_minter wallet2)
              lCallEP stablecoinContract (Call @"Change_master_minter") wallet2 `catchExpectedError` mgmNotContractOwner

      specify "contract owner does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $ do
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Change_master_minter wallet2)
              lCallEP stablecoinContract (Call @"Change_master_minter") wallet2
              assertPermitCount stablecoinContract 1

    describe "Change_pauser" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Change_pauser") wallet2 `catchExpectedError` mgmNotContractOwner
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Change_pauser wallet2)
              lCallEP stablecoinContract (Call @"Change_pauser") wallet2
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the contract owner" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 (Change_pauser wallet2)
              lCallEP stablecoinContract (Call @"Change_pauser") wallet2 `catchExpectedError` mgmNotContractOwner

      specify "contract owner does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $ do
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Change_pauser wallet2)
              lCallEP stablecoinContract (Call @"Change_pauser") wallet2
              assertPermitCount stablecoinContract 1

    describe "Set_transferlist" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Set_transferlist") Nothing `catchExpectedError` mgmNotContractOwner
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Set_transferlist Nothing)
              lCallEP stablecoinContract (Call @"Set_transferlist") Nothing
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the contract owner" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 (Set_transferlist Nothing)
              lCallEP stablecoinContract (Call @"Set_transferlist") Nothing `catchExpectedError` mgmNotContractOwner

      specify "contract owner does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testOwner $ do
              callPermit stablecoinContract testOwnerPK testOwnerSK 0 (Set_transferlist Nothing)
              lCallEP stablecoinContract (Call @"Set_transferlist") Nothing
              assertPermitCount stablecoinContract 1

    describe "Configure_minter" $ do
      let param = ConfigureMinterParam wallet1 Nothing 30

      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Configure_minter") param `catchExpectedError` mgmNotMasterMinter
              callPermit stablecoinContract testMasterMinterPK testMasterMinterSK 0 (Configure_minter param)
              lCallEP stablecoinContract (Call @"Configure_minter") param
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the master minter" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 (Configure_minter param)
              lCallEP stablecoinContract (Call @"Configure_minter") param `catchExpectedError` mgmNotMasterMinter

      specify "master minter does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testMasterMinter $ do
              callPermit stablecoinContract testMasterMinterPK testMasterMinterSK 0 (Configure_minter param)
              lCallEP stablecoinContract (Call @"Configure_minter") param
              assertPermitCount stablecoinContract 1

    describe "Remove_minter" $ do
      let minter = wallet2

      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testMasterMinter $
              lCallEP stablecoinContract (Call @"Configure_minter") $ ConfigureMinterParam minter Nothing 30

            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Remove_minter") minter `catchExpectedError` mgmNotMasterMinter
              callPermit stablecoinContract testMasterMinterPK testMasterMinterSK 0 (Remove_minter minter)
              lCallEP stablecoinContract (Call @"Remove_minter") minter
              assertPermitCount stablecoinContract 0

      it "cannot be accessed when permit is not signed by the master minter" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender wallet1 $ do
              callPermit stablecoinContract wallet1PK wallet1SK 0 (Remove_minter minter)
              lCallEP stablecoinContract (Call @"Remove_minter") minter `catchExpectedError` mgmNotMasterMinter

      specify "master minter does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            withSender testMasterMinter $
              lCallEP stablecoinContract (Call @"Configure_minter") $ ConfigureMinterParam minter Nothing 30

            withSender testMasterMinter $ do
              callPermit stablecoinContract testMasterMinterPK testMasterMinterSK 0 (Remove_minter minter)
              lCallEP stablecoinContract (Call @"Remove_minter") minter
              assertPermitCount stablecoinContract 1

    describe "Transfer" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let transferParams =
                  [ FA2.TransferItem wallet2 []
                  , FA2.TransferItem wallet2 []
                  ]

            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Transfer") transferParams `catchExpectedError` fa2NotOperator
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Transfer transferParams)
              lCallEP stablecoinContract (Call @"Transfer") transferParams
              assertPermitCount stablecoinContract 0

      specify "A user X cannot sign a permit allowing other users to transfer tokens from user Y's account" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let transferParams =
                  [ FA2.TransferItem wallet2 []
                  , FA2.TransferItem wallet3 []
                  ]

            withSender wallet1 $ do
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Transfer transferParams)
              lCallEP stablecoinContract (Call @"Transfer") transferParams `catchExpectedError` fa2NotOperator

      specify "transferring from own account does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let transferParams = [ FA2.TransferItem wallet2 [] ]
            withSender wallet2 $ do
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Transfer transferParams)
              lCallEP stablecoinContract (Call @"Transfer") transferParams
              assertPermitCount stablecoinContract 1

      specify "operators do not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let transferParams = [ FA2.TransferItem wallet2 [] ]
            withSender wallet2 $ do
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Transfer transferParams)
              lCallEP stablecoinContract (Call @"Update_operators")
                [FA2.AddOperator FA2.OperatorParam { opOwner = wallet2, opOperator = wallet3, opTokenId = FA2.theTokenId }]
            withSender wallet3 $ do
              lCallEP stablecoinContract (Call @"Transfer") transferParams
              assertPermitCount stablecoinContract 1

    describe "Update_operators" $ do
      it "can be accessed via a permit" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let params =
                  [ FA2.AddOperator FA2.OperatorParam { opOwner = wallet2, opOperator = wallet3, opTokenId = FA2.theTokenId }
                  , FA2.RemoveOperator FA2.OperatorParam { opOwner = wallet2, opOperator = wallet4, opTokenId = FA2.theTokenId }
                  ]

            withSender wallet1 $ do
              lCallEP stablecoinContract (Call @"Update_operators") params `catchExpectedError` fa2NotOwner
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Update_operators params)
              lCallEP stablecoinContract (Call @"Update_operators") params
              assertPermitCount stablecoinContract 0

      specify "A user X cannot sign a permit allowing other users to modify user Y's operators" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let params =
                  [ FA2.AddOperator FA2.OperatorParam { opOwner = wallet2, opOperator = wallet3, opTokenId = FA2.theTokenId }
                  , FA2.RemoveOperator FA2.OperatorParam { opOwner = wallet3, opOperator = wallet4, opTokenId = FA2.theTokenId }
                  ]

            withSender wallet1 $ do
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Update_operators params)
              lCallEP stablecoinContract (Call @"Update_operators") params `catchExpectedError` fa2NotOwner

      specify "updating own operators does not consume permits" $
        integrationalTestExpectation $ do
          withOriginated originate defaultOriginationParams $ \stablecoinContract -> do
            let params =
                  [ FA2.AddOperator FA2.OperatorParam { opOwner = wallet2, opOperator = wallet3, opTokenId = FA2.theTokenId } ]

            withSender wallet2 $ do
              callPermit stablecoinContract wallet2PK wallet2SK 0
                (Call_FA2 $ FA2.Update_operators params)
              lCallEP stablecoinContract (Call @"Update_operators") params
              assertPermitCount stablecoinContract 1
