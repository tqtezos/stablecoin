-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

{-# LANGUAGE InstanceSigs #-}

module Stablecoin.Client.Cleveland.Caps
  ( StablecoinScenario
  , MonadStablecoin
  , runStablecoinClient
  , stablecoinCapImpl
  -- * Operations
  , deploy
  , transfer
  , getBalanceOf
  , updateOperators
  , isOperator
  , pause
  , unpause
  , configureMinter
  , removeMinter
  , mint
  , burn
  , transferOwnership
  , acceptOwnership
  , changeMasterMinter
  , changePauser
  , setTransferlist
  , getBalance
  , getPaused
  , getContractOwner
  , getPendingContractOwner
  , getMasterMinter
  , getPauser
  , getTransferlist
  , getMintingAllowance
  , getTokenMetadata
  , assertEq
  , revealKeyUnlessRevealed
  ) where

import Control.Exception.Uncaught (displayUncaughtException)
import qualified Monad.Capabilities as Caps
import Morley.Client (Alias, MorleyClientConfig, disableAlphanetWarning)
import Morley.Nettest (AddressOrAlias, MorleyClientEnv, NettestImpl(..), nettestImplClient)
import Morley.Nettest.Abstract (initSender)
import Morley.Nettest.Caps (SenderCap, implActionToCaps, nettestCapImpl, senderCapImpl)
import Tezos.Address (Address)
import Tezos.Core (Mutez)
import Util.Named ((:!))

import Stablecoin.Client (AddressAndAlias(..), InitialStorageData(..), UpdateOperatorData)
import Stablecoin.Client.Cleveland.StablecoinImpl (StablecoinImpl(..), stablecoinImplClient)

type StablecoinScenario m a = Monad m => Caps.CapsT '[ NettestImpl, StablecoinImpl, SenderCap ] m a

type MonadStablecoin caps base m =
  (Monad base, m ~ Caps.CapsT caps base, Caps.HasCaps '[ StablecoinImpl, SenderCap ] caps)

runStablecoinClient :: MorleyClientConfig -> MorleyClientEnv -> StablecoinScenario IO () -> IO ()
runStablecoinClient conf env scenario =
  displayUncaughtException $ do
    disableAlphanetWarning
    uncapsStablecoin scenario (stablecoinImplClient conf env) (nettestImplClient env)
  where
    uncapsStablecoin :: forall m a. Monad m => StablecoinScenario m a -> StablecoinImpl m -> NettestImpl m -> m a
    uncapsStablecoin action stablecoinImpl nettestImpl =
      runReaderT action $
        Caps.buildCaps $
          Caps.AddCap (nettestCapImpl nettestImpl) $
          Caps.AddCap (stablecoinCapImpl stablecoinImpl) $
          Caps.AddCap (senderCapImpl initSender) $
          Caps.BaseCaps Caps.emptyCaps

stablecoinCapImpl :: Monad m => StablecoinImpl m -> Caps.CapImpl StablecoinImpl '[] m
stablecoinCapImpl stablecoinImpl = Caps.CapImpl $ StablecoinImpl
  { siDeploy = lift ... siDeploy stablecoinImpl
  , siTransfer = lift ... siTransfer stablecoinImpl
  , siGetBalanceOf = lift ... siGetBalanceOf stablecoinImpl
  , siUpdateOperators = lift ... siUpdateOperators stablecoinImpl
  , siIsOperator = lift ... siIsOperator stablecoinImpl
  , siPause = lift ... siPause stablecoinImpl
  , siUnpause = lift ... siUnpause stablecoinImpl
  , siConfigureMinter = lift ... siConfigureMinter stablecoinImpl
  , siRemoveMinter = lift ... siRemoveMinter stablecoinImpl
  , siMint = lift ... siMint stablecoinImpl
  , siBurn = lift ... siBurn stablecoinImpl
  , siTransferOwnership = lift ... siTransferOwnership stablecoinImpl
  , siAcceptOwnership = lift ... siAcceptOwnership stablecoinImpl
  , siChangeMasterMinter = lift ... siChangeMasterMinter stablecoinImpl
  , siChangePauser = lift ... siChangePauser stablecoinImpl
  , siSetTransferlist = lift ... siSetTransferlist stablecoinImpl
  , siGetBalance = lift ... siGetBalance stablecoinImpl
  , siGetPaused = lift ... siGetPaused stablecoinImpl
  , siGetContractOwner = lift ... siGetContractOwner stablecoinImpl
  , siGetPendingContractOwner = lift ... siGetPendingContractOwner stablecoinImpl
  , siGetMasterMinter = lift ... siGetMasterMinter stablecoinImpl
  , siGetPauser = lift ... siGetPauser stablecoinImpl
  , siGetTransferlist = lift ... siGetTransferlist stablecoinImpl
  , siGetMintingAllowance = lift ... siGetMintingAllowance stablecoinImpl
  , siGetTokenMetadata = lift ... siGetTokenMetadata stablecoinImpl
  , siAssertEq = lift ... siAssertEq stablecoinImpl
  , siRevealKeyUnlessRevealed = lift ... siRevealKeyUnlessRevealed stablecoinImpl
  }

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

deploy :: MonadStablecoin caps base m => "sender" :! AddressOrAlias -> InitialStorageData AddressOrAlias -> m Address
deploy = implActionToCaps siDeploy

transfer
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> Natural -> m ()
transfer = implActionToCaps siTransfer

getBalanceOf
  :: MonadStablecoin caps base m
  => "contract" :! AddressOrAlias
  -> AddressOrAlias -> m Natural
getBalanceOf = implActionToCaps siGetBalanceOf

updateOperators
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> NonEmpty UpdateOperatorData -> m ()
updateOperators = implActionToCaps siUpdateOperators

isOperator
  :: MonadStablecoin caps base m
  => "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> m Bool
isOperator = implActionToCaps siIsOperator

pause :: MonadStablecoin caps base m => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> m ()
pause = implActionToCaps siPause

unpause :: MonadStablecoin caps base m => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> m ()
unpause = implActionToCaps siUnpause

configureMinter
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> Maybe Natural -> Natural -> m ()
configureMinter = implActionToCaps siConfigureMinter

removeMinter
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
removeMinter = implActionToCaps siRemoveMinter

mint
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> Natural -> m ()
mint = implActionToCaps siMint

burn
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> NonEmpty Natural -> m ()
burn = implActionToCaps siBurn

transferOwnership
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
transferOwnership = implActionToCaps siTransferOwnership

acceptOwnership
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> m ()
acceptOwnership = implActionToCaps siAcceptOwnership

changeMasterMinter
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
changeMasterMinter = implActionToCaps siChangeMasterMinter

changePauser
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
changePauser = implActionToCaps siChangePauser

setTransferlist
  :: MonadStablecoin caps base m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> Maybe AddressOrAlias -> m ()
setTransferlist = implActionToCaps siSetTransferlist

getBalance :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m Mutez
getBalance = implActionToCaps siGetBalance

getPaused :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m Bool
getPaused = implActionToCaps siGetPaused

getContractOwner :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m AddressAndAlias
getContractOwner = implActionToCaps siGetContractOwner

getPendingContractOwner :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m (Maybe AddressAndAlias)
getPendingContractOwner = implActionToCaps siGetPendingContractOwner

getMasterMinter :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m AddressAndAlias
getMasterMinter = implActionToCaps siGetMasterMinter

getPauser :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m AddressAndAlias
getPauser = implActionToCaps siGetPauser

getTransferlist :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> m (Maybe AddressAndAlias)
getTransferlist = implActionToCaps siGetTransferlist

getMintingAllowance :: MonadStablecoin caps base m => "contract" :! AddressOrAlias -> AddressOrAlias -> m Natural
getMintingAllowance = implActionToCaps siGetMintingAllowance

getTokenMetadata
  :: MonadStablecoin caps base m
  => "contract" :! AddressOrAlias
  -> m ("symbol" :! Text, "name" :! Text, "decimals" :! Natural)
getTokenMetadata = implActionToCaps siGetTokenMetadata

assertEq :: MonadStablecoin caps base m => (Eq a, Show a) => a -> a -> m ()
assertEq = implActionToCaps siAssertEq

revealKeyUnlessRevealed :: MonadStablecoin caps base m => Alias -> m ()
revealKeyUnlessRevealed = implActionToCaps siRevealKeyUnlessRevealed
