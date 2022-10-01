-- SPDX-FileCopyrightText: 2021 Oxhead Alpha
-- SPDX-License-Identifier: MIT

{-# LANGUAGE InstanceSigs #-}

module Stablecoin.Client.Cleveland.Caps
  ( StablecoinT
  , MonadStablecoin
  , StablecoinCaps(..)
  , runStablecoinClient
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
import Control.Lens (makeLensesFor)
import Fmt (Buildable(..))

import Morley.Client (disableAlphanetWarning)
import Morley.Tezos.Address (ContractAddress, ImplicitAddress, L1Address)
import Morley.Tezos.Core (Mutez)
import Morley.Util.Named ((:!))
import Test.Cleveland (NetworkEnv(..))
import Test.Cleveland.Internal.Abstract
  (ClevelandCaps(..), DefaultAliasCounter(..), HasClevelandCaps(..), MonadCleveland, Moneybag(..),
  Sender(..))
import Test.Cleveland.Internal.Client
  (ClientM(..), ClientState(..), networkMiscImpl, networkOpsImpl, setupMoneybagAddress)

import Stablecoin.Client (AddressAndAlias(..), UpdateOperatorData)
import Stablecoin.Client.Cleveland.StablecoinImpl (StablecoinImpl(..), stablecoinImplClient)
import Stablecoin.Client.Contract (InitialStorageOptions(..))

data StablecoinCaps m = StablecoinCaps
  { scCleveland :: ClevelandCaps m
  , scStablecoin :: StablecoinImpl m
  }

type StablecoinT m a = Monad m => ReaderT (StablecoinCaps m) m a

type MonadStablecoin caps m =
  ( MonadCleveland caps m
  , HasStablecoinCaps caps
  )

class HasClevelandCaps caps => HasStablecoinCaps caps where
  getStablecoinCap :: caps -> StablecoinImpl (ClevelandBaseMonad caps)

makeLensesFor [("scCleveland", "scClevelandL")] ''StablecoinCaps

instance Monad m => HasClevelandCaps (StablecoinCaps m) where
  type ClevelandBaseMonad (StablecoinCaps m) = m
  clevelandCapsL = scClevelandL

instance Monad m => HasStablecoinCaps (StablecoinCaps m) where
  getStablecoinCap = scStablecoin

runStablecoinClient :: NetworkEnv -> StablecoinT ClientM () -> IO ()
runStablecoinClient env scenario = displayUncaughtException $ do
  disableAlphanetWarning
  moneybag <- setupMoneybagAddress env
  let caps = ClevelandCaps
        { ccSender = Sender $ unMoneybag moneybag
        , ccMoneybag = moneybag
        , ccMiscCap = networkMiscImpl env
        , ccOpsCap = networkOpsImpl clientEnv
        }
      clientEnv = neMorleyClientEnv env
  ist <- newIORef ClientState
    { csDefaultAliasCounter = DefaultAliasCounter 0
    , csRefillableAddresses = mempty
    , csMoneybagAddress = moneybag
    }
  runReaderT (unClientM $ uncapsStablecoin scenario (stablecoinImplClient clientEnv) caps) ist
  where
    uncapsStablecoin
      :: forall m a. Monad m => StablecoinT m a -> StablecoinImpl m -> ClevelandCaps m -> m a
    uncapsStablecoin action scStablecoin scCleveland =
      runReaderT action StablecoinCaps { .. }

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

implActionToCaps
  :: MonadStablecoin caps m
  => (StablecoinImpl (ClevelandBaseMonad caps) -> ClevelandBaseMonad caps a)
  -> m a
implActionToCaps useCap = do
  cap <- asks getStablecoinCap
  lift $ useCap cap

deploy :: MonadStablecoin caps m => "sender" :! ImplicitAddress -> InitialStorageOptions -> m ContractAddress
deploy s st = implActionToCaps \cap -> siDeploy cap s st

transfer
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> L1Address -> Natural -> m ()
transfer s c a1 a2 n = implActionToCaps \caps -> siTransfer caps s c a1 a2 n

getBalanceOf
  :: MonadStablecoin caps m
  => "contract" :! ContractAddress
  -> L1Address -> m Natural
getBalanceOf c a = implActionToCaps \caps -> siGetBalanceOf caps c a

updateOperators
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> NonEmpty UpdateOperatorData -> m ()
updateOperators s c d = implActionToCaps \caps -> siUpdateOperators caps s c d

isOperator
  :: MonadStablecoin caps m
  => "contract" :! ContractAddress
  -> L1Address -> L1Address -> m Bool
isOperator c a1 a2 = implActionToCaps \caps -> siIsOperator caps c a1 a2

pause :: MonadStablecoin caps m => "sender" :! ImplicitAddress -> "contract" :! ContractAddress -> m ()
pause s c = implActionToCaps \cap -> siPause cap s c

unpause :: MonadStablecoin caps m => "sender" :! ImplicitAddress -> "contract" :! ContractAddress -> m ()
unpause s c = implActionToCaps \cap -> siUnpause cap s c

configureMinter
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> Maybe Natural -> Natural -> m ()
configureMinter s c a mn n = implActionToCaps \cap -> siConfigureMinter cap s c a mn n

removeMinter
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
removeMinter s c a = implActionToCaps \cap -> siRemoveMinter cap s c a

mint
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> Natural -> m ()
mint s c a n = implActionToCaps \cap -> siMint cap s c a n

burn
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> NonEmpty Natural -> m ()
burn s c ns = implActionToCaps \cap -> siBurn cap s c ns

transferOwnership
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
transferOwnership s c a = implActionToCaps \cap -> siTransferOwnership cap s c a

acceptOwnership
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> m ()
acceptOwnership s c = implActionToCaps \cap -> siAcceptOwnership cap s c

changeMasterMinter
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
changeMasterMinter s c a = implActionToCaps \cap -> siChangeMasterMinter cap s c a

changePauser
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> L1Address -> m ()
changePauser s c a = implActionToCaps \cap -> siChangePauser cap s c a

setTransferlist
  :: MonadStablecoin caps m
  => "sender" :! ImplicitAddress -> "contract" :! ContractAddress
  -> Maybe ContractAddress -> m ()
setTransferlist s c a = implActionToCaps \cap -> siSetTransferlist cap s c a

getBalance :: MonadStablecoin caps m => "contract" :! ContractAddress -> m Mutez
getBalance c = implActionToCaps (`siGetBalance` c)

getPaused :: MonadStablecoin caps m => "contract" :! ContractAddress -> m Bool
getPaused c = implActionToCaps (`siGetPaused` c)

getContractOwner :: MonadStablecoin caps m => "contract" :! ContractAddress -> m AddressAndAlias
getContractOwner c = implActionToCaps (`siGetContractOwner` c)

getPendingContractOwner :: MonadStablecoin caps m => "contract" :! ContractAddress -> m (Maybe AddressAndAlias)
getPendingContractOwner c = implActionToCaps (`siGetPendingContractOwner` c)

getMasterMinter :: MonadStablecoin caps m => "contract" :! ContractAddress -> m AddressAndAlias
getMasterMinter c = implActionToCaps (`siGetMasterMinter` c)

getPauser :: MonadStablecoin caps m => "contract" :! ContractAddress -> m AddressAndAlias
getPauser c = implActionToCaps (`siGetPauser` c)

getTransferlist :: MonadStablecoin caps m => "contract" :! ContractAddress -> m (Maybe AddressAndAlias)
getTransferlist c = implActionToCaps (`siGetTransferlist` c)

getMintingAllowance :: MonadStablecoin caps m => "contract" :! ContractAddress -> L1Address -> m Natural
getMintingAllowance c a = implActionToCaps \cap -> siGetMintingAllowance cap c a

getTokenMetadata
  :: MonadStablecoin caps m
  => "contract" :! ContractAddress
  -> m ("symbol" :! Text, "name" :! Text, "decimals" :! Natural)
getTokenMetadata c = implActionToCaps (`siGetTokenMetadata` c)

assertEq :: MonadStablecoin caps m => (Eq a, Buildable a, Show a) => a -> a -> m ()
assertEq a b = implActionToCaps \cap -> siAssertEq cap a b

revealKeyUnlessRevealed :: MonadStablecoin caps m => ImplicitAddress -> m ()
revealKeyUnlessRevealed a = implActionToCaps (`siRevealKeyUnlessRevealed` a)
