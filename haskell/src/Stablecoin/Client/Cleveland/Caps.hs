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
import Morley.Tezos.Address (Address)
import Morley.Tezos.Address.Alias (AddressOrAlias(..))
import Morley.Tezos.Core (Mutez)
import Morley.Util.Named ((:!))
import Test.Cleveland.Internal.Abstract
  (ClevelandCaps(..), DefaultAliasCounter(..), HasClevelandCaps(..), MonadCleveland, Moneybag(..),
  Sender(..))
import Test.Cleveland.Internal.Client
  (ClientM(..), ClientState(..), NetworkEnv(..), networkMiscImpl, networkOpsImpl,
  setupMoneybagAddress)

import Stablecoin.Client (AddressAndAlias(..), InitialStorageData(..), UpdateOperatorData)
import Stablecoin.Client.Cleveland.StablecoinImpl (StablecoinImpl(..), stablecoinImplClient)

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
        , ccMiscCap = networkMiscImpl clientEnv
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

deploy :: MonadStablecoin caps m => "sender" :! AddressOrAlias -> InitialStorageData AddressOrAlias -> m Address
deploy s st = implActionToCaps \cap -> siDeploy cap s st

transfer
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> Natural -> m ()
transfer s c a1 a2 n = implActionToCaps \caps -> siTransfer caps s c a1 a2 n

getBalanceOf
  :: MonadStablecoin caps m
  => "contract" :! AddressOrAlias
  -> AddressOrAlias -> m Natural
getBalanceOf c a = implActionToCaps \caps -> siGetBalanceOf caps c a

updateOperators
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> NonEmpty UpdateOperatorData -> m ()
updateOperators s c d = implActionToCaps \caps -> siUpdateOperators caps s c d

isOperator
  :: MonadStablecoin caps m
  => "contract" :! AddressOrAlias
  -> AddressOrAlias -> AddressOrAlias -> m Bool
isOperator c a1 a2 = implActionToCaps \caps -> siIsOperator caps c a1 a2

pause :: MonadStablecoin caps m => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> m ()
pause s c = implActionToCaps \cap -> siPause cap s c

unpause :: MonadStablecoin caps m => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias -> m ()
unpause s c = implActionToCaps \cap -> siUnpause cap s c

configureMinter
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> Maybe Natural -> Natural -> m ()
configureMinter s c a mn n = implActionToCaps \cap -> siConfigureMinter cap s c a mn n

removeMinter
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
removeMinter s c a = implActionToCaps \cap -> siRemoveMinter cap s c a

mint
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> Natural -> m ()
mint s c a n = implActionToCaps \cap -> siMint cap s c a n

burn
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> NonEmpty Natural -> m ()
burn s c ns = implActionToCaps \cap -> siBurn cap s c ns

transferOwnership
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
transferOwnership s c a = implActionToCaps \cap -> siTransferOwnership cap s c a

acceptOwnership
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> m ()
acceptOwnership s c = implActionToCaps \cap -> siAcceptOwnership cap s c

changeMasterMinter
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
changeMasterMinter s c a = implActionToCaps \cap -> siChangeMasterMinter cap s c a

changePauser
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> AddressOrAlias -> m ()
changePauser s c a = implActionToCaps \cap -> siChangePauser cap s c a

setTransferlist
  :: MonadStablecoin caps m
  => "sender" :! AddressOrAlias -> "contract" :! AddressOrAlias
  -> Maybe AddressOrAlias -> m ()
setTransferlist s c a = implActionToCaps \cap -> siSetTransferlist cap s c a

getBalance :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m Mutez
getBalance c = implActionToCaps (`siGetBalance` c)

getPaused :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m Bool
getPaused c = implActionToCaps (`siGetPaused` c)

getContractOwner :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m AddressAndAlias
getContractOwner c = implActionToCaps (`siGetContractOwner` c)

getPendingContractOwner :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m (Maybe AddressAndAlias)
getPendingContractOwner c = implActionToCaps (`siGetPendingContractOwner` c)

getMasterMinter :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m AddressAndAlias
getMasterMinter c = implActionToCaps (`siGetMasterMinter` c)

getPauser :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m AddressAndAlias
getPauser c = implActionToCaps (`siGetPauser` c)

getTransferlist :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> m (Maybe AddressAndAlias)
getTransferlist c = implActionToCaps (`siGetTransferlist` c)

getMintingAllowance :: MonadStablecoin caps m => "contract" :! AddressOrAlias -> AddressOrAlias -> m Natural
getMintingAllowance c a = implActionToCaps \cap -> siGetMintingAllowance cap c a

getTokenMetadata
  :: MonadStablecoin caps m
  => "contract" :! AddressOrAlias
  -> m ("symbol" :! Text, "name" :! Text, "decimals" :! Natural)
getTokenMetadata c = implActionToCaps (`siGetTokenMetadata` c)

assertEq :: MonadStablecoin caps m => (Eq a, Buildable a, Show a) => a -> a -> m ()
assertEq a b = implActionToCaps \cap -> siAssertEq cap a b

revealKeyUnlessRevealed :: MonadStablecoin caps m => Address -> m ()
revealKeyUnlessRevealed a = implActionToCaps (`siRevealKeyUnlessRevealed` a)
