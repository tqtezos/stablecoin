-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | This a dummy file that resembles lorentz-contract-transferlist parameter needed for origination.
--
-- For more information you can refer to
-- https://github.com/tqtezos/lorentz-contract-transferlist/tree/476c0429adf74041ed7d26c44e5e8e73f5a54f36

module Indigo.Contracts.Transferlist.External
  ( Storage (..)
  , convertToExternalStorage
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Indigo.Contracts.Transferlist.Internal as Internal
import Lorentz as L
import qualified Unsafe

type TransferlistId = Natural

data OutboundTransferlists = OutboundTransferlists
  { unrestricted :: Bool
  , allowedTransferlists :: Set TransferlistId
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

data Storage = Storage
  { issuer :: Address
  , users  :: BigMap Address TransferlistId
  , transferlists :: BigMap TransferlistId OutboundTransferlists
  , admin :: Address
  }
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

-- | Construct an external whitelist storage from our internal one.
-- This function basically converts `Set (from, to)` to
-- `BigMap from (Bool, Set to)`. Note that not all addresses can be
-- listed as receivers that are allowed for transfer in `Internal.Storage`.
-- This can fail on empty lists of transfers.
convertToExternalStorage :: Internal.Storage -> Address -> Address -> Storage
convertToExternalStorage Internal.Storage {..} issuer admin = Storage {..}
  where
    users :: BigMap Address TransferlistId
    users = BigMap uniqueUsers

    transferlists :: BigMap TransferlistId OutboundTransferlists
    transferlists
      | null grouped = BigMap Map.empty -- Check for `head` call in `translate`
      | otherwise = BigMap $ Map.fromList (translate <$> grouped)

    -- Construct a map of all unique addresses and their ids participating in transfers
    uniqueUsers :: Map Address TransferlistId
    uniqueUsers = sTransfers
                & Set.toList
                & fmap (\(a, b) -> [a, b])
                & mconcat
                & Set.fromList
                & Set.toList
                & flip zip [0..]
                & Map.fromList

    -- Group all transfers by their respectful senders
    grouped :: [[(Address, Address)]]
    grouped = List.groupBy ((==) `on` fst) $ Set.toList sTransfers

    -- Construct an element of `transferlists` bigmap from a group of transfers
    -- with same sender
    translate :: [(Address, Address)] -> (TransferlistId, OutboundTransferlists)
    translate transfers =
      -- We construct map from `sTransfers` so it's safe to call `(!)` here
      ( uniqueUsers Map.! fst (Unsafe.head transfers)
      , OutboundTransferlists True $ Set.fromList $ fmap (\(_,x) -> uniqueUsers Map.! x) transfers
      )
