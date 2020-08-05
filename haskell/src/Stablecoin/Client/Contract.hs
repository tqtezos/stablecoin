-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Contract
  ( parseStablecoinContract
  , mkInitialStorage
  , InitialStorageData(..)
  ) where

import Data.FileEmbed (embedStringFile)
import qualified Data.Map.Strict as Map
import Michelson.Runtime (parseExpandContract)
import Michelson.Text (MText)
import Michelson.Typed (BigMap(BigMap))
import qualified Michelson.Untyped as U
import Tezos.Address (Address)
import Util.Named ((.!))

import Lorentz.Contracts.Stablecoin (Storage, mkTokenMetadata, stablecoinPath)

-- | Parse the stablecoin contract.
parseStablecoinContract :: MonadThrow m => m U.Contract
parseStablecoinContract =
  either throwM pure $
    parseExpandContract
      (Just stablecoinPath)
      $(embedStringFile stablecoinPath)

-- | The data needed in order to create the stablecoin contract's initial storage.
data InitialStorageData addr = InitialStorageData
  { isdMasterMinter :: addr
  , isdContractOwner :: addr
  , isdPauser :: addr
  , isdTransferlist :: Maybe addr
  , isdTokenSymbol :: MText
  , isdTokenName :: MText
  , isdTokenDecimals :: Natural
  }

-- | Construct the stablecoin contract's initial storage in order to deploy it.
mkInitialStorage :: InitialStorageData Address -> Storage
mkInitialStorage (InitialStorageData {..}) =
  (
    (
      ( #ledger .! mempty
      , #minting_allowances .! mempty
      )
    , ( #operators .! mempty
      , #paused .! False
      )
    )
  , (
      ( #roles .!
        (
          ( #master_minter .! isdMasterMinter
          , #owner .! isdContractOwner
          )
        , ( #pauser .! isdPauser
          , #pending_owner_address .! Nothing
          )
        )
      , #token_metadata .! tokenMetadata
      )
    , ( #total_supply .! 0
      , #transferlist_contract .! isdTransferlist
      )
    )
  )
  where
    tokenMetadata = BigMap $ Map.singleton 0 $
      mkTokenMetadata $
        ( #token_id .! 0
        , #mdr .!
          ( #symbol .! isdTokenSymbol
          , #mdr2 .!
            ( #name .! isdTokenName
            , #mdr3 .!
              ( #decimals .! isdTokenDecimals
              , #extras .! mempty
              )
            )
          )
        )
