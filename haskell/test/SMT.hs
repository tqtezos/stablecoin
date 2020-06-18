-- SPDX-FileCopyrightText: 2020 tqtezos
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-orphans #-}

module SMT
  ( smtProperty
  ) where

import qualified Data.Map as Map
import Data.Typeable (cast)
import Fmt
import qualified Unsafe (fromJust)

import Test.Tasty.QuickCheck
  ( Arbitrary, Gen, Property, arbitrary, choose, arbitraryBoundedEnum, elements
  , sublistOf, shuffle, vectorOf)
import qualified Text.Show

import Tezos.Core (unsafeMkMutez)

import Lorentz (NiceParameter, arg, toVal, fromVal, def)
import Lorentz.Address
import Lorentz.Contracts.Stablecoin
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Test.FA2
import Lorentz.Contracts.Test.Common
import Michelson.Test.Integrational
import Michelson.Test.Dummy
import Michelson.Interpret
import Michelson.Typed (untypeValue)
import Michelson.Text
import Michelson.TypeCheck
import qualified Michelson.Typed as T
import qualified Michelson.Untyped as U
import Util.Named

-- Some implementation notes:
--
-- In this module we test the contract by implementing whole contract
-- functionality in a Haskell, and comparing its behavior against the actual
-- contract when run using a Michelson interpreter.
--
-- First we generate a random series of (sender address, contract parameter)
-- pairs. Then we generated a random initial contract state.
-- Then we apply each of these, both to the Haskell model, and on the
-- michelson interpretation. After each iteration we check the contract state has not
-- diverged. If it did, we indicate a test failure. If it did not, we extract the new
-- contract state and initialize both models with it and apply next parameter, and so on.
--
-- For random input generation, we first assign random pool of addresses for
-- each contract roles.  When we make a contract parameter, say transfer, we
-- fetch a random sender address from the pool of token owners, and to/from
-- address (for now we pick operator address and token owner address from the
-- same pool). This is supposed to create inputs that have a little more
-- chance of being valid input that has a chance to execute successfully.

-- Right now safelist interaction is not tested, since it seems that doing it
-- in a useful way require modeling a safelist contract as well.

-- | Assign pools of random address for main roles. Then generate
-- a list of contract calls of given size. When creating the calls, addresses
-- for various roles are not generated randomly, but randomly choosen from the
-- pool for the corresponding role. This give us some assurance that some of these
-- actions will be legal, and will be executed succesfully.
generateContractInputs :: Int -> Gen ([ContractCall Parameter], ContractState)
generateContractInputs count = do
  gsOwnerPool <- vectorOf poolSize (arbitrary @Address)
  gsMasterMinterPool <- vectorOf poolSize (arbitrary @Address)
  gsPauserPool <- vectorOf poolSize (arbitrary @Address)
  gsTokenOwnerPool <- vectorOf poolSize (arbitrary @Address)
  gsMinterPool <- Map.fromList . fmap unMintingAllowance <$> vectorOf 3 (arbitrary @MintingAllowance)
  owner <- elements gsOwnerPool
  masterMinter <- elements gsMasterMinterPool
  pauser <- elements gsPauserPool
  pendingOwner <-  do
    p <- elements gsOwnerPool
    elements [Nothing, Just p]
  operators <- (shuffle $ zip gsTokenOwnerPool gsTokenOwnerPool) >>= sublistOf
  isPaused <- arbitrary @Bool
  ledgerBalances <- Map.fromList <$> do
    randomBalances <- vectorOf poolSize ((fromIntegral @Int) <$> (choose @Int (0, amountRange)))
    sublistOf $ zip gsTokenOwnerPool randomBalances
  let
    generatorState :: GeneratorState = GeneratorState
        gsOwnerPool gsMasterMinterPool gsMinterPool
         gsPauserPool gsTokenOwnerPool
    operatorsMap = Map.fromList $ zip operators $ repeat ()
    totalSupply = sum $ Map.elems ledgerBalances
    startingStorage = SimpleStorage
      gsMinterPool ledgerBalances owner masterMinter pauser pendingOwner Nothing operatorsMap isPaused totalSupply
  inputs <- runReaderT (mapM generateAction [1..count]) generatorState
  pure (inputs, ContractState startingStorage [])

-- | The property that is being tested.
-- This is a quickcheck `Testable` because we have an
-- arbitrary instance for `PropertyTestInput`.
smtProperty
  :: U.Contract
  -> PropertyTestInput
  -> Property
smtProperty contract (PropertyTestInput (inputs, initialState))
  = integrationalTestProperty $ applyBothAndCompare contract inputs initialState

-- | Accept a contract, a list of contract inputs and an initial state.  Then
-- apply each contract input to each model (haskell and michelson), check if
-- contract state have diverged If it hasn't, extract the contract storage and
-- initialize the contract with the result storage, and apply the next
-- parameter and repeat until all contract inputs are applied or until the contract
-- state diverges.
applyBothAndCompare
  :: U.Contract
  -> [ContractCall Parameter]
  -> ContractState
  -> IntegrationalScenario
applyBothAndCompare _ [] _ = skipTest
applyBothAndCompare contract (cc:ccs) cs = let
  haskellResult = stablecoinHaskellModel cc cs
  michelsonResult = stablecoinMichelsonModel contract cc cs
  in if haskellResult /= michelsonResult
    then integrationalFail $ CustomValidationError ("Models differ : " <> (show (cc, cs, haskellResult, michelsonResult)))
    else applyBothAndCompare contract ccs haskellResult

-- Size of the random address pool
poolSize :: Int
poolSize = 3

-- The max amount that will be included in contract transactions.
amountRange :: (Integral a) => a
amountRange = 100

-- The number of contract calls in a single batch
testSize :: Int
testSize = 100

-- The possible contract errors for the models
data ModelError
  = INSUFFICIENT_BALANCE
  | ALLOWANCE_EXCEEDED
  | NOT_OPERATOR
  | NOT_CONTRACT_OWNER
  | NOT_TOKEN_OWNER
  | NOT_PAUSER
  | NOT_MASTERMINTER
  | NOT_MINTER
  | NOT_PENDING_OWNER
  | CURRENT_ALLOWANCE_REQUIRED
  | ALLOWANCE_MISMATCH
  | ADDR_NOT_MINTER
  | CONTRACT_PAUSED
  | CONTRACT_NOT_PAUSED
  | UNKNOWN_ADDRESS
  | CONTRACT_NOT_IN_TRANSFER
  | SOME_ERROR Text
  deriving stock (Generic, Eq, Show)

instance Buildable ModelError where
  build = genericF

data SimpleStorage = SimpleStorage
  { ssMintingAllowances :: Map Address Natural
  , ssLedger :: Map Address Natural
  , ssOwner :: Address
  , ssMasterMinter :: Address
  , ssPauser :: Address
  , ssPendingOwner :: Maybe Address
  , ssSafelistContract :: Maybe Address
  , ssOperators :: Map (Address, Address) ()
  , ssIsPaused :: Bool
  , ssTotalSupply :: Natural
  } deriving stock (Eq, Generic, Show)

instance Buildable SimpleStorage where
  build = genericF

resultToSs :: InterpretResult -> SimpleStorage
resultToSs InterpretResult {..} = case cast iurNewStorage of
  (Just (sval :: T.Value (T.ToT Storage))) -> storageToSs (fromVal @Storage sval)
  Nothing -> error "Impossible"

storageToSs :: Storage -> SimpleStorage
storageToSs storage = let
  StorageMinters ssMintingAllowances = storage
  StorageLedger ssLedger = storage
  StorageOperators (T.BigMap ssOperators) = storage
  StorageRoles (MasterMinterRole ssMasterMinter) = storage
  StorageRoles (PauserRole ssPauser) = storage
  StorageRoles (OwnerRole ssOwner) = storage
  StorageRoles (PendingOwnerRole ssPendingOwner) = storage
  StorageSafelistContract ssSafelistContract = storage
  StoragePaused ssIsPaused = storage
  (_, (_, arg #total_supply -> ssTotalSupply)) = storage
  in SimpleStorage {..}

ssToOriginationParams
  :: SimpleStorage
  -> OriginationParams
ssToOriginationParams SimpleStorage {..} = let
  minters = ssMintingAllowances
  in defaultOriginationParams
    { opBalances = ssLedger
    , opOwner = ssOwner
    , opOwnerToOperators = mkOwnerToOperator ssOperators
    , opMasterMinter = ssMasterMinter
    , opPauser = ssPauser
    , opPaused = ssIsPaused
    , opMinters = minters
    , opPendingOwner = ssPendingOwner
    , opSafelistContract = TAddress <$> ssSafelistContract
    }
  where
    mkOwnerToOperator :: Map (Address, Address) () -> Map Address [Address]
    mkOwnerToOperator operatorMap =
      Map.foldrWithKey (\(ow, op) _ m -> Map.alter (alterFn op) ow m) mempty operatorMap

    alterFn :: Address -> Maybe [Address] -> Maybe [Address]
    alterFn op Nothing = Just [op]
    alterFn op (Just ops) = Just (op:ops)

data ContractCall p = ContractCall
  { ccSender :: Address
  , ccParameter :: p
  , ccIdx :: Int
  } deriving stock Generic

instance (Buildable p) => Buildable (ContractCall p) where
  build = genericF

instance (Buildable p) => Show (ContractCall p) where
  show = pretty

instance Buildable (Address, Natural) where
  build  = genericF

instance Buildable (Map Address Natural) where
  build = pretty . Map.assocs

data ContractState = ContractState
  { csStorage :: SimpleStorage
  , csError :: [(Int, ModelError)]
  } deriving stock (Eq, Generic)

instance Buildable ContractState where
  build = genericF

instance Show ContractState where
  show = pretty

-- | This structure holds the state of the generator
-- based on which new input values are generated.
data GeneratorState = GeneratorState
  { gsOwnerPool :: [Address]
  , gsMasterMinterPool :: [Address]
  , gsMinterPool :: Map Address Natural
  , gsPauserPool :: [Address]
  , gsTokenOwnerPool :: [Address]
  }

type GeneratorM = ReaderT GeneratorState Gen


newtype PropertyTestInput =
  PropertyTestInput ([ContractCall Parameter], ContractState)
  deriving stock Show

instance Arbitrary PropertyTestInput where
  arbitrary = PropertyTestInput <$> (generateContractInputs testSize)

newtype MintingAllowance = MintingAllowance { unMintingAllowance :: (Address, Natural) }

instance Arbitrary MintingAllowance where
  arbitrary = do
    addr <- arbitrary
    allowance <- choose @Int (0, amountRange)
    pure $ MintingAllowance (addr, fromIntegral allowance)

-- | A helper type to randomly select the type of action that we should
-- create at every iteration/call of @generateAction@ function.
data ActionType
  = OwnerAction
  | MasterMinterAction
  | PauserAction
  | TokenOwnerAction
  | OperatorAction
  | MinterAction
  deriving stock (Bounded, Enum)

-- | Generate a random contract call with a random sender
-- choosed from the corresponding address pool.
generateAction :: Int -> GeneratorM (ContractCall Parameter)
generateAction idx = do
  (lift $ arbitraryBoundedEnum @ActionType) >>= \case
    OwnerAction -> generateOwnerAction idx
    MasterMinterAction -> generateMasterMinterAction idx
    PauserAction -> generatePauserAction idx
    TokenOwnerAction -> generateTokenOwnerAction idx
    OperatorAction -> generateOperatorAction idx
    MinterAction -> generateMinterAction idx

generateOwnerAction :: Int -> GeneratorM (ContractCall Parameter)
generateOwnerAction idx = do
  sender <- getRandomOwner
  transferOwnershipAction <- generateTransferOwnershipAction
  acceptOwnershipAction <- generateAcceptOwnershipAction
  changeMasterMinterAction <- generateChangeMasterMinterAction
  changePauserAction <- generateChangePauserAction
  param <- lift $ elements
    [ transferOwnershipAction
    , acceptOwnershipAction
    , changeMasterMinterAction
    , changePauserAction
    ] -- We don't include `Set_safelist` in the operations.
  pure $ ContractCall sender param idx
  where
    generateTransferOwnershipAction = do
      newOwner <- getRandomOwner
      pure $ Transfer_ownership newOwner

    generateAcceptOwnershipAction = pure Accept_ownership

    generateChangeMasterMinterAction = do
      masterMinter <- getRandomMasterMinter
      pure $ Change_master_minter masterMinter

    generateChangePauserAction = do
      pauser <- getRandomPauser
      pure $ Change_pauser pauser

getMinterAllowance :: Address -> GeneratorM (Maybe Natural)
getMinterAllowance minter = do
  minters <- gsMinterPool <$> ask
  pure $ Map.lookup minter minters

getRandomOwner :: GeneratorM Address
getRandomOwner = do
  pool <- gsOwnerPool <$> ask
  lift $ elements pool

getRandomPauser :: GeneratorM Address
getRandomPauser = do
  pool <- gsPauserPool <$> ask
  lift $ elements pool

getRandomMinter :: GeneratorM Address
getRandomMinter = do
  pool <- gsMinterPool <$> ask
  lift $ elements $ Map.keys pool

getRandomMasterMinter :: GeneratorM Address
getRandomMasterMinter = do
  pool <- gsMasterMinterPool <$> ask
  lift $ elements pool

getRandomTokenOwner :: GeneratorM Address
getRandomTokenOwner = do
  pool <- gsTokenOwnerPool <$> ask
  lift $ elements pool

getRandomOperator :: GeneratorM Address
getRandomOperator = do
  pool <- gsTokenOwnerPool <$> ask
  lift $ elements pool

generateMasterMinterAction :: Int -> GeneratorM (ContractCall Parameter)
generateMasterMinterAction idx = do
  sender <-getRandomMasterMinter
  configureMinterAction <- generateConfigureMinterAction
  removeMinterAction <- generateRemoveMinterAction
  param <- lift $ elements
    [ configureMinterAction
    , removeMinterAction
    ]
  pure $ ContractCall sender param idx
    where
      generateRemoveMinterAction = do
        minter <- getRandomMinter
        pure $ Remove_minter minter

      generateConfigureMinterAction = do
        minter <- getRandomMinter
        mbMintingAllowance <- getMinterAllowance minter
        currentMintingAllowance <-
          case mbMintingAllowance of
            Just ma -> do
              randomNatural <- lift $ choose @Int (0, amountRange)
              lift $ elements [Just (fromIntegral randomNatural), Just ma, Nothing] -- supply correct current minting allowances only some of the time
            Nothing -> do
              randomNatural <- lift $ choose @Int (0, amountRange)
              lift $ elements [Just (fromIntegral randomNatural), Nothing] -- supply correct current minting allowances only some of the time

        newMintingAllowances <- lift $ choose @Int (0, amountRange)
        pure $ Configure_minter (#minter .! minter, (#current_minting_allowance .! currentMintingAllowance, #new_minting_allowance .! (fromIntegral newMintingAllowances)))

generatePauserAction :: Int -> GeneratorM (ContractCall Parameter)
generatePauserAction idx = do
  sender <- getRandomPauser
  param <- lift $ elements [Pause, Unpause]
  pure $ ContractCall sender param idx

generateTokenOwnerAction :: Int -> GeneratorM (ContractCall Parameter)
generateTokenOwnerAction idx = do
  sender <- getRandomTokenOwner
  transferAction <- generateTransferAction
  updateOperatorsAction <- generateUpdateOperatorsAction
  param <- lift $ elements
    [ transferAction
    , updateOperatorsAction
    ]
  pure $ ContractCall sender param idx
  where
    generateUpdateOperatorsAction = do
      operator <- getRandomOperator
      owner <- getRandomOwner
      updateOperation <- lift $ elements
        [ FA2.Add_operator (#owner .! owner, #operator .! operator)
        , FA2.Remove_operator (#owner .! owner, #operator .! operator)
        ]
      pure $ Call_FA2 $ FA2.Update_operators [updateOperation]

generateTransferAction :: GeneratorM Parameter
generateTransferAction = do
  from <- getRandomTokenOwner
  txs <- replicateM 10 $ do
    to <- getRandomTokenOwner
    amount <- lift $ choose @Int (0, amountRange)
    pure (#to_ .! to, (#token_id .! 0, #amount .! fromIntegral amount))
  stxs <- lift $ sublistOf txs
  let transferItem = (#from_ .! from, #txs .! stxs)
  pure $ Call_FA2 $ FA2.Transfer [transferItem]

generateOperatorAction :: Int -> GeneratorM (ContractCall Parameter)
generateOperatorAction idx = do
  sender <- getRandomOperator
  parameter <- generateTransferAction
  pure $ ContractCall sender parameter idx

generateMinterAction :: Int -> GeneratorM (ContractCall Parameter)
generateMinterAction idx = do
  mintAction <- generateMintAction idx
  burnAction <- generateBurnAction idx
  lift $ elements [mintAction, burnAction]

generateMintAction :: Int -> GeneratorM (ContractCall Parameter)
generateMintAction idx = do
  sender <- getRandomMinter
  mints <- replicateM 10 $ do
    targetMinter <- getRandomMinter
    targetTokenOwner <- getRandomTokenOwner
    target <- lift $ elements [targetMinter, targetTokenOwner]
    mintValue <- lift $ choose @Int (0, amountRange)
    pure (#to_ .! target, #amount .! fromIntegral mintValue)
  parameter <- lift $ sublistOf mints
  pure $ ContractCall sender (Mint parameter) idx

generateBurnAction :: Int -> GeneratorM (ContractCall Parameter)
generateBurnAction idx = do
  sender <- getRandomMinter
  burns <- lift $ vectorOf 10 $ choose @Int (0, amountRange)
  pure $ ContractCall sender (Burn $ fromIntegral <$> burns) idx

-- | Haskell model that mimicks the expected contract behavior
stablecoinHaskellModel
  :: ContractCall Parameter
  -> ContractState
  -> ContractState
stablecoinHaskellModel cc cs =
  case applyParameter cc (csStorage cs) of
    Right cs1 -> cs { csStorage = cs1 }
    Left err -> cs { csError = ((ccIdx cc), err) : csError cs }

contractErrorToModelError :: MText -> ModelError
contractErrorToModelError m = let
  txtError = toString $ unMText m
 in case txtError of
   "CONTRACT_PAUSED" -> CONTRACT_PAUSED
   "NOT_CONTRACT_OWNER" -> NOT_CONTRACT_OWNER
   "NOT_OPERATOR" -> NOT_OPERATOR
   "NOT_MASTER_MINTER" -> NOT_MASTERMINTER
   "NOT_MINTER" -> NOT_MINTER
   "NOT_PAUSER" -> NOT_PAUSER
   "NOT_PENDING_OWNER" -> NOT_PENDING_OWNER
   "INSUFFICIENT_BALANCE" -> INSUFFICIENT_BALANCE
   "CONTRACT_NOT_PAUSED" -> CONTRACT_NOT_PAUSED
   "NO_PENDING_OWNER_SET" -> CONTRACT_NOT_IN_TRANSFER
   "ADDR_NOT_MINTER" -> ADDR_NOT_MINTER
   "NOT_TOKEN_OWNER" -> NOT_TOKEN_OWNER
   "ALLOWANCE_MISMATCH" -> ALLOWANCE_MISMATCH
   "CURRENT_ALLOWANCE_REQUIRED" -> CURRENT_ALLOWANCE_REQUIRED
   "ALLOWANCE_EXCEEDED" -> ALLOWANCE_EXCEEDED
   _ -> SOME_ERROR (unMText m)

-- | The michelson contract interpreter. The arguments to this
-- function is similar to the Haskell model. So this function also
-- initializes the contract state from @ContractState@, run the
-- list of @ContractCall@, then convert contract state from the Michelson
-- interpreter to the @ContractState@. If contract works as expected,
-- the final @ContractState@ from Haskell model and this model should
-- match exactly, including the list of errors.
stablecoinMichelsonModel
  :: U.Contract
  -> ContractCall Parameter
  -> ContractState
  -> ContractState
stablecoinMichelsonModel contract cc@(ContractCall {..}) cs = let
  contractEnv = dummyContractEnv { ceSender = ccSender, ceAmount = unsafeMkMutez 0 }
  initSt = Unsafe.fromJust $ mkInitialStorage $ ssToOriginationParams $ csStorage cs
  iResult = callEntrypoint contract cc initSt contractEnv
  in case iResult of
    Right iRes -> let
      newStorage = resultToSs iRes
      in cs { csStorage = newStorage }
    Left (RuntimeFailure (MichelsonFailedWith (T.VString tval), _)) -> cs { csError = (ccIdx, contractErrorToModelError tval):(csError cs) }
    Left err -> error $ "Unexpected error:" <> show err

-- | A version of @interpretUntyped@ that also can make an entrypoint
-- based contract execution.
interpretUntyped_
  :: U.Contract
  -> U.EpName
  -> U.Value
  -> Storage
  -> ContractEnv
  -> Either InterpretError InterpretResult
interpretUntyped_ uContract@U.Contract{..} epName paramU initStU env = do
  SomeContract (T.Contract (instr :: T.ContractCode cp st) cpNotes _)
      <- first IllTypedContract $ typeCheckContract (ceContracts env) uContract
  let
    runTC :: forall t. T.SingI t => U.Value -> Either TCError (T.Value t)
    runTC =
      runTypeCheck contractParameter (ceContracts env) .
      usingReaderT def .
      typeCheckValue @t

  case T.mkEntryPointCall epName cpNotes of
      Just (T.MkEntryPointCallRes (_ :: T.Notes arg) epCallT) -> do
        paramV <- first IllTypedParam $ runTC @arg paramU
        case cast instr of
          Just (cInst :: T.ContractCode cp (T.ToT Storage)) ->
            handleContractReturn $ interpret cInst epCallT paramV (toVal initStU) env
          Nothing -> error "Error casting contract"
      Nothing -> error "Entrypoint not found"

callEntrypoint
  :: U.Contract
  -> ContractCall Parameter
  -> Storage
  -> ContractEnv
  -> Either InterpretError InterpretResult
callEntrypoint contract cc st env = case ccParameter cc of
  Pause -> callContract "pause" ()
  Unpause -> callContract "unpause" ()
  Configure_minter p -> callContract "configure_minter" p
  Remove_minter p -> callContract "remove_minter" p
  Mint p -> callContract "mint" p
  Burn p -> callContract "burn" p
  Transfer_ownership p -> callContract "transfer_ownership" p
  Accept_ownership -> callContract "accept_ownership" ()
  Change_master_minter p -> callContract "change_master_minter" p
  Change_pauser p -> callContract "change_pauser" p
  Set_safelist p -> callContract "set_safelist" p
  Call_FA2 fa2Param -> case fa2Param of
    FA2.Transfer p -> callContract "transfer" p
    FA2.Update_operators p -> callContract "update_operators" p
    _ -> error "Unexpected call"
  where
    callContract
      :: (NiceParameter p, T.HasNoOp (T.ToT p))
      => Text
      -> p
      -> Either InterpretError InterpretResult
    callContract epName param = case U.epNameFromParamAnn (U.ann epName) of
      Just epn ->
        interpretUntyped_ contract epn (untypeValue $ toVal param) st env
      Nothing -> error "Bad entrypoint name"

ensureOwner :: Address -> SimpleStorage -> Either ModelError ()
ensureOwner addr SimpleStorage {..} = if addr == ssOwner then Right () else Left NOT_CONTRACT_OWNER

ensurePauser :: Address -> SimpleStorage -> Either ModelError ()
ensurePauser addr SimpleStorage {..} = if addr == ssPauser then Right () else Left NOT_PAUSER

ensureMasterMinter :: Address -> SimpleStorage -> Either ModelError ()
ensureMasterMinter addr SimpleStorage {..} =
  if addr == ssMasterMinter then Right () else Left NOT_MASTERMINTER

ensurePendingOwner :: Address -> SimpleStorage -> Either ModelError Address
ensurePendingOwner addr SimpleStorage {..} = case ssPendingOwner of
  Just pa -> if addr == pa then Right pa else Left NOT_PENDING_OWNER
  Nothing -> Left CONTRACT_NOT_IN_TRANSFER

ensureNotPaused :: SimpleStorage -> Either ModelError ()
ensureNotPaused SimpleStorage {..} = if not ssIsPaused then Right () else Left CONTRACT_PAUSED

ensurePaused :: SimpleStorage -> Either ModelError ()
ensurePaused SimpleStorage {..} = if ssIsPaused then Right () else Left CONTRACT_NOT_PAUSED

ensureMinter :: Address -> SimpleStorage -> Either ModelError Natural
ensureMinter minter cs = case Map.lookup minter $ ssMintingAllowances cs of
  Just ma -> Right ma
  Nothing -> Left NOT_MINTER

isOperatorOf :: SimpleStorage -> Address -> Address -> Bool
isOperatorOf ss operator owner = case Map.lookup (operator, owner) (ssOperators ss) of
  Just _ -> True
  _ -> False

applyPause :: Address -> SimpleStorage -> Either ModelError SimpleStorage
applyPause sender ss = do
  ensureNotPaused ss
  ensurePauser sender ss
  Right ss { ssIsPaused = True }

applyUnpause :: Address -> SimpleStorage -> Either ModelError SimpleStorage
applyUnpause sender ss = do
  ensurePaused ss
  ensurePauser sender ss
  Right ss { ssIsPaused = False }

applyConfigureMinter :: Address -> SimpleStorage -> ConfigureMinterParam -> Either ModelError SimpleStorage
applyConfigureMinter sender cs (ConfigureMinterParams minter mbCurrent new) = do
  -- Check sender is master minter
  -- if yes then see if provided current allowance if Just value
  --  if yes then check if minter allowance exist
  --    if yes then compare the two values
  --      if same set the new value
  --      if not set the error and return contract state
  --  if not then then check if minter allowance exist
  --    if yes then set the error that minter unexpectedly exist and return contract state
  --    if no then set the new value
  -- if not then set the permission error and return contract state
  ensureNotPaused cs
  ensureMasterMinter sender cs
  case Map.lookup minter $ ssMintingAllowances cs of
    Just currentAllowanceInStorage ->
      case mbCurrent of
        Just current ->
          if current == currentAllowanceInStorage
            then Right cs { ssMintingAllowances = Map.insert minter new $ ssMintingAllowances cs }
            else Left ALLOWANCE_MISMATCH
        Nothing -> Left CURRENT_ALLOWANCE_REQUIRED
    Nothing ->
      case mbCurrent of
        Just _ -> Left ADDR_NOT_MINTER
        Nothing -> Right $ cs { ssMintingAllowances = Map.insert minter new $ ssMintingAllowances cs }

applyRemoveMinter :: Address -> SimpleStorage -> RemoveMinterParam -> Either ModelError SimpleStorage
applyRemoveMinter sender cs minter = do
  ensureMasterMinter sender cs
  case Map.lookup minter $ ssMintingAllowances cs of
    Just _ -> Right cs { ssMintingAllowances = Map.delete minter $ ssMintingAllowances cs }
    Nothing -> Left ADDR_NOT_MINTER

reduceMintingAllowance :: Address -> Natural -> SimpleStorage -> SimpleStorage
reduceMintingAllowance minter amount ss@SimpleStorage {..} =
  ss { ssMintingAllowances = Map.update (\m -> Just $ m - amount) minter ssMintingAllowances }

applySingleMint :: MintParam -> SimpleStorage -> SimpleStorage
applySingleMint (arg #to_ -> to, arg #amount -> value) storage =
  storage { ssLedger = Map.alter (\case Nothing -> Just value; Just x -> Just $ x + value) to $ ssLedger storage }

applyMint :: Address -> SimpleStorage -> MintParams -> Either ModelError SimpleStorage
applyMint sender cs mintparams = do
  ensureNotPaused cs
  ma <- ensureMinter sender cs
  let totalMint = sum $ (arg #amount . snd) <$> mintparams
  if ma >= totalMint
    then Right $ amendTotalSupply (+ totalMint) $ reduceMintingAllowance sender totalMint $ foldr applySingleMint cs mintparams
    else Left ALLOWANCE_EXCEEDED

applyBurn :: Address -> SimpleStorage -> BurnParams -> Either ModelError SimpleStorage
applyBurn ccSender cs burnparams = do
  ensureNotPaused cs
  void $ ensureMinter ccSender cs
  let totalBurn = sum burnparams
  if totalBurn <= (fromMaybe 0 $ Map.lookup ccSender $ ssLedger cs)
    then let
      burn value storage =
        storage { ssLedger = Map.alter (\case
          Nothing -> error "Unexpected burn"
          Just x -> Just $ x - value) ccSender $ ssLedger cs }
      in Right $ amendTotalSupply (\x -> x - totalBurn) $ foldr burn cs burnparams
    else Left INSUFFICIENT_BALANCE

amendTotalSupply :: (Natural -> Natural) -> SimpleStorage  -> SimpleStorage
amendTotalSupply fn ss = ss { ssTotalSupply = fn (ssTotalSupply ss) }

applyTransferOwnership :: Address -> SimpleStorage -> TransferOwnershipParam -> Either ModelError SimpleStorage
applyTransferOwnership sender cs newOwner = do
  ensureOwner sender cs
  Right cs { ssPendingOwner = Just newOwner }

applyAcceptOwnership :: Address -> SimpleStorage -> Either ModelError SimpleStorage
applyAcceptOwnership sender cs = do
  newOwner <- ensurePendingOwner sender cs
  Right cs { ssOwner = newOwner, ssPendingOwner = Nothing }

applyChangeMasterMinter :: Address -> SimpleStorage -> ChangeMasterMinterParam -> Either ModelError SimpleStorage
applyChangeMasterMinter sender cs newMm = do
  ensureOwner sender cs
  Right cs { ssMasterMinter = newMm }

applyChangePauser :: Address -> SimpleStorage -> ChangeMasterMinterParam -> Either ModelError SimpleStorage
applyChangePauser sender cs newPauser = do
  ensureOwner sender cs
  Right cs { ssPauser = newPauser }

applyParameter :: ContractCall Parameter -> SimpleStorage -> Either ModelError SimpleStorage
applyParameter cc@(ContractCall {..}) cs = case ccParameter of
  Call_FA2 fa2 -> applyFA2Parameter (cc { ccParameter = fa2 }) cs
  Pause -> applyPause ccSender cs
  Unpause -> applyUnpause ccSender cs
  Configure_minter cmp -> applyConfigureMinter ccSender cs cmp
  Remove_minter minter -> applyRemoveMinter ccSender cs minter
  Mint mintparams -> applyMint ccSender cs mintparams
  Burn burnparams -> applyBurn ccSender cs burnparams
  Transfer_ownership newOwner -> applyTransferOwnership ccSender cs newOwner
  Accept_ownership -> applyAcceptOwnership ccSender cs
  Change_master_minter newMm -> applyChangeMasterMinter ccSender cs newMm
  Change_pauser newPsr -> applyChangePauser ccSender cs newPsr
  _ -> error "Unexpected call"

applyDebit :: Address -> Natural ->  SimpleStorage -> SimpleStorage
applyDebit from amount ss@SimpleStorage {..} =  ss { ssLedger = Map.update (\x -> Just $ x - amount) from ssLedger }

applyCredit :: Address -> Natural -> SimpleStorage -> SimpleStorage
applyCredit from amount ss@SimpleStorage {..} =
  ss { ssLedger = Map.alter (\case Just x -> Just $ x + amount; Nothing -> Just amount;) from ssLedger }

applyTransfer :: Address -> SimpleStorage -> FA2.TransferParams -> Either ModelError SimpleStorage
applyTransfer ccSender storage tis = do
  ensureNotPaused storage
  foldl' (applySingleTransfer ccSender) (Right storage) tis

applySingleTransfer :: Address -> Either ModelError SimpleStorage -> FA2.TransferParam -> Either ModelError SimpleStorage
applySingleTransfer ccSender estorage
  (arg #from_ -> from, arg #txs -> toItems)
  = case estorage of
      Left err -> Left err
      Right storage@(SimpleStorage {..}) ->
        if ccSender == from || isOperatorOf storage ccSender from
          then let
            in foldl' singleTranferTx (Right storage) toItems
          else Left NOT_OPERATOR
  where
    singleTranferTx (Left err) _ =  Left err
    singleTranferTx
      (Right storage@(SimpleStorage {..}))
      (arg #to_ -> to, (arg #token_id -> _, arg #amount -> amount)) = let
        -- consider zero balance if account is not found
        srcBalance = fromMaybe 0 (Map.lookup from ssLedger)
        in if srcBalance >= amount
          then Right $ applyCredit to amount $ applyDebit from amount storage
          else Left INSUFFICIENT_BALANCE

applyUpdateOperator :: Address -> Either ModelError SimpleStorage -> FA2.UpdateOperator -> Either ModelError SimpleStorage
applyUpdateOperator ccSender estorage op = case estorage of
  Left err -> Left err
  Right storage -> case op of
    FA2.Add_operator (arg #owner -> own, arg #operator -> operator)
      -> if own == ccSender -- enforce that sender is owner
        then Right $ storage { ssOperators = Map.insert (own, operator) () $ ssOperators storage }
        else Left NOT_TOKEN_OWNER
    FA2.Remove_operator (arg #owner -> own, arg #operator -> operator)
      -> if own == ccSender
        then Right $ storage { ssOperators = Map.delete (own, operator) $ ssOperators storage }
        else Left NOT_TOKEN_OWNER

applyFA2Parameter :: ContractCall FA2.Parameter -> SimpleStorage -> Either ModelError SimpleStorage
applyFA2Parameter ContractCall {..} cs = do
  case ccParameter of
    FA2.Transfer tis -> applyTransfer ccSender cs tis
    FA2.Update_operators ops -> foldl' (applyUpdateOperator ccSender) (Right cs) ops
    _ -> error "Unexpected param"
