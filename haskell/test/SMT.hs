-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-orphans #-}

module SMT
  ( smtProperty
  ) where

import qualified Data.Map as Map
import Data.Typeable (cast)
import Fmt
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Text.Show

import Hedgehog.Gen.Tezos.Address (genAddress)
import Lorentz
  (Address, EntrypointRef(Call), GetEntrypointArgCustom, IsoValue(..), TAddress(TAddress),
  parameterEntrypointCallCustom)
import Michelson.Interpret
import Michelson.Test.Dummy
import Michelson.Test.Integrational
import Michelson.Text
import qualified Michelson.Typed as T
import Tezos.Core (unsafeMkMutez)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Lorentz.Contracts.Stablecoin
import Lorentz.Contracts.Test.Common

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

-- Right now transferlist interaction is not tested, since it seems that doing it
-- in a useful way require modeling a transferlist contract as well.

-- | Assign pools of random address for main roles. Then generate
-- a list of contract calls of given size. When creating the calls, addresses
-- for various roles are not generated randomly, but randomly choosen from the
-- pool for the corresponding role. This give us some assurance that some of these
-- actions will be legal, and will be executed succesfully.
generateContractInputs :: MonadGen m => Int -> m ([ContractCall Parameter], ContractState)
generateContractInputs count = do
  gsOwnerPool <- vectorOf poolSize genAddress
  gsMasterMinterPool <- vectorOf poolSize genAddress
  gsPauserPool <- vectorOf poolSize genAddress
  gsTokenOwnerPool <- vectorOf poolSize genAddress
  gsMinterPool <- Map.fromList . fmap unMintingAllowance <$> vectorOf 3 genMintingAllowance
  owner <- Gen.element gsOwnerPool
  masterMinter <- Gen.element gsMasterMinterPool
  pauser <- Gen.element gsPauserPool
  pendingOwner <-  do
    p <- Gen.element gsOwnerPool
    Gen.element [Nothing, Just p]
  operators <- (Gen.shuffle $ zip gsTokenOwnerPool gsTokenOwnerPool) >>= Gen.subsequence
  isPaused <- Gen.bool
  ledgerBalances <- Map.fromList <$> do
    randomBalances <- vectorOf poolSize $
      Gen.integral (Range.constant 0 amountRange)
    Gen.subsequence $ zip gsTokenOwnerPool randomBalances
  let
    generatorState :: GeneratorState = GeneratorState
        gsOwnerPool gsMasterMinterPool gsMinterPool
         gsPauserPool gsTokenOwnerPool
    operatorsMap = Map.fromList $ zip operators $ repeat ()
    startingStorage = SimpleStorage
      gsMinterPool ledgerBalances owner masterMinter pauser pendingOwner Nothing operatorsMap
      isPaused (sum $ Map.elems ledgerBalances)
  inputs <- runReaderT (mapM generateAction [1..count]) generatorState
  pure (inputs, ContractState startingStorage [])

-- | The property that is being tested.
smtProperty :: Property
smtProperty = property $ do
  PropertyTestInput (inputs, initialState) <- forAll genPropertyTestInput
  integrationalTestProp $ do
    applyBothAndCompare inputs initialState

-- | Accept a contract, a list of contract inputs and an initial state.  Then
-- apply each contract input to each model (haskell and michelson), check if
-- contract state have diverged If it hasn't, extract the contract storage and
-- initialize the contract with the result storage, and apply the next
-- parameter and repeat until all contract inputs are applied or until the contract
-- state diverges.
applyBothAndCompare
  :: [ContractCall Parameter]
  -> ContractState
  -> IntegrationalScenario
applyBothAndCompare [] _ = pass
applyBothAndCompare (cc:ccs) cs = let
  haskellResult = stablecoinHaskellModel cc cs
  michelsonResult = stablecoinMichelsonModel cc cs
  in if haskellResult /= michelsonResult
    then integrationalFail $ CustomTestError $
         "Models differ : " <> show (cc, cs, haskellResult, michelsonResult)
    else applyBothAndCompare ccs haskellResult

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
  = FA2_INSUFFICIENT_BALANCE
  | FA2_TOKEN_UNDEFINED
  | ALLOWANCE_EXCEEDED
  | FA2_NOT_OPERATOR
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
  , ssTransferlistContract :: Maybe Address
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
storageToSs storage = SimpleStorage
  { ssMintingAllowances = sMintingAllowances storage
  , ssLedger = T.unBigMap $ sLedger storage
  , ssOwner = rOwner $ sRoles storage
  , ssMasterMinter = rMasterMinter $ sRoles storage
  , ssPauser = rPauser $ sRoles storage
  , ssPendingOwner = rPendingOwner $ sRoles storage
  , ssTransferlistContract = sTransferlistContract storage
  , ssOperators = T.unBigMap $ sOperators storage
  , ssIsPaused = sIsPaused storage
  , ssTotalSupply = sTotalSupply storage
  }

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
    , opTransferlistContract = TAddress <$> ssTransferlistContract
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

type GeneratorM a = forall m. MonadGen m => ReaderT GeneratorState m a

newtype PropertyTestInput =
  PropertyTestInput ([ContractCall Parameter], ContractState)
  deriving stock Show

genPropertyTestInput :: MonadGen m => m PropertyTestInput
genPropertyTestInput = PropertyTestInput <$> generateContractInputs testSize

newtype MintingAllowance = MintingAllowance
  { unMintingAllowance :: (Address, Natural) }

genMintingAllowance :: MonadGen m => m MintingAllowance
genMintingAllowance = do
  addr <- genAddress
  allowance <- Gen.integral (Range.constant 0 amountRange)
  pure $ MintingAllowance (addr, allowance)

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
  Gen.enumBounded >>= \case
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
  param <- Gen.element
    [ transferOwnershipAction
    , acceptOwnershipAction
    , changeMasterMinterAction
    , changePauserAction
    ] -- We don't include `Set_transferlist` in the operations.
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
  Gen.element pool

getRandomPauser :: GeneratorM Address
getRandomPauser = do
  pool <- gsPauserPool <$> ask
  Gen.element pool

getRandomMinter :: GeneratorM Address
getRandomMinter = do
  pool <- gsMinterPool <$> ask
  Gen.element (Map.keys pool)

getRandomMasterMinter :: GeneratorM Address
getRandomMasterMinter = do
  pool <- gsMasterMinterPool <$> ask
  Gen.element pool

getRandomTokenOwner :: GeneratorM Address
getRandomTokenOwner = do
  pool <- gsTokenOwnerPool <$> ask
  Gen.element pool

getRandomOperator :: GeneratorM Address
getRandomOperator = do
  pool <- gsTokenOwnerPool <$> ask
  Gen.element pool

generateMasterMinterAction :: Int -> GeneratorM (ContractCall Parameter)
generateMasterMinterAction idx = do
  sender <-getRandomMasterMinter
  configureMinterAction <- generateConfigureMinterAction
  removeMinterAction <- generateRemoveMinterAction
  param <- Gen.element
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
              randomNatural <- Gen.integral (Range.constant 0 amountRange)
              -- supply correct current minting allowances only some of the time
              Gen.element [Just randomNatural, Just ma, Nothing]
            Nothing -> do
              randomNatural <- Gen.integral (Range.constant 0 amountRange)
              -- supply correct current minting allowances only some of the time
              Gen.element [Just randomNatural, Nothing]

        newMintingAllowances <- Gen.integral (Range.constant 0 amountRange)
        pure $ Configure_minter ConfigureMinterParam
          { cmpMinter = minter
          , cmpCurrentMintingAllowance = currentMintingAllowance
          , cmpNewMintingAllowance = newMintingAllowances
          }

generatePauserAction :: Int -> GeneratorM (ContractCall Parameter)
generatePauserAction idx = do
  sender <- getRandomPauser
  param <- Gen.element [Pause, Unpause]
  pure $ ContractCall sender param idx

generateTokenOwnerAction :: Int -> GeneratorM (ContractCall Parameter)
generateTokenOwnerAction idx = do
  sender <- getRandomTokenOwner
  transferAction <- generateTransferAction
  updateOperatorsAction <- generateUpdateOperatorsAction
  param <- Gen.element
    [ transferAction
    , updateOperatorsAction
    ]
  pure $ ContractCall sender param idx
  where
    generateUpdateOperatorsAction = do
      operator <- getRandomOperator
      owner <- getRandomOwner
      updateOperation <- Gen.element
        [ FA2.AddOperator FA2.OperatorParam { opOwner = owner, opOperator = operator, opTokenId = FA2.theTokenId }
        , FA2.AddOperator FA2.OperatorParam { opOwner = owner, opOperator = operator, opTokenId = oneTokenId }
        , FA2.RemoveOperator FA2.OperatorParam { opOwner = owner, opOperator = operator, opTokenId = FA2.theTokenId }
        , FA2.RemoveOperator FA2.OperatorParam { opOwner = owner, opOperator = operator, opTokenId = oneTokenId }
        ]
      pure $ Call_FA2 $ FA2.Update_operators [updateOperation]

generateTransferAction :: GeneratorM Parameter
generateTransferAction = do
  from <- getRandomTokenOwner
  txs <- replicateM 10 $ do
    to <- getRandomTokenOwner
    amount <- Gen.integral (Range.constant 0 amountRange)
    pure FA2.TransferDestination { tdTo = to, tdTokenId = FA2.theTokenId, tdAmount = amount }
  stxs <- Gen.subsequence txs
  let transferItem = FA2.TransferItem { tiFrom = from, tiTxs = stxs }
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
  Gen.element [mintAction, burnAction]

generateMintAction :: Int -> GeneratorM (ContractCall Parameter)
generateMintAction idx = do
  sender <- getRandomMinter
  mints <- replicateM 10 $ do
    targetMinter <- getRandomMinter
    targetTokenOwner <- getRandomTokenOwner
    target <- Gen.element [targetMinter, targetTokenOwner]
    mintValue <- Gen.integral (Range.constant 0 amountRange)
    pure (MintParam target mintValue)
  parameter <- Gen.subsequence mints
  pure $ ContractCall sender (Mint parameter) idx

generateBurnAction :: Int -> GeneratorM (ContractCall Parameter)
generateBurnAction idx = do
  sender <- getRandomMinter
  burns <- vectorOf 10 $ Gen.integral (Range.constant 0 amountRange)
  pure $ ContractCall sender (Burn burns) idx

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
   "FA2_NOT_OPERATOR" -> FA2_NOT_OPERATOR
   "NOT_MASTER_MINTER" -> NOT_MASTERMINTER
   "NOT_MINTER" -> NOT_MINTER
   "NOT_PAUSER" -> NOT_PAUSER
   "NOT_PENDING_OWNER" -> NOT_PENDING_OWNER
   "FA2_INSUFFICIENT_BALANCE" -> FA2_INSUFFICIENT_BALANCE
   "FA2_TOKEN_UNDEFINED" -> FA2_TOKEN_UNDEFINED
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
  :: ContractCall Parameter
  -> ContractState
  -> ContractState
stablecoinMichelsonModel cc@(ContractCall {..}) cs = let
  contractEnv = dummyContractEnv { ceSender = ccSender, ceAmount = unsafeMkMutez 0 }
  initSt = mkInitialStorage (ssToOriginationParams $ csStorage cs)
  iResult = callEntrypoint cc initSt contractEnv
  in case iResult of
    Right iRes -> let
      newStorage = resultToSs iRes
      in cs { csStorage = newStorage }
    Left (InterpretError (MichelsonFailedWith (T.VString tval), _)) -> cs { csError = (ccIdx, contractErrorToModelError tval):(csError cs) }
    Left err -> error $ "Unexpected error:" <> show err

callEntrypoint
  :: ContractCall Parameter
  -> Storage
  -> ContractEnv
  -> Either InterpretError InterpretResult
callEntrypoint cc st env = case ccParameter cc of
  Pause -> call (Call @"Pause") ()
  Unpause -> call (Call @"Unpause") ()
  Configure_minter p -> call (Call @"Configure_minter") p
  Remove_minter p -> call (Call @"Remove_minter") p
  Mint p -> call (Call @"Mint") p
  Burn p -> call (Call @"Burn") p
  Transfer_ownership p -> call (Call @"Transfer_ownership") p
  Accept_ownership -> call (Call @"Accept_ownership") ()
  Change_master_minter p -> call (Call @"Change_master_minter") p
  Change_pauser p -> call (Call @"Change_pauser") p
  Set_transferlist p -> call (Call @"Set_transferlist") p
  Call_FA2 fa2Param -> case fa2Param of
    FA2.Transfer p -> call (Call @"Transfer") p
    FA2.Update_operators p -> call (Call @"Update_operators") p
    _ -> error "Unexpected call"
  Permit _ -> error "Unexpected call"
  Set_expiry _ -> error "Unexpected call"
  where
    call
      :: IsoValue (GetEntrypointArgCustom Parameter ep)
      => EntrypointRef ep
      -> GetEntrypointArgCustom Parameter ep
      -> Either InterpretError InterpretResult
    call epRef param =
      handleContractReturn $
        interpret
          (T.cCode stablecoinContract)
          (parameterEntrypointCallCustom @Parameter epRef)
          (toVal param)
          (toVal st)
          env

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

validateTokenId :: FA2.UpdateOperator -> Either ModelError ()
validateTokenId = \case
  FA2.AddOperator op -> validateTokenId' op
  FA2.RemoveOperator op -> validateTokenId' op
  where
    validateTokenId' :: FA2.OperatorParam -> Either ModelError ()
    validateTokenId' FA2.OperatorParam {..} = if opTokenId == FA2.theTokenId then Right () else Left FA2_TOKEN_UNDEFINED

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
applyConfigureMinter sender cs (ConfigureMinterParam minter mbCurrent new) = do
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
applySingleMint (MintParam to value) storage =
  storage
    { ssTotalSupply = ssTotalSupply storage + value
    , ssLedger = Map.alter
        (\case Nothing -> Just value; Just x -> Just $ x + value) to $ ssLedger storage }

applyMint :: Address -> SimpleStorage -> MintParams -> Either ModelError SimpleStorage
applyMint sender cs mintparams = do
  ensureNotPaused cs
  ma <- ensureMinter sender cs
  let totalMint = sum $ mpAmount <$> mintparams
  if ma >= totalMint
    then Right $ reduceMintingAllowance sender totalMint $ foldr applySingleMint cs mintparams
    else Left ALLOWANCE_EXCEEDED

applyBurn :: Address -> SimpleStorage -> BurnParams -> Either ModelError SimpleStorage
applyBurn ccSender cs burnparams = do
  ensureNotPaused cs
  void $ ensureMinter ccSender cs
  let totalBurn = sum burnparams
  if totalBurn <= (fromMaybe 0 $ Map.lookup ccSender $ ssLedger cs)
    then if totalBurn > ssTotalSupply cs
      then error ("Unexpected burn:" <> show totalBurn)
      else let
        newStorageAfterBurn = foldr (applySingleBurn ccSender) cs burnparams
        in Right $ newStorageAfterBurn { ssTotalSupply = ssTotalSupply cs - totalBurn }
    else Left FA2_INSUFFICIENT_BALANCE

applySingleBurn :: Address -> Natural -> SimpleStorage -> SimpleStorage
applySingleBurn src value storage =
  storage { ssLedger = Map.alter (\case
    Nothing -> if value > 0
      then error ("Unexpected burn:" <> show value)
      else Nothing
          -- As per FA2, burn should follow the tranfer logic, which allows zero transfer
          -- from non-existant accounts. So here we should do nothing, instead of throwing an
          -- error.
    Just x -> let b = x - value
      in if b > 0 then Just b else Nothing) src $ ssLedger storage }

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
applyDebit from amount ss@SimpleStorage {..} =  ss
  { ssLedger =
      Map.update (\x -> let n = x - amount in if n == 0 then Nothing else Just n) from ssLedger }

applyCredit :: Address -> Natural -> SimpleStorage -> SimpleStorage
applyCredit from amount ss@SimpleStorage {..} =
  ss { ssLedger = Map.alter (\case Just x -> Just $ x + amount; Nothing -> Just amount;) from ssLedger }

applyTransfer :: Address -> SimpleStorage -> FA2.TransferParams -> Either ModelError SimpleStorage
applyTransfer ccSender storage tis = do
  ensureNotPaused storage
  foldl' (applySingleTransfer ccSender) (Right storage) tis

applySingleTransfer :: Address -> Either ModelError SimpleStorage -> FA2.TransferItem -> Either ModelError SimpleStorage
applySingleTransfer ccSender estorage (FA2.TransferItem from txs) =
  case estorage of
    Left err -> Left err
    Right storage@(SimpleStorage {..}) ->
      if ccSender == from || isOperatorOf storage ccSender from
        then foldl' singleTranferTx (Right storage) txs
        else Left FA2_NOT_OPERATOR
  where
    singleTranferTx (Left err) _ =  Left err
    singleTranferTx (Right storage@(SimpleStorage {..})) (FA2.TransferDestination to _ amount) = let
      -- consider zero balance if account is not found
      srcBalance = fromMaybe 0 (Map.lookup from ssLedger)
      in if srcBalance >= amount
        then Right $ applyCredit to amount $ applyDebit from amount storage
        else Left FA2_INSUFFICIENT_BALANCE

applyUpdateOperator :: Address -> Either ModelError SimpleStorage -> FA2.UpdateOperator -> Either ModelError SimpleStorage
applyUpdateOperator ccSender estorage op = case estorage of
  Left err -> Left err
  Right storage -> do
    ensureNotPaused storage
    case op of
      FA2.AddOperator (FA2.OperatorParam own operator _)
        -> if own == ccSender -- enforce that sender is owner
          then do
            validateTokenId op
            Right $ storage { ssOperators = Map.insert (own, operator) () $ ssOperators storage }
          else Left NOT_TOKEN_OWNER
      FA2.RemoveOperator (FA2.OperatorParam own operator _)
        -> if own == ccSender
          then do
            validateTokenId op
            Right $ storage { ssOperators = Map.delete (own, operator) $ ssOperators storage }
          else Left NOT_TOKEN_OWNER

applyFA2Parameter :: ContractCall FA2.Parameter -> SimpleStorage -> Either ModelError SimpleStorage
applyFA2Parameter ContractCall {..} cs = do
  case ccParameter of
    FA2.Transfer tis -> applyTransfer ccSender cs tis
    FA2.Update_operators ops -> foldl' (applyUpdateOperator ccSender) (Right cs) ops
    _ -> error "Unexpected param"

-- | Generate a list of items using given generator of a single item.
-- Takes a size parameter. Unlike QuickCheck's @vectorOf@, this function
-- can generate a smaller list (always non-empty).
vectorOf :: MonadGen m => Int -> m x -> m [x]
vectorOf n = Gen.list (Range.linear 1 n)
