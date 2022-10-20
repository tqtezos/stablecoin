-- SPDX-FileCopyrightText: 2022 Oxhead Alpha
-- SPDX-License-Identifier: MIT

module Stablecoin.Client.Metadata
  ( callOffChainView
  , ViewParam(..)
  , MetadataError(..)
  )
  where

import Data.Constraint (evidence, (\\))
import Data.Singletons (demote)
import Fmt (Buildable(build), indentF, pretty, unlinesF, (+|), (|+))
import Lorentz (NiceParameter, NiceStorage, ToT, def, niceParameterEvi, niceStorageEvi)
import Lorentz qualified as L
import Lorentz.Contracts.Spec.TZIP16Interface (MichelsonStorageView(..), View)
import Lorentz.Contracts.Spec.TZIP16Interface qualified as TZ
import Lorentz.Contracts.Stablecoin (Storage(..))
import Morley.AsRPC (HasRPCRepr(..), rpcHasNoOpEvi)
import Morley.Client (MorleyClientM)
import Morley.Client qualified as Client
import Morley.Client.Util qualified as Client
import Morley.Micheline (Expression, FromExpression, FromExpressionError, fromExpression)
import Morley.Michelson.Typed
  (Dict(..), Instr(..), IsoValue, T, mkContractCode, pattern (:#), starNotes, starParamNotes, toVal,
  untypeValue)
import Morley.Michelson.Typed qualified as T
import Morley.Tezos.Address (ContractAddress)
import Morley.Util.Interpolate

-- TODO [morley-metadata#18]:
-- Delete this when https://gitlab.com/morley-framework/morley-metadata/-/issues/18 is merged.
callOffChainView
  :: forall viewRet viewParamMaybe storage
   . ( NiceStorage storage
     , HasRPCRepr storage
     , HasRPCRepr viewRet
     , IsoValue (AsRPC viewRet)
     , NiceStorage viewRet
     )
  => [View (ToT storage)]
  -> (AsRPC Storage)
  -> ContractAddress
  -> Text
  -> ViewParam viewParamMaybe
  -> MorleyClientM (AsRPC viewRet)
callOffChainView offChainViews st addr viewName viewParam = do

  offChainView <- findView viewName offChainViews
    & evalJust (MEViewNotFound viewName)

  offChainViewImpl <- findViewImplementation @storage viewName offChainView

  balance <- Client.getBalance addr

  Dict <- pure $ evidence $ niceStorageEvi @storage
  Dict <- pure $ evidence $ rpcHasNoOpEvi @(ToT storage)
  Dict <- pure $ evidence $ niceStorageEvi @viewRet

  Dict <- pure $ viewInstrParamDict @storage viewParam
  Dict <- pure $ evidence $ niceParameterEvi @(ViewInstrParam viewParamMaybe storage)

  instr <- typeCheckOffChainView @storage @viewRet viewParam offChainViewImpl
    & evalRight (METypeCheckingFailed viewName)

  let contract :: L.Contract (ViewInstrParam viewParamMaybe storage) (Maybe viewRet) () =
        L.Contract
          { cMichelsonContract = T.Contract
            { cCode = mkContractCode $
                T.CAR
                :# instr
                :# SOME
                :# NIL
                :# PAIR
            , cParamNotes = starParamNotes
            , cStoreNotes = starNotes
            , cViews = T.emptyViewsSet
            , cEntriesOrder = def
            }
          , cDocumentedCode = L.mkContractCode L.fakeCoerce
          }

  Client.runContract Client.RunContractParameters
    { rcpContract = L.toMichelsonContract $ contract
    , rcpParameter =
        case viewParam of
          ViewParam vp -> untypeValue $ toVal (vp, st)
          NoParam -> untypeValue $ toVal st

    , rcpStorage = untypeValue $ toVal $ Nothing @viewRet
    , rcpAmount = 0
    , rcpBalance = balance
    , rcpSource = Nothing
    , rcpLevel = Nothing
    , rcpNow = Nothing
    , rcpSender = Nothing
    }
    <&> T.fromVal @(Maybe (AsRPC viewRet))
    >>= evalJust (MEInternalError viewName "Expected contract's storage to be 'Some', but it was 'None'.")

-- Find a view in the contract's metadata by its name.
findView :: Text -> [TZ.View st] -> Maybe (TZ.View st)
findView viewName xs = find (\v -> TZ.vName v == viewName) xs

findViewImplementation
  :: forall storage m. MonadThrow m => Text -> TZ.View (ToT storage) -> m (TZ.MichelsonStorageView (ToT storage))
findViewImplementation viewName view_ = do
  case getFirst $ foldMap (First . isMichelsonStorageView @storage) $ TZ.vImplementations view_ of
    Just msv -> pure msv
    Nothing -> throwM $ MEImplementationNotFound viewName

isMichelsonStorageView :: TZ.ViewImplementation (ToT storage) -> Maybe (TZ.MichelsonStorageView (ToT storage))
isMichelsonStorageView = \case
  TZ.VIMichelsonStorageView msv -> Just msv
  _ -> Nothing

evalJust :: MonadThrow m => MetadataError -> Maybe a -> m a
evalJust err = maybe (throwM err) pure

evalRight :: MonadThrow m => (a -> MetadataError) -> Either a b -> m b
evalRight mkErr = either (throwM . mkErr) pure

typeCheckOffChainView
  :: forall st ret viewParamMaybe
  . (IsoValue st, IsoValue ret)
  => ViewParam viewParamMaybe
  -> MichelsonStorageView (ToT st)
  -> Either MichelsonStorageViewTypeCheckError (Instr '[ ToT (ViewInstrParam viewParamMaybe st) ] '[ ToT ret ])
typeCheckOffChainView viewParam (MichelsonStorageView paramTypeExprMaybe retTypeExpr code _ _) = do
  -- Note: we don't actually *need* to perform these typechecks here, we
  -- could just try to decode the expression to a typed `Instr` and see it if succeeds.
  -- But the decoding error message could potentially be very confusing.
  --
  -- So, it's best to typecheck `param` and `ret` first and, if they fail, return user-friendly
  -- error messages. If they succeed, _then_ we decode the expression to a typed `Instr`.
  typeCheckReturnType @ret retTypeExpr

  case viewParam of
    ViewParam (_ :: viewParam) -> do
      case paramTypeExprMaybe of
        Nothing -> Left $ MSVTCEParamExpected (demote @(ToT viewParam))
        Just paramTypeExpr -> do
          paramType <- fromExpressionEither @T "parameter" paramTypeExpr
          when (demote @(ToT viewParam) /= paramType) $
            Left $ MSVTCEParamTypeMismatch (demote @(ToT viewParam)) paramType
      fromExpressionEither @(Instr '[ ToT (viewParam, st) ] '[ ToT ret ]) "code" code

    NoParam {} -> do
      whenJust paramTypeExprMaybe \paramTypeExpr ->
        Left $ MSVTCEParamNotExpected paramTypeExpr
      fromExpressionEither @(Instr '[ ToT st ] '[ ToT ret ]) "code" code

typeCheckReturnType :: forall ret. IsoValue ret => Expression -> Either MichelsonStorageViewTypeCheckError ()
typeCheckReturnType retTypeExpr = do
  retType <- fromExpressionEither @T "return-type" retTypeExpr
  when (demote @(ToT ret) /= retType) $
    Left $ MSVTCEReturnTypeMismatch (demote @(ToT ret)) retType

fromExpressionEither :: FromExpression a => Text -> Expression -> Either MichelsonStorageViewTypeCheckError a
fromExpressionEither fieldName = first (MSVTCEDecodingError fieldName) . fromExpression

-- | Construct a proof that there is a `NiceParameter` dict for the view's parameter.
viewInstrParamDict
  :: forall storage viewParamMaybe
  .  NiceStorage storage
  => ViewParam viewParamMaybe
  -> Dict (NiceParameter (ViewInstrParam viewParamMaybe storage))
viewInstrParamDict = \case
  ViewParam (_ :: viewParam) -> Dict \\ niceParameterEvi @viewParam
  NoParam {} -> Dict


-- | A parameter to be passed to a view.
data ViewParam (viewParamMaybe :: Maybe Type) where
  ViewParam :: forall viewParam. NiceParameter viewParam => viewParam -> ViewParam ('Just viewParam)
  NoParam :: ViewParam 'Nothing

-- | If a view has a parameter, then its instructions should
-- expect a @(viewParam, storage)@ tuple at the top of the stack.
--
-- If it doesn't, then they should expect a @storage@ at the top of the stack.
type ViewInstrParam :: Maybe Type -> Type -> Type
type family ViewInstrParam viewParamMaybe storage where
  ViewInstrParam ('Just viewParam) storage = (viewParam, storage)
  ViewInstrParam 'Nothing storage = storage

data MichelsonStorageViewTypeCheckError
  = MSVTCEParamExpected T
  | MSVTCEParamNotExpected Expression
  | MSVTCEParamTypeMismatch T T
  | MSVTCEReturnTypeMismatch T T
  | MSVTCEDecodingError Text FromExpressionError
  deriving stock (Eq, Show)

instance Buildable MichelsonStorageViewTypeCheckError where
  build = \case
    MSVTCEParamExpected expectedT -> unlinesF
      [ "Expected the view's parameter type to be:"
      , indentF 2 $ build expectedT
      , "But the view does not have a parameter."
      ]
    MSVTCEParamNotExpected actualT -> unlinesF
      [ "Expected the view to not have a parameter, but it did:"
      , indentF 2 $ build actualT
      ]
    MSVTCEParamTypeMismatch expectedT actualT -> unlinesF
      [ "Expected the view's parameter type to be:"
      , indentF 2 $ build expectedT
      , "But it was:"
      , indentF 2 $ build actualT
      ]
    MSVTCEReturnTypeMismatch expectedT actualT -> unlinesF
      [ "Expected the view's return type to be:"
      , indentF 2 $ build expectedT
      , "But it was:"
      , indentF 2 $ build actualT
      ]
    MSVTCEDecodingError fieldName err -> unlinesF
      [ "Failed to decode '" +| fieldName |+ "'."
      , "Reason:"
      , indentF 2 $ build err
      ]

data MetadataError
  = MEViewNotFound Text
  | MEImplementationNotFound Text
  | METypeCheckingFailed Text MichelsonStorageViewTypeCheckError
  | MEInternalError Text Text
  deriving stock Show

instance Exception MetadataError where
  displayException = pretty

instance Buildable MetadataError where
  build = \case
    MEImplementationNotFound viewName ->
      [itu|
        A 'Michelson Storage View' implementation was not found for off-chain view '#{viewName}'.
        Note that 'Rest API Query' implementations are not supported at the moment.
        |]
    MEViewNotFound viewName ->
      [itu|Metadata does not contain off-chain view with name: '#{viewName}'.|]
    METypeCheckingFailed viewName err ->
      [itu|
        Failed to typecheck off-chain view '#{viewName}':
          #{err}
        |]
    MEInternalError viewName err ->
      [itu|
        Internal error while evaluating off-chain view '#{viewName}', this is most likely a bug:
          #{err}
        |]
