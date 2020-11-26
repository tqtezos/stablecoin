-- SPDX-FileCopyrightText: 2020 TQ Tezos
-- SPDX-License-Identifier: MIT

-- | TZIP-16 specification.
-- https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md
module Lorentz.Contracts.Spec.TZIP16Interface
  ( MetadataMap
  , Metadata(..)
  , License(..)
  , Source(..)
  , Error(..)
  , View(..)
  , ViewImplementation(..)
  , SomeMichelsonStorageView(..)
  , MichelsonStorageView(..)
  , mkMichelsonStorageView
  , getView
  , interpretView

  -- * Internals
  , interpretMichelsonStorageView
  , ViewInterpretError(..)
  ) where

import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (Parser)
import Data.Singletons (Sing, demote)
import Fmt (Buildable(..), pretty, unlinesF, (+|), (|+))

import Lorentz (Lambda, MText, Rec((:&), RNil), compileLorentz)
import Michelson.Interpret (ContractEnv, MichelsonFailed, interpretInstr)
import Michelson.Typed (Instr, IsoValue(..), KnownT, T(TPair), withSomeSingT)
import Michelson.Untyped (SomeAnn)
import Morley.Micheline
  (Expression, FromExpression(fromExpression), FromExpressionError, ToExpression(toExpression))
import Util.Typeable ((:~:)(Refl), eqT)

import Stablecoin.Util (aesonOptions)

----------------------------------------------------------------------------
-- ADTs
----------------------------------------------------------------------------

-- | TZIP-16 Metadata big_map.
--
-- See: <https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md#contract-storage>
type MetadataMap big_map = big_map MText ByteString

-- | TZIP-16 Metadata JSON.
--
-- See: <https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md#metadata-json-format>
data Metadata st = Metadata
  { mName :: Maybe Text
  , mDescription :: Maybe Text
  , mVersion :: Maybe Text
  , mLicense :: Maybe License
  , mAuthors :: [Text]
  , mHomepage :: Maybe Text
  , mSource :: Maybe Source
  , mInterfaces :: [Text]
  , mErrors :: [Error]
  , mViews :: [View st]
  }
  deriving stock Generic

data License = License
  { lName :: Text
  , lDetails :: Maybe Text
  }

data Source = Source
  { sLocation :: Text
  , sTools :: [Text]
  }

-- TODO: support dynamic errors
data Error = Error
  { eError :: Expression
  , eExpansion :: Expression
  , eLanguages :: [Text]
  }

data View st = View
  { vName :: Text
  , vDescription :: Maybe Text
  , vPure :: Bool
  , vImplementations :: NonEmpty (ViewImplementation st)
  }
  deriving stock (Generic)

data ViewImplementation st
  = VIMichelsonStorageView (SomeMichelsonStorageView st)
  -- TODO: add support for Rest API Views
  -- https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md#rest-api-views
  deriving stock (Generic)

data SomeMichelsonStorageView st where
  SomeMichelsonStorageView
    :: forall st param ret
    --  . (IsoValue st, IsoValue param, IsoValue ret)
     . (KnownT st, KnownT param, KnownT ret)
    => MichelsonStorageView st param ret
    -> SomeMichelsonStorageView st

-- | A view object that requires access to the contract's storage in order to run,
-- as defined by TZIP-16.
--
-- See: <https://gitlab.com/tzip/tzip/-/blob/eb1da57684599a266334a73babd7ba82dbbbce66/proposals/tzip-16/tzip-16.md#michelson-storage-views>
data MichelsonStorageView st param ret = MichelsonStorageView
  { msvCode :: Instr '[ 'TPair st param] '[ret]
  , msvAnnotations :: [(SomeAnn, Text)]
  }

----------------------------------------------------------------------------
-- JSON serializers/deserializers
----------------------------------------------------------------------------

deriveJSON aesonOptions ''License
deriveJSON aesonOptions ''Source
deriveJSON aesonOptions ''Error
instance KnownT st => ToJSON (Metadata st) where toJSON = genericToJSON aesonOptions
instance KnownT st => FromJSON (Metadata st)  where parseJSON = genericParseJSON aesonOptions
instance KnownT st => ToJSON (View st)  where toJSON = genericToJSON aesonOptions
instance KnownT st => FromJSON (View st)  where parseJSON = genericParseJSON aesonOptions

instance KnownT st => ToJSON (ViewImplementation st) where
  toJSON (VIMichelsonStorageView smsv) =
    object [ "michelson-storage-view" .= smsv ]

instance KnownT st => FromJSON (ViewImplementation st) where
  parseJSON = withObject "ViewImplementation" $ \obj -> do
    smsv <- obj .: "michelson-storage-view"
    pure (VIMichelsonStorageView smsv)


instance ToJSON (SomeMichelsonStorageView st) where
  toJSON (SomeMichelsonStorageView msv) = toJSON msv

instance (KnownT st, KnownT param, KnownT ret) => ToJSON (MichelsonStorageView st param ret) where
  toJSON (MichelsonStorageView code anns) =
    object
      [ "parameter" .= toExpression @T (demote @param)
      , "return-type" .= toExpression @T (demote @ret)
      , "code" .= toExpression code
      , "annotations" .= annsJSON
      ]
    where
      annsJSON = anns <&> \(someAnn, description) ->
        object
          [ "name" .= someAnn
          , "description" .= description
          ]

instance KnownT st => FromJSON (SomeMichelsonStorageView st) where
  parseJSON = withObject "MichelsonStorageView" $ \obj -> do

    annotations <- obj .: "annotations"
    paramT <- obj .: "parameter" >>= evalEither . fromExpression @T
    returnTypeT <- obj .: "return-type" >>= evalEither . fromExpression @T

    withSomeSingT paramT $ \(_ :: Sing param) ->
      withSomeSingT returnTypeT $ \(_ :: Sing ret) -> do
        instr <- obj .: "code" >>= evalEither . fromExpression @(Instr '[ 'TPair st param] '[ret])
        pure $
          SomeMichelsonStorageView @st
            (MichelsonStorageView @st @param @ret instr annotations)
    where
      evalEither :: Either FromExpressionError a -> Parser a
      evalEither (Right a) = pure a
      evalEither (Left err) = fail (pretty err)

----------------------------------------------------------------------------
-- Handling views
----------------------------------------------------------------------------

mkMichelsonStorageView :: [(SomeAnn, Text)] -> Lambda (st, param) ret -> MichelsonStorageView (ToT st) (ToT param) (ToT ret)
mkMichelsonStorageView anns code =
  MichelsonStorageView
    { msvCode = compileLorentz code
    , msvAnnotations = anns
    }

getView :: Metadata st -> Text -> Maybe (View st)
getView metadata viewName = find (\v -> vName v == viewName) (mViews metadata)

interpretView
  :: forall st param ret
   . (IsoValue st, IsoValue param, IsoValue ret)
  => ContractEnv
  -> View (ToT st)
  -> st
  -> param
  -> Either ViewInterpretError ret
interpretView env view_ st param =
  case head (vImplementations view_) of
    VIMichelsonStorageView (SomeMichelsonStorageView (msv :: MichelsonStorageView st' param' ret')) ->
      case (eqT @(ToT param) @param', eqT @(ToT ret) @ret') of
        (Nothing, _) -> Left $ VIEParamTypeMismatch viewName (demote @(ToT param)) (demote @param')
        (_, Nothing) -> Left $ VIEReturnTypeMismatch viewName (demote @(ToT ret)) (demote @ret')
        (Just Refl, Just Refl) ->
          first (VIEMichelsonFailed viewName) $
            interpretMichelsonStorageView @st @param @ret env msv st param
  where
    viewName = vName view_

interpretMichelsonStorageView
  :: (IsoValue st, IsoValue param, IsoValue ret)
  => ContractEnv
  -> MichelsonStorageView (ToT st) (ToT param) (ToT ret)
  -> st
  -> param
  -> Either MichelsonFailed ret
interpretMichelsonStorageView env (MichelsonStorageView code _) st param =
  interpretInstr env code (toVal (st, param) :& RNil) <&> \case
    out :& RNil -> fromVal out

data ViewInterpretError
  = VIEParamTypeMismatch Text T T
  | VIEReturnTypeMismatch Text T T
  | VIEMichelsonFailed Text MichelsonFailed
  deriving stock (Show)

instance Buildable ViewInterpretError where
  build = \case
    VIEParamTypeMismatch viewName expectedT actualT -> unlinesF
      [ "Expected view '" +| viewName |+ "' to have a parameter of type:"
      , build expectedT
      , "But the parameter type was:"
      , build actualT
      ]
    VIEReturnTypeMismatch viewName expectedT actualT -> unlinesF
      [ "Expected view '" +| viewName |+ "' to have the return type:"
      , build expectedT
      , "But the return type was:"
      , build actualT
      ]
    VIEMichelsonFailed viewName err -> unlinesF
      [ "View '" +| viewName |+ "' failed with:"
      , build err
      ]
