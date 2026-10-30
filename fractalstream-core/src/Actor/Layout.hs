{-# language UndecidableInstances, OverloadedStrings, TemplateHaskell #-}
module Actor.Layout
  ( Layout(..)
  , Label(..)
  , TabItem(..)
  , CodeString(..)
  , Dimensions(..)
  , parseDimensions
  , Parsed
  , UIVariable(..)
  , SomeUIValue(..)
  , SomeUIExpr(..)
  , layoutBindings
  , layoutEnv
  , layoutContext
  , layoutContext'
  , nonEmptyString
  , parseType'
  , parseConstant'
  , parseEnv'
  , DynamicValue'
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Data.Color
import Data.DynamicValue
import Data.Codec
import Language.Value
import Language.Value.Parser
import Language.Value.Evaluator (evaluate)

import Data.Aeson hiding (Value)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Read (readMaybe)

newtype Label = Label String
  deriving (Eq, Ord, Show, IsString, FromJSON, ToJSON)

instance Codec Label where codec = aeson

newtype CodeString = CodeString { unCodeString :: String }
  deriving (Show, Eq, FromJSON, ToJSON)

instance Codec CodeString where codec = aeson

data SomeUIValue where
  SomeUIValue :: forall name ty
               . KnownSymbol name
              => Proxy name
              -> TypeProxy ty
              -> Variable (HaskellType ty)
              -> SomeUIValue

data SomeUIExpr where
  SomeUIExpr  :: forall name ty
               . KnownSymbol name
              => Proxy name
              -> TypeProxy ty
              -> Dynamic ParsedValue
              -> SomeUIExpr

type Parsed t = Mapped String (Either String t)

data Layout
  = Vertical (Variable [Layout])
  | Horizontal (Variable [Layout])
  | Panel (Variable Label) (Variable Layout)
  | Tabbed (Variable [TabItem])
  | PlainText (Variable String)
  | Button (Variable String)
  | CheckBox (Variable Label) (Parsed String) (Variable Bool)
  | ColorPicker (Variable Label) (Parsed String) (Parsed Color)
  | Selection (Variable Label) (Parsed String) (Variable Int64) (Variable [String])
  | TextBox (Variable Label) UIVariable

data TabItem = TabItem
  { tiLabel :: Variable Label
  , tiBody  :: Variable Layout
  }

data UIVariable = UIVariable
  { exprName  :: Parsed String
  , exprType  :: Parsed SomeType
  , exprValue :: Parsed SomeValue
  }

nonEmptyString :: String -> Either String String
nonEmptyString = \case
  "" -> Left "This field cannot be empty"
  s  -> Right s

parseType' :: String -> Either String SomeType
parseType' s = left (\err -> ppFullError err s) (parseType s)

parseConstant' :: TypeProxy ty -> String -> Either String (HaskellType ty)
parseConstant' ty s = left (\err -> ppFullError err s) (parseConstant ty s)

parseEnv' :: String -> Either String SomeEnvironment
parseEnv' s =
  bimap (`ppFullError` s) (`withEnvFromMap` SomeEnvironment) (parseEnvironment s)

newtype Dimensions = Dimensions { dimToPair :: (Int, Int) }
  deriving Eq

instance Show Dimensions where show (Dimensions (x, y)) = show x ++ "x" ++ show y

-- | Parse a @WIDTHxHEIGHT@ dimension descriptor (e.g. @400x200@). Exposed
-- so that a caller with a bare 'String' (e.g. a unified-script @\@viewer@
-- header's @size=@ attribute) can reuse the exact same parsing logic as
-- the YAML @size:@ field's 'FromJSON' instance below.
parseDimensions :: String -> Either String Dimensions
parseDimensions s = case Text.splitOn "x" (Text.pack s) of
  [xStr, yStr] ->
    case (,) <$> readMaybe (Text.unpack xStr) <*> readMaybe (Text.unpack yStr) of
      Just dim -> Right (Dimensions dim)
      Nothing  -> Left "could not parse dimension descriptor"
  _ -> Left "expected a dimension descriptor, e.g. 400x200"

instance FromJSON Dimensions where
  parseJSON = withText "dimensions" $ either fail pure . parseDimensions . Text.unpack

instance ToJSON Dimensions where
  toJSON d = String . Text.pack $ show d

instance Codec Dimensions where codec = aeson

layoutEnv :: Layout -> Dynamic (Either String SomeEnvironment)
layoutEnv = fmap (fmap ((`withEnvFromMap` SomeEnvironment) . Map.fromList)) . layoutBindings

layoutContext :: Layout -> Dynamic (Either String (SomeContext DynamicValue))
layoutContext = fmap (\(SomeContext' ctx) -> ctx) . go
  where
    single :: KnownType ty => String -> TypeProxy ty -> Dynamic (HaskellType ty)
           -> Dynamic (SomeContext' DynamicValue)
    single n ty x = fmap (SomeContext' . Right) $ do
      SomeSymbol name <- pure (someSymbolVal n)
      pure (SomeContext $ Bind name ty x EmptyContext)

    go :: Layout -> Dynamic (SomeContext' DynamicValue)
    go = \case
      Vertical   xs -> mconcat <$> (mapM go =<< dyn xs)
      Horizontal xs -> mconcat <$> (mapM go =<< dyn xs)
      Panel _ lo -> go =<< dyn lo
      Tabbed items -> do
        parts <- mapM (dyn . tiBody) =<< dyn items
        mconcat <$> mapM go parts
      PlainText{} -> pure mempty
      Button{}    -> pure mempty
      Selection _ var x _ -> dyn var >>= \case
        Left e -> pure (SomeContext' $ Left e)
        Right v -> single v IntegerType (dyn x)
      CheckBox _ var b -> dyn var >>= \case
        Left e  -> pure (SomeContext' $ Left e)
        Right v -> single v BooleanType (dyn b)
      ColorPicker _ var c -> dyn var >>= \case
        Left e  -> pure (SomeContext' $ Left e)
        Right v -> dyn c >>= \case
          Left e -> pure (SomeContext' $ Left e)
          Right c' -> single v ColorType (pure c')
      TextBox _ UIVariable{..} -> dyn exprValue >>= \case
        Left e -> pure (SomeContext' $ Left e)
        Right (SomeValue env ty v) -> dyn exprName >>= \case
          Left e -> pure (SomeContext' $ Left e)
          Right n -> case env of
            EmptyEnvProxy -> withKnownType ty $ single n ty (pure $ evaluate v EmptyContext)
            _ -> pure (SomeContext' $ Left "expected an empty environment")

data DynamicValue' :: Symbol -> FSType -> Exp Type
type instance Eval (DynamicValue' n t) = Dynamic (Either String (HaskellType t))

layoutContext' :: Layout -> IO (Either String (SomeContext DynamicValue'))
layoutContext' = fmap (\(SomeContext' x) -> x) . go
  where
    single :: KnownType ty => String -> TypeProxy ty -> Dynamic (Either String (HaskellType ty))
           -> SomeContext' DynamicValue'
    single n ty x = case someSymbolVal n of
      SomeSymbol name -> SomeContext' . Right . SomeContext $ Bind name ty x EmptyContext

    go :: Layout -> IO (SomeContext' DynamicValue')
    go = \case
      Vertical   xs -> fmap mconcat . mapM go =<< getDynamic (dyn xs)
      Horizontal xs -> fmap mconcat . mapM go =<< getDynamic (dyn xs)
      Panel _ lo -> getDynamic lo >>= go
      Tabbed items -> do
        parts <- fmap tiBody <$> getDynamic items
        mconcat <$> mapM (go <=< getDynamic) parts
      PlainText{} -> pure mempty
      Button{}    -> pure mempty
      Selection _ var x _ -> getDynamic var <&> \case
        Left e -> SomeContext' $ Left e
        Right v -> single v IntegerType (Right <$> dyn x)
      CheckBox _ var b -> getDynamic var <&> \case
        Left e  -> SomeContext' $ Left e
        Right v -> single v BooleanType (Right <$> dyn b)
      ColorPicker _ var c -> getDynamic var >>= \case
        Left e  -> pure (SomeContext' $ Left e)
        Right v -> pure (single v ColorType (dyn c))
      TextBox _ UIVariable{..} -> do
        getDynamic exprValue >>= \case
          Left e -> pure (SomeContext' $ Left e)
          Right (SomeValue _env ty _v) -> getDynamic exprName >>= \case
            Left e -> pure (SomeContext' $ Left e)
            Right n -> withKnownType ty $ do
              let mk (SomeValue env' ty' v') = case env' of
                    EmptyEnvProxy -> case sameHaskellType ty ty' of
                      Nothing   -> Left ("expected type " ++ ppType ty ++  ", not " ++ ppType ty')
                      Just Refl -> Right (evaluate v' EmptyContext)
                    _ -> Left "expected an empty environment"
              pure $ single n ty (either Left mk <$>dyn exprValue)

layoutBindings :: Layout -> Dynamic (Either String [(String, SomeType)])
layoutBindings = \case
  Vertical   xs -> do
    parts <- dyn xs
    fmap concat . sequence <$> (mapM layoutBindings parts)
  Horizontal xs -> do
    parts <- dyn xs
    fmap concat . sequence <$> (mapM layoutBindings parts)
  Panel _ lo -> layoutBindings =<< dyn lo
  Tabbed items -> do
    parts <- mapM (dyn . tiBody) =<< dyn items
    fmap concat . sequence <$> (mapM layoutBindings parts)
  PlainText{} -> pure (pure [])
  Button{} -> pure (pure [])
  Selection _ var _ _ -> fmap (:[]) <$> (fmap (, SomeType IntegerType) <$> dyn var)
  CheckBox    _ var _ -> fmap (:[]) <$> (fmap (, SomeType BooleanType) <$> dyn var)
  ColorPicker _ var _ -> fmap (:[]) <$> (fmap (, SomeType ColorType)   <$> dyn var)
  TextBox _ var -> do
    name <- dyn (exprName var)
    ty <- dyn (exprType var)
    pure (((,) <$> name <*> ty) <&> (:[]))

