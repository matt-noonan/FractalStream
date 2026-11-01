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
  , Field(..)
  , layoutFields
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

-- | One named field of a configuration panel, as text: what a project
-- file saves of it, and how to put a saved value back.
data Field = Field
  { fieldName      :: String
  , fieldText      :: IO String
    -- ^ The field's value as text: what's typed in a text box (exactly,
    -- so that @e^(2 pi i / 3)@ stays just that), @true@ or @false@ for
    -- a checkbox, or the chosen option of a selection.
  , fieldCanonical :: IO (Maybe String)
    -- ^ The field's value, written the same way however it was typed
    -- (@1/2@ and @0.5@ alike), for telling whether it's changed; or
    -- 'Nothing' if what's typed isn't valid.
  , setFieldText   :: String -> IO (Either String ())
    -- ^ Put a value (as 'fieldText' gives it) into the field, or say why
    -- it doesn't fit, leaving the field as it was.
  , watchField     :: IO () -> IO (IO ())
    -- ^ Run an action whenever the field changes; returns an action
    -- that stops it.
  }

-- | Every named field in a panel, in order.
layoutFields :: Layout -> IO [Field]
layoutFields = \case
  Vertical xs -> concat <$> (mapM layoutFields =<< getDynamic xs)
  Horizontal xs -> concat <$> (mapM layoutFields =<< getDynamic xs)
  Panel _ lo -> layoutFields =<< getDynamic lo
  Tabbed items -> concat <$> (mapM (layoutFields <=< getDynamic . tiBody) =<< getDynamic items)
  PlainText{} -> pure []
  Button{} -> pure []
  CheckBox _ name b -> named name $ \n -> Field
    { fieldName = n
    , fieldText = showBool <$> getDynamic b
    , fieldCanonical = Just . showBool <$> getDynamic b
    , setFieldText = \case
        "true"  -> Right <$> setValue b True
        "false" -> Right <$> setValue b False
        other   -> pure (Left ("expected true or false, not " ++ show other))
    , watchField = \k -> watchDynamic b (const k)
    }
  Selection _ name ix options -> named name $ \n ->
    let chosen = do
          opts <- getDynamic options
          i <- fromIntegral <$> getDynamic ix
          pure (if i >= 0 && i < length opts then Just (opts !! i) else Nothing)
    in Field
      { fieldName = n
      , fieldText = fromMaybe "" <$> chosen
      , fieldCanonical = chosen
      , setFieldText = \choice -> getDynamic options >>= \opts ->
          case lookup choice (zip opts [0 ..]) of
            Just i  -> Right <$> setValue ix i
            Nothing -> pure (Left ("no option named " ++ show choice))
      , watchField = \k -> watchDynamic ix (const k)
      }
  ColorPicker _ name c -> named name $ \n -> textField n c $ \case
    Right col -> Just (toUserString ColorType col)
    Left _ -> Nothing
  TextBox _ UIVariable{..} -> named exprName $ \n -> textField n exprValue $ \case
    Right (SomeValue EmptyEnvProxy ty v) -> Just (toUserString ty (evaluate v EmptyContext))
    _ -> Nothing
  where
    showBool b = if b then "true" else "false"
    named :: Parsed String -> (String -> Field) -> IO [Field]
    named name k = getDynamic (dyn name) <&> \case
      Right n -> [k n]
      Left _  -> []
    -- A field whose value is parsed from its text: setting the text
    -- re-parses it, and a value that doesn't parse is taken back out.
    textField :: String -> Parsed a -> (Either String a -> Maybe String) -> Field
    textField n p canonical = Field
      { fieldName = n
      , fieldText = getDynamic (source p)
      , fieldCanonical = canonical <$> getDynamic (dyn p)
      , setFieldText = \txt -> do
          old <- getDynamic (source p)
          setValue (source p) txt
          getDynamic (dyn p) >>= \case
            Right _ -> pure (Right ())
            Left err -> do
              setValue (source p) old
              pure (Left err)
      , watchField = \k -> watchDynamic (source p) (const k)
      }

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

