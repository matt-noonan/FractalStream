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
  , UIScript(..)
  , SomeUIValue(..)
  , SomeUIExpr(..)
  , layoutBindings
  , layoutEnv
  , layoutToSplices
  , layoutContext
  , layoutContext'
  , nonEmptyString
  , parseType'
  , parseConstant'
  , parseEnv'
  , parseScript'
  , ScriptDependencies
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
import Language.Code hiding (End)
import Language.Code.Parser
import Language.Value.Evaluator (evaluate)
import Language.Parser.SourceRange
import Language.Typecheck

import Data.Aeson hiding (Value)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
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
  | ScriptBox UIScript

data TabItem = TabItem
  { tiLabel :: Variable Label
  , tiBody  :: Variable Layout
  }

instance CodecWith (Dynamic (Either String Splices)) TabItem where
  codecWith_ splices = do
    title <-tiLabel-< key "title"
    body  <-tiBody-< codecWith splices
    build TabItem title body

-- Newtypes for orphan instances
newtype SomeEnvironment' = SomeEnvironment' SomeEnvironment
newtype SomeType'        = SomeType'        SomeType

instance Optionally SomeEnvironment' where
  makeDefault = pure (SomeEnvironment' $ SomeEnvironment EmptyEnvProxy)
  isDefault (SomeEnvironment' (SomeEnvironment env)) = pure $
    case env of { EmptyEnvProxy -> True; _ -> False }

instance Codec SomeEnvironment' where codec = aeson

instance FromJSON SomeEnvironment' where
  parseJSON = withObject "SomeEnvironment" $ \o -> do
    (`withEnvFromMap` (SomeEnvironment' . SomeEnvironment))
      . Map.fromList
      . fmap (first Key.toString)
      . KM.toList <$> traverse (fmap (\(SomeType' t) -> t) . parseJSON) o

instance ToJSON SomeEnvironment' where
  toJSON (SomeEnvironment' (SomeEnvironment e)) =
    object (map (bimap Key.fromString (toJSON . SomeType')) . Map.toList $ envToMap e)

instance FromJSON SomeType' where
  parseJSON = withText "SomeType" $ \t -> do
    let src = Text.unpack t
    case parseType src of
      Left err -> fail (ppFullError err src)
      Right ty -> pure (SomeType' ty)

instance ToJSON SomeType' where
  toJSON (SomeType' (SomeType ty)) = String (Text.pack $ ppType ty)

instance CodecWith (Dynamic (Either String Splices)) Layout where
  codecWith_ (splices :: Part b a (Dynamic (Either String Splices))) = do
    match
      [ Fragment Vertical (\case { Vertical x -> Just x; _ -> Nothing }) $
        field "vertical-contents" (codecWith splices)
      , Fragment Horizontal (\case { Horizontal x -> Just x; _ -> Nothing }) $
        field "horizontal-contents" (codecWith splices)
      , Fragment (uncurry Panel) (\case { Panel l x -> Just (l, x); _ -> Nothing }) $
        field "panel" $ do
          title <-fst-< key "title"
          body  <-snd-< codecWith splices
          build (,) title body
      , Fragment Tabbed (\case { Tabbed ts -> Just ts; _ -> Nothing }) $
        field "tabbed" (codecWith splices)
      , Fragment PlainText (\case { PlainText x -> Just x; _ -> Nothing }) $
        key "text"
      , Fragment Button (\case { Button x -> Just x; _ -> Nothing }) $
        key "button"
      , Fragment (uncurry TextBox) (\case { TextBox l x -> Just (l, x); _ -> Nothing }) $
        field "text-entry" $ do
          label <-fst-< key "label"
          body  <-snd-< codecWith splices
          build (,) label body
      , Fragment (uncurry3 CheckBox) (\case { CheckBox l v x -> Just (l, v, x); _ -> Nothing }) $
        field "checkbox" $ do
          label <-(\(x,_,_) -> x)-< key "label"
          var   <-(\(_,y,_) -> y)-< field "variable" (mapped codec $ \_ -> pure nonEmptyString)
          body  <-(\(_,_,z) -> z)-< key "value"
          build (,,) label var body
      , Fragment (uncurry3 ColorPicker) (\case { ColorPicker l v x -> Just (l, v, x); _ -> Nothing }) $
        field "color-picker" $ do
          label <-(\(x,_,_) -> x)-< key "label"
          var   <-(\(_,y,_) -> y)-< field "variable" (mapped codec $ \_ -> pure nonEmptyString)
          body  <-(\(_,_,z) -> z)-< field "value" (mapped codec $ \_ -> pure (parseConstant' ColorType))
          build (,,) label var body
      , Fragment (uncurry4 Selection) (\case { Selection l v i xs -> Just (l, v, i, xs); _ -> Nothing }) $
        field "selection" $ do
          label <-(\(x,_,_,_) -> x)-< key "label"
          var   <-(\(_,y,_,_) -> y)-< field "variable" (mapped codec $ \_ -> pure nonEmptyString)
          ix    <-(\(_,_,z,_) -> z)-< key "current-choice"
          opts  <-(\(_,_,_,w) -> w)-< key "choices"
          build (,,,) label var ix opts
      , Fragment ScriptBox (\case { ScriptBox x -> Just x; _ -> Nothing }) $
        field "code" (codecWith splices)
      ]

uncurry3 :: (a1 -> a2 -> a3 -> b) -> (a1, a2, a3) -> b
uncurry3 f (x, y, z) = f x y z

uncurry4 :: (a1 -> a2 -> a3 -> a4 -> b) -> (a1, a2, a3, a4) -> b
uncurry4 f (w, x, y, z) = f w x y z

data UIVariable = UIVariable
  { exprName  :: Parsed String
  , exprType  :: Parsed SomeType
  , exprEnv   :: Variable SomeEnvironment
  , exprValue :: Parsed SomeValue
  }

data UIScript = UIScript
  { scriptName :: Parsed String
  , scriptEnv  :: Variable SomeEnvironment
  , scriptCode :: Mapped CodeString (Either (SourceRange, String) SomeCode)
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

type ScriptDependencies =
  (Dynamic (Either String (SomeContext DynamicValue)),
   Dynamic (Either String Splices))

parseScript' :: SomeEnvironment
             -> Splices
             -> CodeString
             -> Either (SourceRange, String) SomeCode
parseScript' (SomeEnvironment env) splices (CodeString src) = withEnvironment env $
    bimap (errorLocation &&& unlines . pp) SomeCode (parseCode env splices src)

instance CodecWith (Dynamic (Either String Splices)) UIVariable where
  codecWith_ ctx = do
    ty  <-exprType-< field "type" $ mapBy parseType'
    env <-coerce . exprEnv-< optionalKey @(Variable SomeEnvironment') "environment"

    -- Variable name is required and must be non-empty, and not
    -- already present in the environment
    name <-exprName-< mapped (key "variable") $ \use -> (<$> dyn (use env)) $
      \(SomeEnvironment' (SomeEnvironment e)) s -> do
        void (nonEmptyString s)
        case someSymbolVal s of
           SomeSymbol n -> case lookupEnv' n e of
             Absent' _ -> pure ()
             _ -> Left ("The variable name `" ++ s ++ "` is already defined in the environment.")
        pure s

    expr <-exprValue-< mapped (key "value") $ \use -> do
      let f = \mt (SomeEnvironment' (SomeEnvironment e)) msplices src -> do
            SomeType t <- case mt of
              Left _ -> Left "Cannot check this value until the error in its type is fixed."
              Right t -> pure t
            splices <- case msplices of
              Left _ -> Left "Cannot check this value until the error in the Setup is fixed."
              Right s -> pure s
            withEnvironment e $ withKnownType t $ do
              bimap (`ppFullError` src) (SomeValue e t) (parseInputValue (valueSplices splices) src)
      f <$> dyn (use ty) <*> dyn (use env) <*> use ctx

    build UIVariable name ty (coerce <$> env) expr

instance CodecWith (Dynamic (Either String Splices)) UIScript where
  codecWith_ ctx = do
    env <-scriptEnv-< coerce <$> optionalKey @(Variable SomeEnvironment') "environment"

    -- Variable name is required and must be non-empty, and not
    -- already present in the environment
    name <-scriptName-< mapped (key "variable") $ \use -> (<$> dyn (use env)) $ \(SomeEnvironment e) s -> do
      void (nonEmptyString s)
      case someSymbolVal s of
        SomeSymbol n -> case lookupEnv' n e of
          Absent' _ -> pure ()
          _ -> Left ("The variable name `" ++ s ++ "` is already defined in the environment.")
      pure s

    code <-scriptCode-< mapped (key "value") $ \use ->
      (either (\err _ -> Left (NoSourceRange, "Cannot check this script until another error is fixed: " ++ err))
              (uncurry parseScript'))
      <$> ((\(x,y) -> (x,) <$> y) <$> (((,) <$> dyn (use env)) <*> dyn (use ctx)))

    build UIScript name env code

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
      ScriptBox{} -> pure (SomeContext' $ Left "Unexpected script")
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
      ScriptBox{} -> pure (SomeContext' $ Left "Unexpected script")
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
  ScriptBox {} -> pure (pure [])
  Selection _ var _ _ -> fmap (:[]) <$> (fmap (, SomeType IntegerType) <$> dyn var)
  CheckBox    _ var _ -> fmap (:[]) <$> (fmap (, SomeType BooleanType) <$> dyn var)
  ColorPicker _ var _ -> fmap (:[]) <$> (fmap (, SomeType ColorType)   <$> dyn var)
  TextBox _ var -> do
    name <- dyn (exprName var)
    ty <- dyn (exprType var)
    pure (((,) <$> name <*> ty) <&> (:[]))

layoutToSplices :: Layout -> Dynamic (Either String Splices)
layoutToSplices = fmap (fmap finish) . go
  where
    finish :: [Either (String, ParsedCode) (String, ParsedValue)] -> Splices
    finish = (\(xs, ys) -> Splices (Map.fromList xs) (Map.fromList ys)) . partitionEithers

    go :: Layout -> Dynamic (Either String [Either (String, ParsedCode) (String, ParsedValue)])
    go = \case
      Vertical xs -> do
        parts <- dyn xs
        fmap concat . sequence <$> mapM go parts
      Horizontal xs -> do
        parts <- dyn xs
        fmap concat . sequence <$> mapM go parts
      Panel _ lo -> go =<< dyn lo
      Tabbed items -> do
        parts <- mapM (dyn . tiBody) =<< dyn items
        fmap concat . sequence <$> mapM go parts
      PlainText{} -> pure (pure [])
      Button{} -> pure (pure [])
      ScriptBox UIScript{..} -> do
        name   <- dyn scriptName
        script <- dyn (source scriptCode) <&> \(CodeString src) ->
          bimap (`ppFullError` src) id (parseParsedCode noSplices src)
        pure (Left <$> ((,) <$> name <*> script) <&> (:[]))
      CheckBox _ var val -> do
        name <- dyn var
        value <- dyn val <&> \b ->
          let src = if b then "true" else "false"
          in bimap (`ppFullError` src) id (parseParsedValue Map.empty src)
        pure (Right <$> ((,) <$> name <*> value) <&> (:[]))
      ColorPicker _ var val -> do
        name <- dyn var
        value <- dyn (source val) <&> \src ->
          bimap (`ppFullError` src) id (parseParsedValue Map.empty src)
        pure (Right <$> ((,) <$> name <*> value) <&> (:[]))
      Selection _ var ix _ -> do
        name <- dyn var
        v <- dyn ix
        let value = Right $ ParsedValue NoSourceRange $ \case
              IntegerType -> pure (Const (Scalar IntegerType v))
              t -> throwError (Surprise NoSourceRange "a selection value" "an integer" (Expected $ an t))
        pure (Right <$> ((,) <$> name <*> value) <&> (:[]))
      TextBox _ UIVariable{..} -> do
        name  <- dyn exprName
        value <- dyn (source exprValue) <&> \src ->
          bimap (`ppFullError` src) id (parseParsedValue Map.empty src)
        pure (Right <$> ((,) <$> name <*> value) <&> (:[]))
