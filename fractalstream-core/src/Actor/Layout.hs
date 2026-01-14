{-# language UndecidableInstances, OverloadedStrings, TemplateHaskell #-}
module Actor.Layout
  ( Layout(..)
  , Label(..)
  , CodeString(..)
  , Dimensions(..)
  , basicSplices
  , Parsed
  , UIVariable(..)
  , UIScript(..)
  , layoutBindings
  , layoutEnv
  , layoutToSplices
  , nonEmptyString
  , parseType'
  , parseConstant'
  , parseEnv'
  , parseScript'
{-
  , Dimensions(..)
  , allBindings
  , allBindingVars
  , extractAllBindings
  , Dummy(..)
  , unDummy
  , ConfigVar(..)
  , StringOrNumber(..)
  , CodeString(..)
  -- *
  , parseLayout
  , parseToHaskellValue
  , setTextOnly
  , setToParsed
  , Expression(..)
  , ConstantExpression(..)
  , ConstantExpression'(..)
  , allocateUIExpressions
  , allocateUIConstants
  , withSplices
  , withDynamicBindings
  , mapLayout
  , mapLayoutA
  , basicSplices
-}
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
import Language.Value.Typecheck ( internalVanishingRadius
                                , internalEscapeRadius
                                , internalIterationLimit
                                , internalIterations
                                , internalStuck )
import Language.Parser.SourceRange
import Language.Typecheck

import Data.Aeson hiding (Value)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Read (readMaybe)

newtype Label = Label String
  deriving (Show, IsString, FromJSON, ToJSON)

instance Codec Label where codec = aeson

newtype CodeString = CodeString { unCodeString :: String }
  deriving (Show, FromJSON, ToJSON)

instance Codec CodeString where codec = aeson

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
  | TextBox (Variable Label) UIVariable
  | ScriptBox UIScript

data TabItem = TabItem
  { tiLabel :: Variable Label
  , tiBody  :: Variable Layout
  }

instance CodecWith (Dynamic Splices) TabItem where
  codecWith_ splices = do
    title <-tiLabel-< key "title"
    body  <-tiBody-< codecWith splices
    build TabItem title body

instance CodecWith (Dynamic Splices) Layout where
  codecWith_ (splices :: Part b a (Dynamic Splices)) =
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
          var   <-(\(_,y,_) -> y)-< mapped codec $ \_ -> pure nonEmptyString
          body  <-(\(_,_,z) -> z)-< codec
          build (,,) label var body
      , Fragment (uncurry3 ColorPicker) (\case { ColorPicker l v x -> Just (l, v, x); _ -> Nothing }) $
        field "color-picker" $ do
          label <-(\(x,_,_) -> x)-< key "label"
          var   <-(\(_,y,_) -> y)-< mapped codec $ \_ -> pure nonEmptyString
          body  <-(\(_,_,z) -> z)-< mapped codec $ \_ -> pure (parseConstant' ColorType)
          build (,,) label var body
      , Fragment ScriptBox (\case { ScriptBox x -> Just x; _ -> Nothing }) $
        field "code" (codecWith splices)
      ]

uncurry3 :: (a1 -> a2 -> a3 -> b) -> (a1, a2, a3) -> b
uncurry3 f (x, y, z) = f x y z

data UIVariable = UIVariable
  { exprName  :: Parsed String
  , exprType  :: Parsed SomeType
  , exprEnv   :: Parsed SomeEnvironment
  , exprValue :: Parsed SomeValue
  }

data UIScript = UIScript
  { scriptName :: Parsed String
  , scriptEnv  :: Parsed SomeEnvironment
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

parseScript' :: Either String SomeEnvironment
             -> Splices
             -> CodeString
             -> Either (SourceRange, String) SomeCode
parseScript' menv splices (CodeString src) = do
  SomeEnvironment env <- case menv of
    Left _ -> Left (NoSourceRange,
                     "Cannot check this script until the error in its environment is fixed.")
    Right e -> pure e
  withEnvironment env $
    bimap (errorLocation &&& unlines . pp) SomeCode (parseCode env splices src)

instance CodecWith (Dynamic Splices) UIVariable where
  codecWith_ ctx = do
    ty  <-exprType-< field "type" $ mapBy parseType'
    env <-exprEnv-<  field "environment"  $ mapBy parseEnv'

    -- Variable name is required and must be non-empty, and not
    -- already present in the environment
    name <-exprName-< mapped (key "variable") $ \use -> (<$> use env) $ \me s -> do
      void (nonEmptyString s)
      case me of
        Left _ -> Left "Cannot check this value until the errors in the environment are fixed."
        Right (SomeEnvironment e) -> case someSymbolVal s of
          SomeSymbol n -> case lookupEnv' n e of
            Absent' _ -> pure ()
            _ -> Left ("The variable name `" ++ s ++ "` is already defined in the environment.")
      pure s

    expr <-exprValue-< mapped (key "value") $ \use ->
      let f = \mt me splices src -> do
            SomeType t <- case mt of
              Left _ -> Left "Cannot check this value until the error in its type is fixed."
              Right t -> pure t
            SomeEnvironment e <- case me of
              Left _ -> Left "Cannot check this value until the error in its environment is fixed."
              Right e -> pure e
            withEnvironment e $ withKnownType t $ do
              bimap (`ppFullError` src) (SomeValue e t) (parseInputValue splices src)
      in f <$> use ty <*> use env <*> use ctx

    build UIVariable name ty env expr

instance CodecWith (Dynamic Splices) UIScript where
  codecWith_ ctx = do
    env <-scriptEnv-< field "environment" $ mapBy parseEnv'

    -- Variable name is required and must be non-empty, and not
    -- already present in the environment
    name <-scriptName-< mapped (key "variable") $ \use -> (<$> use env) $ \me s -> do
      void (nonEmptyString s)
      case me of
        Left _ -> Left "Cannot check this value until the errors in the environment are fixed."
        Right (SomeEnvironment e) -> case someSymbolVal s of
          SomeSymbol n -> case lookupEnv' n e of
            Absent' _ -> pure ()
            _ -> Left ("The variable name `" ++ s ++ "` is already defined in the environment.")
      pure s

    code <-scriptCode-< mapped (key "value") $ \use -> parseScript' <$> use env <*> use ctx

    build UIScript name env code

newtype Dimensions = Dimensions { dimToPair :: (Int, Int) }

instance Show Dimensions where show (Dimensions (x, y)) = show x ++ "x" ++ show y

instance FromJSON Dimensions where
  parseJSON = withText "dimensions" $ \txt -> do
    case Text.splitOn "x" txt of
      [xStr, yStr] -> do
        case (,) <$> readMaybe (Text.unpack xStr) <*> readMaybe (Text.unpack yStr) of
          Just dim -> pure (Dimensions dim)
          Nothing  -> fail "could not parse dimension descriptor"
      _ -> fail "expected a dimension descriptor, e.g. 400x200"

instance ToJSON Dimensions where
  toJSON d = String . Text.pack $ show d

instance Codec Dimensions where codec = aeson

layoutEnv :: Layout -> Dynamic (Either String SomeEnvironment)
layoutEnv = fmap (fmap ((`withEnvFromMap` SomeEnvironment) . Map.fromList)) . layoutBindings

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
  CheckBox    _ var _ -> fmap (:[]) <$> (fmap (, SomeType BooleanType) <$> dyn var)
  ColorPicker _ var _ -> fmap (:[]) <$> (fmap (, SomeType ColorType)   <$> dyn var)
  TextBox _ var -> do
    name <- dyn (exprName var)
    ty <- dyn (exprType var)
    pure (((,) <$> name <*> ty) <&> (:[]))

layoutToSplices :: Layout -> Dynamic (Either String Splices)
layoutToSplices = fmap (fmap Map.fromList) . go
  where
    go :: Layout -> Dynamic (Either String [(String, ParsedValue)])
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
      ScriptBox{} -> pure (pure [])
      CheckBox _ var val -> do
        name <- dyn var
        value <- dyn val <&> \b ->
          let src = if b then "true" else "false"
          in bimap (`ppFullError` src) id (parseParsedValue Map.empty src)
        pure (((,) <$> name <*> value) <&> (:[]))
      ColorPicker _ var val -> do
        name <- dyn var
        value <- dyn (source val) <&> \src ->
          bimap (`ppFullError` src) id (parseParsedValue Map.empty src)
        pure (((,) <$> name <*> value) <&> (:[]))
      TextBox _ ve -> do
        name <- dyn (exprName ve)
        value <- dyn (source (exprValue ve)) <&> \src ->
          bimap (`ppFullError` src) id (parseParsedValue Map.empty src)
        pure (((,) <$> name <*> value) <&> (:[]))

{-
allBindings :: Layout Dummy -> [(String, SomeType)]
allBindings = map (\ConfigVar{..} -> (varVariable, varType)) . allBindingVars

allBindingVars :: Layout Dummy -> [ConfigVar]
allBindingVars = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ d -> [unDummy d]
      CheckBox _ d -> [unDummy d]
      ColorPicker _ d -> [unDummy d]
      PlainText _ -> []
      Button {} -> []
      ScriptBox d -> [unDummy d]
-}

{-
data Layout f
  = Vertical [Layout f]
  | Horizontal [Layout f]
  | Panel String (Layout f)
  | Tabbed [(String, Layout f)]
  | TextBox Label (f String)
  | CheckBox Label (f Bool)
  | ColorPicker Label (f Color)
  | PlainText String
  | Button String
  | Multiline (f CodeString)

deriving instance (Show (f String), Show (f Bool), Show (f Color), Show (f CodeString))
  => Show (Layout f)
-}

{-
mapLayoutA :: Applicative m
           => (forall t. f t -> m (g t))
           -> Layout f
           -> m (Layout g)
mapLayoutA f = go
  where
    go = \case
      Vertical xs -> Vertical <$> traverse go xs
      Horizontal xs -> Horizontal <$> traverse go xs
      Panel n x -> Panel n <$> go x
      Tabbed xs -> Tabbed <$> traverse (\(s, x) -> (s,) <$> go x) xs
      TextBox n x -> TextBox n <$> f x
      CheckBox n x -> CheckBox n <$> f x
      ColorPicker n x -> ColorPicker n <$> f x
      PlainText s -> pure (PlainText s)
      Button s -> pure (Button s)
      Multiline x -> Multiline <$> f x

mapLayout :: (forall t. f t -> g t)
          -> Layout f
          -> Layout g
mapLayout f = runIdentity . mapLayoutA (Identity . f)

data Dimensions = Dimensions (Int, Int)

instance FromJSON Dimensions where
  parseJSON = withText "dimensions" $ \txt -> do
    case Text.splitOn "x" txt of
      [xStr, yStr] -> do
        case (,) <$> readMaybe (Text.unpack xStr) <*> readMaybe (Text.unpack yStr) of
          Just dim -> pure (Dimensions dim)
          Nothing  -> fail "could not parse dimension descriptor"
      _ -> fail "expected a dimension descriptor, e.g. 400x200"

data Dummy t where
  Dummy :: forall t. ConfigVar -> Dummy t
  DummyCode :: ConfigVar -> Dummy CodeString

unDummy :: Dummy t -> ConfigVar
unDummy = \case
  Dummy x -> x
  DummyCode x -> x

deriving instance Show (Dummy t)

data ConfigVar = ConfigVar
  { varValue :: String
  , varType :: SomeType
  , varEnv :: Map String SomeType
  , varVariable :: Variable String
  }
  deriving Show

instance FromJSON (Layout Dummy) where
  parseJSON = withObject "layout" parseLayout

newtype SomeType' = SomeType' { getSomeType :: SomeType }

instance FromJSON SomeType' where
  parseJSON = withText "type" $ \txt -> do
    case parseType (Text.unpack txt) of
      Left err -> fail (ppFullError err $ Text.unpack txt)
      Right t  -> pure (SomeType' t)

parseLayout :: Object -> JSON.Parser (Layout Dummy)
parseLayout o
  =   (Vertical <$> (o .: "vertical-contents"))
  <|> (Horizontal <$> (o .: "horizontal-contents"))
  <|> (uncurry Panel <$> (titled =<< (o .: "panel")))
  <|> (Tabbed <$> ((o .: "tabbed") >>= mapM titled))
  <|> (textBoxLayout =<< (o .: "text-entry"))
  <|> (checkBoxLayout =<< (o .: "checkbox"))
  <|> (colorPickerLayout =<< (o .: "color-picker"))
  <|> (PlainText <$> (o .: "text"))
  <|> (Button <$> (o .: "button"))
  <|> (multilineLayout =<< (o .: "code"))
  <|> fail "bad layout description"
 where
   titled p = (,) <$> (p .: "title") <*> parseLayout p

   textBoxLayout p = do
     lab <- p .: "label"
     StringOrNumber varValue <- p .: "value"
     SomeType' varType <- p .: "type"
     varVariable <- p .: "variable"
     varEnv <- Map.map getSomeType <$> (p .:? "environment" .!= Map.empty)
     pure (TextBox lab (Dummy ConfigVar{..}))

   checkBoxLayout p = do
     lab <- p .: "label"
     val <- p .: "value"
     let varValue = if val then "true" else "false"
         varType = SomeType BooleanType
         varEnv = Map.empty
     varVariable <- p .: "variable"
     pure (CheckBox lab (Dummy ConfigVar{..}))

   colorPickerLayout p = do
     lab <- p .: "label"
     varValue <- p .: "value"
     varVariable <- p .: "variable"
     let varType = SomeType ColorType
         varEnv = Map.empty
     pure (ColorPicker lab (Dummy ConfigVar{..}))

   multilineLayout p = do
     varVariable <- p .: "variable"
     varEnv <- Map.map getSomeType <$> (p .:? "environment" .!= Map.empty)
     let varType = SomeType TextType
     varValue <- p .: "value"
     pure (Multiline (Dummy ConfigVar{..}))


extractAllBindings :: (forall t. f t -> a)
                   -> Layout f
                   -> [a]
extractAllBindings extractor = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ x -> [extractor x]
      CheckBox _ x -> [extractor x]
      ColorPicker _ x -> [extractor x]
      PlainText _ -> []
      Button {} -> []
      Multiline x -> [extractor x]

parseToHaskellValue :: forall env ty
                     . Context HaskellTypeOfBinding env
                    -> TypeProxy ty
                    -> String
                    -> Either String (HaskellType ty)
parseToHaskellValue ctx ty input =
  withKnownType ty $
  withEnvironment (contextToEnv ctx) $
  case parseInputValue @env @ty Map.empty input of
    Left err -> Left (ppFullError err input)
    Right v  -> pure (evaluate v ctx)

setTextOnly :: forall ty
             . TypeProxy ty
            -> String
            -> UIValue (String, HaskellType ty)
            -> IO (Maybe String)
setTextOnly _ s ui = do
  modifyUIValue ui (\(_, v) -> (s, v))
  pure Nothing

setToParsed :: forall ty
             . (TypeProxy ty -> String -> Either (Either ParseError TCError) (HaskellType ty))
            -> TypeProxy ty
            -> String
            -> Mapped String (Either String (HaskellType ty))
            -> IO (Maybe String)
setToParsed parser ty input ui = do
  case parser ty input of
    Left err -> pure (Just $ ppFullError err input)
    Right v  -> setUIValue ui (input, v) >> pure Nothing


{-
instance Dynamic Expression where
  getDynamic = \case
    BoolExpression _ b -> getDynamic b
    ColorExpression _ c -> getDynamic c
    Expression _ _ _ v -> fst <$> getDynamic v
    ScriptExpression _ _ e -> do
      x <- fst <$> getDynamic e
      putStrLn ("Script expression:\n" ++ unCodeString x)
      return x

  setDynamic d new = case d of
    BoolExpression _ b -> setDynamic b new
    ColorExpression _ c -> setDynamic c new
    ScriptExpression _ env e ->
      withEnvironment env $ do
        case parseCode env basicSplices (unCodeString new) of
          Left err -> pure (Just $ ppFullError err (unCodeString new))
          Right newC -> do
            setDynamic e (new, newC)
            pure Nothing
    Expression _ (env :: EnvironmentProxy env) (ty :: TypeProxy ty) v ->
      withEnvironment env $ withKnownType ty $ do
        let new' = case ty of
              TextType -> show new
              _  -> new
        case parseInputValue @env @ty basicSplices new' of
          Left err   -> pure (Just $ ppFullError err new)
          Right newV -> do
            setDynamic v (new, newV)
            pure Nothing

  listenWith d action = case d of
    BoolExpression _ b -> listenWith b action
    ColorExpression _ c -> listenWith c action
    ScriptExpression _ _ e -> listenWith e (\old new -> action (fst old) (fst new))
    Expression _ _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))
-}

data ConstantExpression t where
  ConstantExpression :: forall ty
                      . String
                     -> TypeProxy ty
                     -> Mapped String (Either String (HaskellType ty))
                     -> ConstantExpression String
  ConstantBoolExpression :: String -> UIValue Bool -> ConstantExpression Bool
  ConstantColorExpression :: String -> UIValue Color -> ConstantExpression Color
  ConstantScriptExpression :: String -> UIValue String -> ConstantExpression CodeString

{-
instance Dynamic ConstantExpression where
  getDynamic = \case
    ConstantBoolExpression _ b -> getDynamic b
    ConstantColorExpression _ c -> getDynamic c
    ConstantScriptExpression _ e -> CodeString <$> getDynamic e
    ConstantExpression _ _ v -> fst <$> getDynamic v

  setDynamic d new = case d of
    ConstantBoolExpression _ b -> setDynamic b new
    ConstantColorExpression _ c -> setDynamic c new
    ConstantScriptExpression _ e -> setDynamic e (unCodeString new)
    ConstantExpression _ (ty :: TypeProxy ty) v ->
      withKnownType ty $ do
      let new' = case ty of
                   TextType -> show new
                   _  -> new
      case parseInputValue @'[] @ty Map.empty new' of
        Left err   -> pure (Just $ ppFullError err new)
        Right newV -> do
          setDynamic v (new, evaluate newV EmptyContext)
          pure Nothing

  listenWith d action = case d of
    ConstantBoolExpression _ b -> listenWith b action
    ConstantColorExpression _ c -> listenWith c action
    ConstantScriptExpression _ e -> listenWith e (coerce action)
    ConstantExpression _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))
-}

allocateUIExpressions :: Layout Dummy
                      -> ExceptT String IO (Layout Expression)
allocateUIExpressions = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab d ->
        let ConfigVar{..} = unDummy d
        in case varType of
             SomeType (ty :: TypeProxy ty) -> withKnownType ty $
               withEnvFromMap varEnv $ \(env :: EnvironmentProxy env) ->
                 withEnvironment env $
                   case parseInputValue @env @ty basicSplices varValue of
                     Left err -> throwError (ppFullError err varValue)
                     Right v  ->
                       TextBox lab . Expression varVariable env ty
                       <$> newUIValue (varValue, v)

      CheckBox lab d ->
        let ConfigVar{..} = unDummy d
        in case parseInputValue @'[] @'BooleanT basicSplices varValue of
             Left err -> throwError (ppFullError err varValue)
             Right v  -> CheckBox lab . BoolExpression varVariable
                         <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab d ->
        let ConfigVar{..} = unDummy d
        in case parseInputValue @'[] @'ColorT basicSplices varValue of
             Left err -> throwError (ppFullError err varValue)
             Right v  -> ColorPicker lab . ColorExpression varVariable
                         <$> newUIValue (evaluate v EmptyContext)

      PlainText txt -> pure (PlainText txt)

      Button txt -> pure (Button txt)

      Multiline d ->
        let ConfigVar{..} = unDummy d
        in withEnvFromMap varEnv $ \env -> withEnvironment env $
             case parseCode env basicSplices varValue of
               Left err -> throwError (ppFullError err varValue)
               Right c -> Multiline . ScriptExpression varVariable env
                          <$> newUIValue (CodeString varValue, c)

allocateUIConstants :: Layout Dummy
                    -> ExceptT String IO (Layout ConstantExpression)
allocateUIConstants = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy ConfigVar{..}) -> case varType of
        SomeType (ty :: TypeProxy ty) -> withKnownType ty $
          case parseInputValue @'[] @ty Map.empty varValue of
            Left err -> throwError (ppFullError err varValue)
            Right v  -> TextBox lab . ConstantExpression varVariable ty
                        <$> newUIValue (varValue, evaluate v EmptyContext)

      CheckBox lab (Dummy ConfigVar{..}) ->
        case parseInputValue @'[] @'BooleanT Map.empty varValue of
          Left err -> throwError (ppFullError err varValue)
          Right v  -> CheckBox lab . ConstantBoolExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy ConfigVar{..}) ->
        case parseInputValue @'[] @'ColorT Map.empty varValue of
          Left err -> throwError (ppFullError err varValue)
          Right v  -> ColorPicker lab . ConstantColorExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      PlainText txt -> pure (PlainText txt)

      Button txt -> pure (Button txt)

      Multiline _ -> error "TODO: allocateUIConstants for Multiline"

withSplices :: forall t
             . Layout Expression
            -> (Splices -> IO t) --(forall splices. Context Splice splices -> IO t)
            -> IO t
withSplices lo action =
    go [] (catMaybes $ extractAllBindings toSomeUIExpr lo)
  where
    go :: [(String, ParsedValue)] -> [SomeUIExpr] -> IO t
    go splices = \case
      [] -> action (Map.fromList splices)
      (SomeUIExpr name _ty getExpr : etc) -> do
        e <- getExpr
        go ((symbolVal name, e) : splices) etc

    unsafeFromRight :: Either a b -> b
    unsafeFromRight = \case
      Left{} -> error "Left"
      Right x -> x

    toSomeUIExpr :: forall a. Expression a -> Maybe SomeUIExpr
    toSomeUIExpr = \case
      Expression nameStr _ ty v -> Just $
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ty (unsafeFromRight . parseParsedValue Map.empty . fst
                                <$> getDynamic v)
      BoolExpression nameStr b -> Just $
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name BooleanType
            (unsafeFromRight . parseParsedValue Map.empty . showValue BooleanType <$>
             getDynamic b)
      ColorExpression nameStr b -> Just $
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ColorType
            (unsafeFromRight . parseParsedValue Map.empty . showValue ColorType <$>
             getDynamic b)
      ScriptExpression {} -> Nothing -- can't build a splice value from a script

{-
withSplices lo action = go EmptyContext (extractAllBindings toSomeUIExpr lo)
  where
    go :: forall splices. Context Splice splices -> [SomeUIExpr] -> IO t
    go ctx = \case
      [] -> action ctx
      (SomeUIExpr name ty getExpr : etc) ->
        case lookupEnv name ty (contextToEnv ctx) of
          Absent proof -> recallIsAbsent proof $ do
            expr <- getExpr
            go (Bind name ty expr ctx) etc
          _ -> error ("`" ++ symbolVal name ++ "` is re-defined")

    pValue :: String -> U.Value
    pValue input = case parseUntypedValue input of
      Left err -> error (show err)
      Right v  -> v

    toSomeUIExpr :: forall a. Expression a -> SomeUIExpr
    toSomeUIExpr = \case
      Expression nameStr _ ty v ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ty (pValue . fst <$> getDynamic v)
      BoolExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name BooleanType (pValue . showValue BooleanType <$> getDynamic b)
      ColorExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIExpr name ColorType (pValue . showValue ColorType <$> getDynamic b)
-}

withDynamicBindings :: forall t
                     . Layout ConstantExpression
                    -> (forall env. Context DynamicValue env -> t)
                    -> t
withDynamicBindings lo action =
    go EmptyContext (extractAllBindings toSomeUIValue lo)
  where
    go :: forall env. Context DynamicValue env -> [SomeUIValue] -> t
    go ctx [] = action ctx
    go ctx ( (SomeUIValue name ty dyn) : xs ) =
      case lookupEnv name ty (contextToEnv ctx) of
        Absent proof -> recallIsAbsent proof
                        $ go (Bind name ty dyn ctx) xs
        _ -> error ("`" ++ symbolVal name ++ "` is re-defined")

    toSomeUIValue :: forall a. ConstantExpression a -> SomeUIValue
    toSomeUIValue = \case
      ConstantExpression nameStr ty v ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name ty (SomeDynamic (ConstantExpression' ty v))
      ConstantBoolExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name BooleanType (SomeDynamic b)
      ConstantColorExpression nameStr c ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name ColorType (SomeDynamic c)
      ConstantScriptExpression nameStr b ->
        case someSymbolVal nameStr of
          SomeSymbol name ->
            SomeUIValue name TextType (SomeDynamic b)

data ConstantExpression' t where
  ConstantExpression' :: forall ty t
                      . (t ~ HaskellType ty)
                     => TypeProxy ty
                     -> Mapped String (Either String t)
                     -> ConstantExpression' t
{-
instance Dynamic ConstantExpression' where
  getDynamic (ConstantExpression' _ d) =
    snd <$> getDynamic d

  setDynamic (ConstantExpression' ty d) v =
    setDynamic d (showValue ty v, v)

  listenWith (ConstantExpression' _ d) action =
    listenWith d (\(_, old) (_, new) -> action old new)
-}

newtype StringOrNumber t = StringOrNumber { unStringOrNumber :: t }

instance (IsString s) => FromJSON (StringOrNumber s) where
  parseJSON v
    =   (withText "string" (pure . StringOrNumber . fromString . Text.unpack) v)
    <|> (withScientific "number" (pure . StringOrNumber . fromString . show) v)

{-
toSomeDynamic :: Dynamic dyn => Layout dyn -> Layout SomeDynamic
toSomeDynamic = \case
  Vertical xs -> Vertical (map toSomeDynamic xs)
  Horizontal xs -> Horizontal (map toSomeDynamic xs)
  Panel lab x -> Panel lab (toSomeDynamic x)
  Tabbed ts -> Tabbed (map (\(lab, x) -> (lab, toSomeDynamic x)) ts)
  TextBox lab x -> TextBox lab (SomeDynamic x)
  CheckBox lab x -> CheckBox lab (SomeDynamic x)
  ColorPicker lab x -> ColorPicker lab (SomeDynamic x)
  PlainText txt -> PlainText txt
  Button txt -> Button txt
  Multiline txt -> Multiline (SomeDynamic txt)
-}

-}
basicSplices :: Splices
basicSplices = Map.fromList
  [ (internalEscapeRadius, ParsedValue NoSourceRange $ \case
        RealType -> pure 10.0
        _ -> throwError $ Advice NoSourceRange "The escape radius should be a real number"
    )
  , (internalVanishingRadius, ParsedValue NoSourceRange $ \case
        RealType -> pure 0.0001
        _ -> throwError $ Advice NoSourceRange "The minimum radius should be a real number"
    )
  , (internalIterationLimit, ParsedValue NoSourceRange $ \case
        IntegerType -> pure 100
        _ -> throwError $ Advice NoSourceRange "The iteration limit should be an integer"
    )
  , (internalIterations, ParsedValue NoSourceRange $ \case
        IntegerType -> pure 100
        _ -> throwError $ Advice NoSourceRange "The iteration count should be an integer"
    )
  , (internalStuck, ParsedValue NoSourceRange $ \case
        BooleanType -> pure $ Const (Scalar BooleanType True)
        _ -> throwError $ Advice NoSourceRange "`stuck` should be a truth value"
    )
  ]
