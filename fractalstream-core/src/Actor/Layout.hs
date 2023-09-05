{-# language UndecidableInstances, OverloadedStrings #-}
module Actor.Layout
  ( Layout(..)
  , Label(..)
  , Dimensions(..)
  , allBindings
  , allBindingVars
  , extractAllBindings
  , Dummy(..)
  , YamlVar(..)
  , StringOrNumber(..)
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
  , toSomeDynamic
  ) where

import Language.Type
import Language.Environment
import Data.Color
import Data.Map.Strict (Map)
import Data.DynamicValue
import Language.Value.Evaluator (HaskellTypeOfBinding, evaluate)
import qualified Language.Untyped.Value as U
import Language.Value
import Language.Value.Parser

import Control.Applicative hiding (Const)
import Data.String
import Data.Aeson hiding (Value)
import qualified Data.Aeson.Types as JSON
import GHC.TypeLits
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Read (readMaybe)

newtype Label = Label String
  deriving Show

instance FromJSON Label where
  parseJSON = withText "label" (pure . Label . Text.unpack)

data Layout f
  = Vertical [Layout f]
  | Horizontal [Layout f]
  | Panel String (Layout f)
  | Tabbed [(String, Layout f)]
  | TextBox Label (f String)
  | CheckBox Label (f Bool)
  | ColorPicker Label (f Color)

deriving instance (Show (f String), Show (f Bool), Show (f Color)) => Show (Layout f)

data Dimensions = Dimensions (Int, Int)

instance FromJSON Dimensions where
  parseJSON = withText "dimensions" $ \txt -> do
    case Text.splitOn "x" txt of
      [xStr, yStr] -> do
        case (,) <$> readMaybe (Text.unpack xStr) <*> readMaybe (Text.unpack yStr) of
          Just dim -> pure (Dimensions dim)
          Nothing  -> fail "could not parse dimension descriptor"
      _ -> fail "expected a dimension descriptor, e.g. 400x200"

data Dummy t = Dummy YamlVar
  deriving Show

data YamlVar = YamlVar
  { varValue :: String
  , varType :: SomeType
  , varEnv :: Map String SomeType
  , varVariable :: String
  }
  deriving Show

instance FromJSON (Layout Dummy) where
  parseJSON = withObject "layout" parseLayout

parseLayout :: Object -> JSON.Parser (Layout Dummy)
parseLayout o
  =   (Vertical <$> (o .: "vertical-contents"))
  <|> (Horizontal <$> (o .: "horizontal-contents"))
  <|> (uncurry Panel <$> (titled =<< (o .: "panel")))
  <|> (Tabbed <$> ((o .: "tabbed") >>= mapM titled))
  <|> (textBoxLayout =<< (o .: "text-entry"))
  <|> (checkBoxLayout =<< (o .: "checkbox"))
  <|> (colorPickerLayout =<< (o .: "color-picker"))
  <|> fail "bad layout description"
 where
   titled p = (,) <$> (p .: "title") <*> parseLayout p

   textBoxLayout p = do
     lab <- p .: "label"
     StringOrNumber varValue <- p .: "value"
     varType <- p .: "type"
     varVariable <- p .: "variable"
     varEnv <- p .:? "environment" .!= Map.empty
     pure (TextBox lab (Dummy YamlVar{..}))

   checkBoxLayout p = do
     lab <- p .: "label"
     val <- p .: "value"
     let varValue = if val then "true" else "false"
         varType = SomeType BooleanType
         varEnv = Map.empty
     varVariable <- p .: "variable"
     pure (CheckBox lab (Dummy YamlVar{..}))

   colorPickerLayout p = do
     lab <- p .: "label"
     varValue <- p .: "value"
     varVariable <- p .: "variable"
     let varType = SomeType ColorType
         varEnv = Map.empty
     pure (ColorPicker lab (Dummy YamlVar{..}))

allBindings :: Layout Dummy -> [(String, SomeType)]
allBindings = map (\YamlVar{..} -> (varVariable, varType)) . allBindingVars

allBindingVars :: Layout Dummy -> [YamlVar]
allBindingVars = go
  where
    go = \case
      Vertical xs -> concatMap go xs
      Horizontal xs -> concatMap go xs
      Panel _ x -> go x
      Tabbed xs -> concatMap (go . snd) xs
      TextBox _ (Dummy x) -> [x]
      CheckBox _ (Dummy x) -> [x]
      ColorPicker _ (Dummy x) -> [x]

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

parseToHaskellValue :: Context HaskellTypeOfBinding env
                    -> TypeProxy ty
                    -> String
                    -> Either String (HaskellType ty)
parseToHaskellValue ctx ty input = do
  case parseValue (contextToEnv ctx) EmptyContext ty input of
    Left err -> Left (show err)
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
             . (TypeProxy ty -> String -> Either String (HaskellType ty))
            -> TypeProxy ty
            -> String
            -> UIValue (String, HaskellType ty)
            -> IO (Maybe String)
setToParsed parse ty input ui = do
  case parse ty input of
    Left err -> pure (Just err)
    Right v  -> setUIValue ui (input, v) >> pure Nothing


data Expression t where
  Expression :: forall env ty
              . String
             -> EnvironmentProxy env
             -> TypeProxy ty
             -> UIValue (String, Value '(env, ty))
             -> Expression String
  BoolExpression :: String -> UIValue Bool -> Expression Bool
  ColorExpression :: String -> UIValue Color -> Expression Color

instance Dynamic Expression where
  getDynamic = \case
    BoolExpression _ b -> getDynamic b
    ColorExpression _ c -> getDynamic c
    Expression _ _ _ v -> fst <$> getDynamic v

  setDynamic d new = case d of
    BoolExpression _ b -> setDynamic b new
    ColorExpression _ c -> setDynamic c new
    Expression _ env ty v -> do
      case parseValue env EmptyContext ty new of
        Left (_, err) -> pure (Just (show err))
        Right newV    -> do
          setDynamic v (new, newV)
          pure Nothing

  listenWith d action = case d of
    BoolExpression _ b -> listenWith b action
    ColorExpression _ c -> listenWith c action
    Expression _ _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))

data ConstantExpression t where
  ConstantExpression :: forall ty
                      . String
                     -> TypeProxy ty
                     -> UIValue (String, HaskellType ty)
                     -> ConstantExpression String
  ConstantBoolExpression :: String -> UIValue Bool -> ConstantExpression Bool
  ConstantColorExpression :: String -> UIValue Color -> ConstantExpression Color

instance Dynamic ConstantExpression where
  getDynamic = \case
    ConstantBoolExpression _ b -> getDynamic b
    ConstantColorExpression _ c -> getDynamic c
    ConstantExpression _ _ v -> fst <$> getDynamic v

  setDynamic d new = case d of
    ConstantBoolExpression _ b -> setDynamic b new
    ConstantColorExpression _ c -> setDynamic c new
    ConstantExpression _ ty v -> do
      case parseValue EmptyEnvProxy EmptyContext ty new of
        Left (_, err) -> pure (Just (show err))
        Right newV    -> do
          setDynamic v (new, evaluate newV EmptyContext)
          pure Nothing

  listenWith d action = case d of
    ConstantBoolExpression _ b -> listenWith b action
    ConstantColorExpression _ c -> listenWith c action
    ConstantExpression _ _ v ->
      listenWith v (\old new -> action (fst old) (fst new))

allocateUIExpressions :: Layout Dummy
                      -> ExceptT String IO (Layout Expression)
allocateUIExpressions = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy YamlVar{..}) -> case varType of
        SomeType ty ->
          withEnvFromMap varEnv $ \env ->
            case parseValue env EmptyContext ty varValue of
              Left (_, err) -> fail (show err)
              Right v  ->
                TextBox lab . Expression varVariable env ty
                <$> newUIValue (varValue, v)

      CheckBox lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext BooleanType varValue of
          Left (_, err) -> fail (show err)
          Right v  -> CheckBox lab . BoolExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext ColorType varValue of
          Left (_, err) -> fail (show err)
          Right v  -> ColorPicker lab . ColorExpression varVariable
                      <$> newUIValue (evaluate v EmptyContext)

allocateUIConstants :: Layout Dummy
                    -> ExceptT String IO (Layout ConstantExpression)
allocateUIConstants = go
  where
    go = \case

      Vertical xs -> Vertical <$> mapM go xs

      Horizontal xs -> Horizontal <$> mapM go xs

      Panel lab x -> Panel lab <$> go x

      Tabbed ps -> Tabbed <$> mapM (\(lab, x) -> (lab,) <$> go x) ps

      TextBox lab (Dummy YamlVar{..}) -> case varType of
        SomeType ty ->
          case parseValue EmptyEnvProxy EmptyContext ty varValue of
            Left (_, err) -> fail (show err)
            Right v       -> TextBox lab . ConstantExpression varVariable ty
                             <$> newUIValue (varValue, evaluate v EmptyContext)

      CheckBox lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext BooleanType varValue of
          Left (_, err) -> fail (show err)
          Right v       -> CheckBox lab . ConstantBoolExpression varVariable
                           <$> newUIValue (evaluate v EmptyContext)

      ColorPicker lab (Dummy YamlVar{..}) ->
        case parseValue EmptyEnvProxy EmptyContext ColorType varValue of
          Left (_, err) -> fail (show err)
          Right v       -> ColorPicker lab . ConstantColorExpression varVariable
                           <$> newUIValue (evaluate v EmptyContext)


withSplices :: forall t
             . Layout Expression
            -> (forall splices. Context Splice splices -> IO t)
            -> IO t
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

data ConstantExpression' t where
  ConstantExpression' :: forall ty t
                      . (t ~ HaskellType ty)
                     => TypeProxy ty
                     -> UIValue (String, t)
                     -> ConstantExpression' t

instance Dynamic ConstantExpression' where
  getDynamic (ConstantExpression' _ d) =
    snd <$> getDynamic d

  setDynamic (ConstantExpression' ty d) v =
    setDynamic d (showValue ty v, v)

  listenWith (ConstantExpression' _ d) action =
    listenWith d (\(_, old) (_, new) -> action old new)

newtype StringOrNumber t = StringOrNumber { unStringOrNumber :: t }

instance (IsString s) => FromJSON (StringOrNumber s) where
  parseJSON v
    =   (withText "string" (pure . StringOrNumber . fromString . Text.unpack) v)
    <|> (withScientific "number" (pure . StringOrNumber . fromString . show) v)

toSomeDynamic :: Dynamic dyn => Layout dyn -> Layout SomeDynamic
toSomeDynamic = \case
  Vertical xs -> Vertical (map toSomeDynamic xs)
  Horizontal xs -> Horizontal (map toSomeDynamic xs)
  Panel lab x -> Panel lab (toSomeDynamic x)
  Tabbed ts -> Tabbed (map (\(lab, x) -> (lab, toSomeDynamic x)) ts)
  TextBox lab x -> TextBox lab (SomeDynamic x)
  CheckBox lab x -> CheckBox lab (SomeDynamic x)
  ColorPicker lab x -> ColorPicker lab (SomeDynamic x)
