{-# language ImpredicativeTypes, OverloadedStrings, NumericUnderscores #-}
{-# options_ghc -Wno-unused-top-binds #-}
module UI.ProjectEditor
  ( editProject
{-
  , Dyn
  , newDyn
  , getDyn
  , setValue
  , setValue'
  , watchDyn

  , DEnsemble(..)

  , DynamicLayout(..)
-}
  ) where

editProject :: FilePath -> IO ()
editProject _ = do
  putStrLn "TODO: editProject"

{-
import FractalStream.Prelude hiding (get)

--import Actor.Ensemble
--import Actor.Configuration
import Actor.Layout
import Actor.Viewer.Complex
--import Actor.Tool
--import Actor.Event
import Language.Type
import Language.Environment
import Language.Value (ValueF(Var))
import Language.Value.Parser
import Language.Value.Typecheck (internalEscapeRadius, internalVanishingRadius, internalIterationLimit, internalIterations, internalStuck)
import Language.Code.Parser (parseCode)
import Language.Parser.SourceRange
import Language.Parser.Tokenizer (commentRanges, Token(Identifier), SRToken(..), tokenize)
import Language.Typecheck
import Graphics.UI.WX hiding (Var, Vertical, Horizontal, Layout, Object, pt, tabs, when, update, center, next)
import qualified Graphics.UI.WX as WX
import Graphics.UI.WXCore.Events (EventTree(..))
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Frame (windowGetScreenPosition)
import Graphics.UI.WXCore.WxcDefs

import UI.CodeEditor

import qualified Data.Yaml as YAML
import qualified Data.Map as Map
import Control.Concurrent

import Lens.Micro hiding (set)
--import Data.Coerce
import Text.Read hiding (get, String, choice)
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as Text
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (isSpace)
--import Data.Void

editProject :: FilePath -> IO ()
editProject yamlFile = do
  dens <- join (YAML.decodeFileThrow yamlFile)
  f <- frame [ text := "Editing project " ++ yamlFile
             , on resize := propagateEvent ]
  makeEditor dens f


data DynVar a = DynVar String (MVar (a, Int, Map Int (a -> IO ())))

instance Eq (DynVar a) where
  DynVar _ m == DynVar _ m' = m == m'


data Dyn a where
  Dyn :: forall a. DynVar a -> Dyn a
  Ap :: forall a b. Dyn (a -> b) -> Dyn a -> Dyn b
  Pure :: forall a. a -> Dyn a
  Join :: forall a. Dyn (Dyn a) -> Dyn a

instance Functor Dyn where
  fmap f = \case
    Pure x -> Pure (f x)
    x -> Ap (Pure f) x

instance Applicative Dyn where
  Pure f <*> Pure x = Pure (f x)
  f <*> x = Ap f x
  pure = Pure

instance Monad Dyn where
  dx >>= f = Join (f <$> dx)


newDyn :: String -> a -> IO (Dyn a)
newDyn n = fmap Dyn . newDynVar n

newDynVar :: String -> a -> IO (DynVar a)
newDynVar n x = DynVar n <$> newMVar (x, 0, Map.empty)

getDyn :: Dyn a -> IO a
getDyn = \case
  Dyn (DynVar _ mvar) -> (^. _1) <$> readMVar mvar
  Ap f x   -> getDyn f <*> getDyn x
  Pure x   -> pure x
  Join ddx -> getDyn ddx >>= getDyn

setValue :: Eq a
         => DynVar a
         -> a
         -> IO ()
setValue (DynVar _ mvar) x' = do
  (x, n, actions) <- readMVar mvar
  when (x /= x') $ do
    modifyMVar_ mvar (\_ -> pure (x', n, actions))
    void $ traverse ($ x') actions

setValue' :: DynVar a
          -> a
          -> IO ()
setValue' (DynVar _ mvar) x' = do
  actions <- modifyMVar mvar (\(_, n, actions) -> pure ((x', n, actions), actions))
  void $ traverse ($ x') actions

modifyValue :: DynVar a
            -> (a -> a)
            -> IO ()
modifyValue (DynVar _ mvar) f = do
  (fx, actions) <- modifyMVar mvar (\(x, n, actions) ->
                                      let fx = f x
                                      in pure ((fx, n, actions), (fx, actions)))
  void $ traverse ($ fx) actions

-- | Perform an action whenever a dynamic value is updated.
-- NOTE 1: This performs the action only when the dynamic value changes.
--         In particular, the action will never run on dynamic
--         values of the form `pure x`.
-- NOTE 2: The action might not run on the same thread that `watchDyn`
--         was invoked from.
watchDyn :: Dyn a -> (a -> IO ()) -> IO (IO ())
watchDyn = flip go
  where
    go :: forall t. (t -> IO ()) -> Dyn t -> IO (IO ())
    go action = \case
      Pure _ -> pure (pure ())

      Ap f x -> do
        update <- newEmptyMVar
        tid <- forkIO $
          let next = takeMVar update >>= \case
                Just (Left  fv) -> do
                  fx <- fv <$> getDyn x
                  action fx
                  next
                Just (Right xv) -> do
                  fx <- getDyn f <&> ($ xv)
                  action fx
                  next
                Nothing -> pure ()
          in next
        stopF <- go (void . putMVar update . Just . Left) f
        stopX <- go (void . putMVar update . Just . Right) x
        pure (stopF >> stopX >> putMVar update Nothing >> killThread tid)

      Dyn (DynVar _ mvar) -> do
        n <- modifyMVar mvar $ \(x, n, m) ->
          pure ((x, n + 1, Map.insert n action m), n)
        pure (modifyMVar_ mvar $ \(x', n', m) -> pure (x', n', Map.delete n m))

      Join ddx -> do
        inner <- newMVar (pure ())
        outerStop <- watchDyn ddx $ \dx -> do
          -- If the inner dynamic value has changed, run the old
          -- stop action (if any), then set the new stop action.
          tryTakeMVar inner >>= \case
            Nothing -> do
              istop <- watchDyn dx action
              putMVar inner istop
            Just oldStop -> do
              oldStop
              istop <- watchDyn dx action
              putMVar inner istop
        pure (join (takeMVar inner) >> outerStop)

data DValue s a = DValue (Dyn (s -> Either String a)) (DynVar s)
  deriving Functor

type DValue' a = DValue a a

asDyn :: DValue s a -> Dyn (Either String a)
asDyn (DValue check input) = check <*> Dyn input

dvSource :: DValue s a -> Dyn s
dvSource (DValue _ input) = Dyn input

setDValue' :: DValue String String -> String -> IO ()
setDValue' (DValue _ dv) input = setValue' dv input

newDValue :: String -> Dyn (s -> Either String a) -> s -> IO (DValue s a)
newDValue n check input = DValue check <$> newDynVar n input

newDValue' :: String -> (s -> Either String a) -> s -> IO (DValue s a)
newDValue' n = newDValue n . pure

data DEnsemble = DEnsemble
  { densembleSetup :: DynVar (Maybe DConfiguration)
  , densembleConfiguration :: DynVar (Maybe DConfiguration)
  , densembleViewers :: DynVar [DComplexViewer]
  }

instance FromJSON (IO DEnsemble) where
  parseJSON = withObject "ensemble" $ \o -> do
    ensembleSetup <- o .:? "setup"
    ensembleConfiguration <- o .:? "configuration"
    singleViewer <- o .:? "viewer"
    ensembleViewers <- case singleViewer of
      Just viewer -> pure [viewer]
      Nothing -> o .:? "viewers" .!= []
    pure $ do
      setup <- sequence ensembleSetup
      densembleSetup <- newDynVar "setup" setup
      config <- sequence ensembleConfiguration
      densembleConfiguration <- newDynVar "config" config
      let splices = maybe (pure Map.empty) (splicesFromSetup . Dyn . dcoContents)
                    =<< Dyn densembleSetup
          env = maybe (pure $ SomeEnvironment endOfDecls) (envFromConfig . Dyn . dcoContents)
                =<< Dyn densembleConfiguration
      densembleViewers <- newDynVar "viewer" =<< traverse (\v -> v env splices) ensembleViewers
      pure DEnsemble{..}

class ToDynamicJSON a where toJson :: a -> IO Value

instance ToDynamicJSON DEnsemble where
  toJson DEnsemble{..} = do
    msetup <- getDyn (Dyn densembleSetup) >>= traverse toJson
    mconfig <- getDyn (Dyn densembleConfiguration) >>= traverse toJson
    viewers <- getDyn (Dyn densembleViewers) >>= traverse toJson
    pure $ Object . KM.fromList . concat $
      [ [ ("setup", setup) | setup <- maybeToList msetup ]
      , [ ("configuration", config) | config <- maybeToList mconfig ]
      , [ ("viewer", v) | v <- viewers, length viewers == 1]
      , [ ("viewers", Array $ V.fromList viewers) | length viewers > 1 ]
      ]

splicesFromSetup :: Dyn DynamicLayout -> Dyn (Map String String)
splicesFromSetup = (>>= go)
  where
    tup :: Applicative f => f a -> f b -> f (a,b)
    tup x y = (,) <$> x <*> y

    singleton :: DValue' String -> DValue' String -> Dyn (Map String String)
    singleton x y = either (const Map.empty) (uncurry Map.singleton)
                  . uncurry tup <$> tup (asDyn x) (asDyn y)

    go :: DynamicLayout -> Dyn (Map String String)
    go = \case
      DVertical _ xs -> do
        xs' <- Dyn xs
        Map.unions <$> mapM go xs'
      DHorizontal _ xs -> do
        xs' <- Dyn xs
        Map.unions <$> mapM go xs'
      DPanel _ _ x -> go x
      DTabbed _ xs -> do
        xs' <- Dyn xs
        Map.unions <$> mapM (go . snd) xs'
      DTextBox _ DConfigVar{..}  -> singleton dvarVariable dvarValue
      DCheckBox _ DConfigVar{..} -> singleton dvarVariable dvarValue
      DColorPicker _ DConfigVar{..} -> singleton dvarVariable dvarValue
      DPlainText{} -> pure Map.empty
      DButton{} -> pure Map.empty
      DCodeBox _ DConfigVar{..} -> singleton dvarVariable dvarValue

envFromConfig :: Dyn DynamicLayout -> Dyn SomeEnvironment
envFromConfig = fmap makeEnv . (>>= go)
  where
    makeEnv :: Map String SomeType -> SomeEnvironment
    makeEnv = flip withEnvFromMap (\env -> withEnvironment env $ SomeEnvironment env)

    singleton :: DValue' String -> Dyn (Either String SomeType) -> Dyn (Map String SomeType)
    singleton x y = either (const Map.empty) (uncurry Map.singleton)
                  . (\(p,q) -> case q of { Right qq -> p <&> (,qq); Left e -> Left e })
                <$> ((,) <$> asDyn x <*> y)

    go :: DynamicLayout -> Dyn (Map String SomeType)
    go = \case
      DVertical _ xs -> do
        xs' <- Dyn xs
        Map.unions <$> mapM go xs'
      DHorizontal _ xs -> do
        xs' <- Dyn xs
        Map.unions <$> mapM go xs'
      DPanel _ _ x -> go x
      DTabbed _ xs -> do
        xs' <- Dyn xs
        Map.unions <$> mapM (go . snd) xs'
      DTextBox _ DConfigVar{..}  -> singleton dvarVariable ((ptype =<<) <$> asDyn dvarType)
      DCheckBox _ DConfigVar{..} -> singleton dvarVariable ((ptype =<<) <$> asDyn dvarType)
      DColorPicker _ DConfigVar{..} -> singleton dvarVariable ((ptype =<<) <$> asDyn dvarType)
      DPlainText{} -> pure Map.empty
      DButton{} -> pure Map.empty
      DCodeBox _ DConfigVar{..} -> singleton dvarVariable ((ptype =<<) <$> asDyn dvarType)



data DConfiguration = DConfiguration
  { dcoTitle :: DValue' String
  , dcoSize :: (DValue' String, DValue' String)
  , dcoContents :: DynVar DynamicLayout
  , dcoFreshId :: IO Int
  }


data Schema a where
  Named :: String -> Schema a -> Schema a
  Field :: forall a. String -> Schema a -> Schema a
  Aeson :: forall a. (FromJSON a, ToJSON a) => Schema a
  Schema :: forall a. DynamicJSON a => Schema (a Schema)
  ListOf :: forall a. Schema a -> Schema [a]
  Opt :: forall a. Schema a -> Schema (Maybe a)
  OmitIf :: forall a. Eq a => a -> Schema a -> Schema (Maybe a)
  Get :: forall a. String -> Schema a
  Alts :: forall a. [Schema a] -> Schema a
  PureS :: forall a. a -> Schema a
  ApS :: forall a b. Schema (a -> b) -> Schema a -> Schema b
  FailS :: forall a. Schema a

instance Functor Schema where
  fmap f = ApS (PureS f)

instance Applicative Schema where
  pure = PureS
  (<*>) = ApS

instance Alternative Schema where
  empty = FailS
  Alts xs <|> Alts ys = Alts (xs ++ ys)
  Alts xs <|> y = Alts (xs ++ [y])
  x <|> Alts ys = Alts (x : ys)
  x <|> y = Alts [x, y]

data SchemaPart s where
  SchemaPart :: forall a s. Lens' s a -> Schema a -> SchemaPart s

data TestEnsemble f = TestEnsemble
  { tensembleSetup :: f (Maybe Int)
  , tensembleConfiguration :: f (Maybe Int)
  , tensembleViewers :: f [Int]
  }

instance DynamicJSON TestEnsemble where
  schema = TestEnsemble
    { tensembleSetup = Opt (Field "setup" (Aeson @Int))
    , tensembleConfiguration = Opt (Field "configuration" (Aeson @Int))
    , tensembleViewers = Field "viewers" (ListOf (Aeson @Int))
                      <|> ((:[]) <$> Field "viewer" (Aeson @Int))
                      <|> pure []
    }

do
  setup <- Opt (Field "setup" (Aeson @Int))
  config <- Opt (Field "configuration" (Aeson @Int))
  viewers <- Field "viewers" (ListOf (Aeson @Int))
             <|> ((:[]) <$> Field "viewer" (Aeson @Int))
             <|> pure []
  pure (TestEnsemble <$> setup <*> config <*> (map () <$> viewers)

class DynamicJSON a where
  schema :: a Schema

instance ToDynamicJSON DConfiguration where
  toJson DConfiguration{..} = do
    title <- getDyn (dvSource dcoTitle)
    sizeX <- getDyn (dvSource $ fst dcoSize)
    sizeY <- getDyn (dvSource $ snd dcoSize)
    lo <- getDyn (Dyn dcoContents)
    let go :: DynamicLayout -> IO (KM.KeyMap Value)
        go = \case
          DVertical _ dxs -> do
            xs <- getDyn (Dyn dxs)
            KM.singleton "vertical-contents" . Array . V.fromList . map Object
              <$> mapM go xs
          DHorizontal _ dxs -> do
            xs <- getDyn (Dyn dxs)
            KM.singleton "horizontal-contents" . Array . V.fromList . map Object
              <$> mapM go xs
          DPanel _ n x -> do
            name <- getDyn (Dyn n)
            (KM.singleton "title" (String . Text.pack $ name) `KM.union`) <$> go x
          DTabbed _ dxs -> do
            xs <- getDyn (Dyn dxs)
            fmap (KM.singleton "tabbed" . Array . V.fromList) $
              forM xs $ \(n, x) -> do
                name <- getDyn (Dyn n)
                Object . (KM.singleton "title" (String . Text.pack $ name) `KM.union`)
                  <$> go x
          DTextBox _ DConfigVar{..} -> do
            lab <- getDyn (Dyn dvarLabel)
            var <- getDyn (dvSource dvarVariable)
            val <- getDyn (dvSource dvarValue)
            typ <- getDyn (dvSource dvarType)
            --Right env <- getDyn (dvSource dvarEnv)
            let menv = Nothing
                body = Object $ KM.fromList
                  ([ ("label", String . Text.pack $ lab)
                   , ("variable", String . Text.pack $ var)
                   , ("type", String . Text.pack $ typ)
                   , ("value", String . Text.pack $ val)] ++
                   [ ("environment", e) | e <- maybeToList menv ])
            pure $ KM.singleton "text-entry" body
          DCheckBox _ DConfigVar{..} -> do
            lab <- getDyn (Dyn dvarLabel)
            var <- getDyn (dvSource dvarVariable)
            val <- getDyn (dvSource dvarValue)
            typ <- getDyn (dvSource dvarType)
            --Right env <- getDyn (dvSource dvarEnv)
            let menv = Nothing
                body = Object $ KM.fromList
                  ([ ("label", String . Text.pack $ lab)
                   , ("variable", String . Text.pack $ var)
                   , ("type", String . Text.pack $ typ)
                   , ("value", String . Text.pack $ val)] ++
                   [ ("environment", e) | e <- maybeToList menv ])
            pure $ KM.singleton "checkbox" body
          DColorPicker _ DConfigVar{..} -> do
            lab <- getDyn (Dyn dvarLabel)
            var <- getDyn (dvSource dvarVariable)
            val <- getDyn (dvSource dvarValue)
            typ <- getDyn (dvSource dvarType)
            --Right env <- getDyn (dvSource dvarEnv)
            let menv = Nothing
                body = Object $ KM.fromList
                  ([ ("label", String . Text.pack $ lab)
                   , ("variable", String . Text.pack $ var)
                   , ("type", String . Text.pack $ typ)
                   , ("value", String . Text.pack $ val)] ++
                   [ ("environment", e) | e <- maybeToList menv ])
            pure $ KM.singleton "color-picker" body
          DPlainText _ txt -> KM.singleton "text" . String . Text.pack <$> getDyn (Dyn txt)
          DButton _ lab -> KM.singleton "button" . String . Text.pack <$> getDyn (Dyn lab)
          DCodeBox _ DConfigVar{..} -> do
            var <- getDyn (dvSource dvarVariable)
            val <- getDyn (dvSource dvarValue)
            --Right env <- getDyn (dvSource dvarEnv)
            let menv = Nothing
                body = Object $ KM.fromList
                  ([ ("variable", String . Text.pack $ var)
                  , ("value", String . Text.pack $ val)] ++
                  [ ("environment", e) | e <- maybeToList menv ])
            pure $ KM.singleton "code" body

    contents <- go lo
    pure $ Object $ KM.fromList
      [ ("title", String . Text.pack $ title)
      , ("size", String . Text.pack $ sizeX ++ "x" ++ sizeY)
      ] `KM.union` contents

newtype DDummy t = DDummy DConfigVar

data DConfigVar = DConfigVar
  { dvarLabel :: DynVar String
  , dvarValue :: DValue' String
  , dvarType :: DValue' String
  , dvarEnv :: DValue' String -- Dyn (Map String SomeType)
  , dvarVariable :: DValue' String
  }

instance FromJSON (IO DConfiguration) where
  parseJSON = withObject "configuration" $ \o -> do
    coTitle0 <- o .: "title"
    (w0, h0) <- either (const ("", "")) (show *** show) . parseDim <$> (o .: "size")
    coContents0 <- parseDLayout o
    pure $ do
      next <- newMVar 0
      let dcoFreshId = modifyMVar next (\n -> pure (n + 1, n))
      dcoTitle <- newDValue' "dcoTitle" nonEmpty coTitle0
      dcoSize <- (,) <$> newDValue' "dcoSize X" readPosInt w0 <*> newDValue' "dcosize Y" readPosInt h0
      dcoContents <- coContents0 dcoFreshId >>= newDynVar "configuration layout"
      pure DConfiguration{..}

readPosInt :: String -> Either String String
readPosInt = \case
  "" -> bad ("an empty string")
  s  -> case readMaybe @Int s of
    Nothing -> bad "Nothing"
    Just n
      | n <= 0 -> bad (show n)
      | otherwise -> pure s
 where
   bad x = Left ("A positive integer is required here, not " ++ x)

parseDim :: String -> Either String (Int, Int)
parseDim txt = case (takeWhile (/= 'x') txt, drop 1 $ dropWhile (/= 'x') txt) of
  (xStr, yStr) -> do
    case (,) <$> readMaybe xStr <*> readMaybe yStr of
      Just (x,y)
        | x <= 0    -> Left "Width must be positive"
        | y <= 0    -> Left "Height must be positive"
        | otherwise -> pure (x,y)
      Nothing  -> Left "Could not parse a dimension descriptor, e.g. 400x200"

parseDLayout :: Object -> YAML.Parser (IO Int -> IO DynamicLayout)
parseDLayout = fmap (flip allocLayout) . parseLayout

--allocLayout :: Layout Dummy -> IO (Dyn (Layout DDummy))
--allocLayout = (>>= newDyn "layout fragment") . mapLayoutA allocDummy

data DynamicLayout
  = DVertical !Int (DynVar [DynamicLayout])
  | DHorizontal !Int (DynVar [DynamicLayout])
  | DPanel !Int (DynVar String) DynamicLayout
  | DTabbed !Int (DynVar [(DynVar String, DynamicLayout)])
  | DTextBox !Int DConfigVar
  | DCheckBox !Int DConfigVar
  | DColorPicker !Int DConfigVar
  | DCodeBox !Int DConfigVar
  | DPlainText !Int (DynVar String)
  | DButton !Int (DynVar String)

allocDummy' :: String -> Dummy t -> IO DConfigVar
allocDummy' l d = do { DDummy v <- allocDummy l d; pure v }

allocLayout :: IO Int -> Layout Dummy -> IO DynamicLayout
allocLayout fresh = go
  where
    go = \case
      Vertical xs -> do
        xs' <- mapM go xs
        DVertical <$> fresh <*> newDynVar "vertical layout" xs'
      Horizontal xs -> do
        xs' <- mapM go xs
        DHorizontal <$> fresh <*> newDynVar "horizontal layout" xs'
      Panel lab x -> do
        dl <- newDynVar "panel label" lab
        DPanel <$> fresh <*> pure dl <*> go x
      Tabbed xs -> do
        ts <- forM xs $ \(name, x) -> (,) <$> newDynVar "tab name" name <*> go x
        DTabbed <$> fresh <*> newDynVar "tabs" ts
      TextBox (Label lab) v -> DTextBox <$> fresh <*> allocDummy' lab v
      CheckBox (Label lab) v -> DCheckBox <$> fresh <*> allocDummy' lab v
      ColorPicker (Label lab) v -> DColorPicker <$> fresh <*> allocDummy' lab v
      PlainText txt -> DPlainText <$> fresh <*> newDynVar "text" txt
      Button lab -> DButton <$> fresh <*> newDynVar "button" lab
      Multiline v -> DCodeBox <$> fresh <*> allocDummy' "" v

toTypeString :: SomeType -> String
toTypeString (SomeType ty) = case ty of
  PairType x y -> concat ["(", toTypeString (SomeType x), ") x (", toTypeString (SomeType y), ")"]
  ListType x  -> "List of " ++ toTypeString (SomeType x)
  TextType -> "Text"
  ImageType -> "Image"
  VoidType -> "Void"
  ColorType -> "Color"
  RationalType -> "Rational"
  ComplexType -> "ℂ"
  RealType -> "ℝ"
  IntegerType -> "ℤ"
  BooleanType -> "Boolean"

toEnvString :: Map String SomeType -> String
toEnvString = intercalate ", "
            . map (\(v, t) -> v ++ " : " ++ toTypeString t)
            . Map.toList


allocDummy :: String -> Dummy t -> IO (DDummy t)
allocDummy l d = do
  let ConfigVar{..} = unDummy d
      isCode = case d of { Dummy{} -> False; DummyCode{} -> True }
  putStrLn ("alloc " ++ varVariable ++ " : " ++ show varType ++ " <- " ++ show varValue)
  dvarLabel <- newDynVar "dvarLabel" l
  dvarType <- newDValue' "dvarType" ptype' (toTypeString varType)
  dvarEnv <- newDValue' "dvarEnv" penv' (toEnvString varEnv)
  dvarVariable <- newDValue' "dvarVariable" isIdentifier varVariable
  let eitherPair :: (Either a b, Either a c) -> Either a (b, c)
      eitherPair (mx, my) = (,) <$> mx <*> my
      parser = case isCode of
        False -> fmap eitherPair ((,) <$> ((ptype =<<) <$> asDyn dvarType) <*> ((penv =<<) <$> asDyn dvarEnv)) <&> \case
          Right (SomeType (ty :: TypeProxy ty), em) -> \s ->
            withKnownType ty $ withEnvFromMap em $ \(env :: EnvironmentProxy env) ->
            withEnvironment env $
               let tweak = case ty of
                     TextType -> show
                     ListType _ -> ("[" ++) . (++ "]")
                     _ -> id
               in bimap (`ppFullError` s) (const s)
                  $ parseValue @env @ty basicSplices (tweak s) -- FIXME: splices?
          Left err -> \_ -> Left err
        True -> ((penv =<<) <$> asDyn dvarEnv) <&> \case
          Right em -> \s -> withEnvFromMap em $ \env ->
            bimap (`ppFullError` s) (const s) $ parseCode env Map.empty s
          Left err -> \_ -> Left err

  dvarValue <- newDValue "dvarValue" parser varValue
  dv <- getDyn (asDyn dvarValue)
  putStrLn ("  => " ++ show dv)
  pure (DDummy DConfigVar{..})

ptype :: String -> Either String SomeType
ptype s = bimap (`ppFullError` s) id (parseType s)

ptype' :: String -> Either String String
ptype' s = bimap (`ppFullError` s) (const s) (parseType s)

penv :: String -> Either String (Map String SomeType)
penv s = bimap (`ppFullError` s) id (parseEnvironment s)

penv' :: String -> Either String String
penv' s = second (const s) (penv s)

data DComplexViewer = DComplexViewer
  { dcvTitle :: DValue' String
  , dcvSize :: (DValue' String, DValue' String)
  , dcvCanResize :: DynVar Bool
  , dcvCenter :: DValue' String
  , dcvPixelSize :: DValue' String
  , dcvCoord :: DValue' String
  , dcvPixel :: DValue' String
  , dcvEscapeRadius :: DValue' String
  , dcvVanishRadius :: DValue' String
  , dcvIterationLimit :: DValue' String
  , dcvCode :: (Dyn (Either (SourceRange, String) String), DynVar String) --DValue' String
--  , dcvOverlay :: Dyn (Maybe String)
  , dcvTools :: DynVar [DComplexTool]
  }

instance ToDynamicJSON DComplexViewer where
  toJson DComplexViewer{..} = do
    title <- getDyn (dvSource dcvTitle)
    sizeX <- getDyn (dvSource $ fst dcvSize)
    sizeY <- getDyn (dvSource $ snd dcvSize)
    coord <- getDyn (dvSource dcvCoord)
    pixel <- getDyn (dvSource dcvPixel)
    code  <- getDyn (Dyn $ snd dcvCode)
    resizes <- getDyn (Dyn $ dcvCanResize)
    icenter <- getDyn (dvSource dcvCenter)
    ipxsize <- getDyn (dvSource dcvPixelSize)
    escape <- getDyn (dvSource dcvEscapeRadius)
    vanish <- getDyn (dvSource dcvVanishRadius)
    ilimit <- getDyn (dvSource dcvIterationLimit)
    tools <- getDyn (Dyn dcvTools) >>= traverse toJson
    pure $ Object $ KM.fromList $ concat
      [ [ ("title", String . Text.pack $ title)
        , ("size", String . Text.pack $ sizeX ++ "x" ++ sizeY)
        , ("z-coord", String . Text.pack $ coord)
        , ("code", String . Text.pack $ code)
        , ("initial-center", String . Text.pack $ icenter)
        , ("initial-pixel-size", String . Text.pack $ ipxsize) ]
      , [ ("pixel-size", String . Text.pack $ pixel) | pixel /= "" ]
      , [ ("escape-radius", String . Text.pack $ escape) | escape /= "" ]
      , [ ("vanishing-radius", String . Text.pack $ vanish) | vanish /= "" ]
      , [ ("iteration-limit", String . Text.pack $ ilimit) | ilimit /= "" ]
      , [ ("resizable", Bool resizes) | not resizes ]
      , [ ("tools", Array . V.fromList $ tools) | not (null tools) ]
      ]

data DComplexTool = DComplexTool
  { dctName :: DValue' String
  , dctShortcut :: DValue' String
  , dctShortHelp :: DValue' String
  , dctHelp :: DValue' String
  , dctDrawLayer :: DValue' String
  , dctRefreshOnActivate :: DynVar Bool
  , dctRefreshCanUpdate :: DynVar Bool
  , dctConfiguration :: DynVar (Maybe DConfiguration)
--   , dctEventHandlers :: _
  }

maybeVar :: String -> Either String String -> Splices
maybeVar k mi = either (const Map.empty) (Map.singleton k)
                       (first show . parseParsedValue Map.empty =<< mi)

data SomeEnvironment where
  SomeEnvironment :: forall env. KnownEnvironment env
                  => EnvironmentProxy env
                  -> SomeEnvironment

instance Show SomeEnvironment where
  show (SomeEnvironment e) = show e

instance FromJSON (Dyn SomeEnvironment -> Dyn (Map String String) -> IO DComplexViewer) where
  parseJSON = withObject "complex viewer" $ \o -> do
    cvTitle <- o .: "title"
    (w0, h0) <- either (const ("", "")) (show *** show) . parseDim <$> (o .: "size")
    cvCanResize <- o .:? "resizable" .!= True
    cvCoord <- o .: "z-coord"
    cvPixel <- o .:? "pixel-size"
    cvEscapeRadius <- o .:? "escape-radius"
    cvVanishRadius <- o .:? "vanishing-radius"
    cvIterationLimit <- o .:? "iteration-limit"
    StringOrNumber cvCenter <- o .: "initial-center"
    StringOrNumber cvPixelSize <- o .: "initial-pixel-size"
    cvCode <- Text.unpack <$> o .: "code"
    --cvOverlay <- o .:? "overlay"
    cvTools <- o .:? "tools" .!= []
    pure $ \env macros -> do
      dcvTitle <- newDValue' "dcvTitle" nonEmpty cvTitle
      dcvSize <- (,) <$> newDValue' "dcvSize X" readPosInt w0 <*> newDValue' "dcvSize Y" readPosInt h0
      dcvCanResize <- newDynVar "dcvCanResize" cvCanResize
      dcvCenter <- newDValue' "dcvCenter" pvalueC cvCenter
      dcvPixelSize <- newDValue' "dcvPixelSize" pvalueR cvPixelSize
      dcvCoord <- newDValue' "dcvCoord" isIdentifier cvCoord
      dcvPixel <- newDValue' "dcvPixel" pure (fromMaybe "" cvPixel)
      dcvEscapeRadius <- newDValue' "dcvEscapeRadius" pure (fromMaybe "" cvEscapeRadius)
      dcvVanishRadius <- newDValue' "dcvVanishRadius" pure (fromMaybe "" cvVanishRadius)
      dcvIterationLimit <- newDValue' "dcvIterationLimit" pure (fromMaybe "" cvIterationLimit)

      let --cvPixelName = fromMaybe "#pixel" cvPixel
          argX = Proxy @InternalX
          argY = Proxy @InternalY
          argPx = Proxy @InternalPx
          argOutput = Proxy @"color"

      SomeSymbol it <- pure (someSymbolVal internalIterations)
      let iterations = ParsedValue NoSourceRange $ \case
            IntegerType -> do
              pf <- findVarAtType NoSourceRange it IntegerType (envProxy Proxy)
              pure (Var it IntegerType pf)
            ty -> throwError (Surprise NoSourceRange "the hidden iteration counter"
                          "an integer" (Expected $ an ty))

      SomeSymbol stuckVar <- pure (someSymbolVal internalStuck)
      let stuck = ParsedValue NoSourceRange $ \case
            BooleanType -> do
              pf <- findVarAtType NoSourceRange stuckVar BooleanType (envProxy Proxy)
              pure (Var stuckVar BooleanType pf)
            ty -> throwError (Surprise NoSourceRange "the `stuck` loop status"
                              "a truth value" (Expected $ an ty))

      let escapeRadius    = maybeVar internalEscapeRadius <$> asDyn dcvEscapeRadius
          vanishingRadius = maybeVar internalVanishingRadius <$> asDyn dcvVanishRadius
          iterationLimit  = maybeVar internalIterationLimit <$> asDyn dcvIterationLimit
          itersAndStuck   = pure $ Map.fromList [ (internalIterations, iterations)
                                                , (internalStuck, stuck) ]
      let splices = Map.unions <$> sequenceA [ escapeRadius
                                             , vanishingRadius
                                             , iterationLimit
                                             , itersAndStuck ]

      let env' = ( declare' stuckVar BooleanType
                 . declare' it IntegerType) <$>
                 ( (declareOrGet' <$> asDyn dcvCoord <*> pure ComplexType) <*>
                 ( (declareOrGet' <$> (\case { Right "" -> Right "#pixel"; n -> n }
                                       <$> asDyn dcvPixel)
                                  <*> pure RealType) <*>
                 ( declare' argX RealType
                 . declare' argY RealType
                 . declare' argPx RealType
                 . declare' argOutput ColorType
                 . Right <$> env )))

      code <- newDynVar "dcvCode" cvCode
      let dcvCode = (pcode' env' macros splices <*> Dyn code, code)
      dcvTools <- newDynVar "dcvTools" =<< sequence cvTools
      pure DComplexViewer{..}

instance FromJSON (IO DComplexTool) where
  parseJSON = withObject "complex tool" $ \o -> do
    name <- o .: "name"
    shortcut <- o .:? "shortcut"
    shortHelp <- o .:? "short-help" .!= ""
    toolHelp <- o .:? "help" .!= ""
    refreshOnActivate <- o .:? "refresh-on-activation" .!= True
    refreshCanUpdate <- o .:? "refresh-can-update" .!= False
    config <- o .:? "configuration"
    drawLayer <- o .:? "draw-to-layer" .!= (100 :: Int)
    pure $ do
      dctName <- newDValue' "dctName" nonEmpty name
      dctShortcut <- newDValue' "dctShortcut" atMostOne (fromMaybe "" shortcut)
      dctShortHelp <- newDValue' "dctShortHelp" pure shortHelp
      dctHelp <- newDValue' "dctHelp" pure toolHelp
      dctDrawLayer <- newDValue' "dctDrawLayer" isInt (show drawLayer)
      dctRefreshOnActivate <- newDynVar "dctRefreshOnActivate" refreshOnActivate
      dctRefreshCanUpdate <- newDynVar "dctRefreshCanUpdate" refreshCanUpdate
      dctConfiguration <- newDynVar "dctConfig" =<< sequence config
      pure DComplexTool{..}

instance ToDynamicJSON DComplexTool where
  toJson DComplexTool{..} = do
    name <- getDyn (dvSource dctName)
    shortcut <- getDyn (dvSource dctShortcut)
    shortHelp <- getDyn (dvSource dctShortHelp)
    fullHelp <- getDyn (dvSource dctHelp)
    drawLayer <- getDyn (dvSource dctDrawLayer)
    refreshOnActivate <- getDyn (Dyn dctRefreshOnActivate)
    refreshCanUpdate <- getDyn (Dyn dctRefreshCanUpdate)
    mconfig <- getDyn (Dyn dctConfiguration) >>= traverse toJson
    pure $ Object . KM.fromList . concat $
      [ [ ("name", String . Text.pack $  name) ]
      , [ ("refresh-on-activation", Bool refreshOnActivate) | refreshOnActivate /= True ]
      , [ ("refresh-can-update", Bool refreshCanUpdate) | refreshCanUpdate /= False ]
      , [ ("draw-to-layer", String . Text.pack $ drawLayer) | drawLayer /= "100" ]
      , [ ("shortcut", String . Text.pack $ shortcut) | shortcut /= "" ]
      , [ ("short-help", String . Text.pack $ shortHelp) | shortHelp /= "" ]
      , [ ("help", String . Text.pack $ fullHelp) | fullHelp /= "" ]
      , [ ("configuration", config) | config <- maybeToList mconfig ]
      ]

declare' :: KnownSymbol name
         => Proxy name
         -> TypeProxy ty
         -> Either String SomeEnvironment
         -> Either String SomeEnvironment
declare' name ty env0 = do
  SomeEnvironment env <- env0
  case lookupEnv name ty env of
    Absent pf -> withKnownType ty $
      recallIsAbsent pf (pure . SomeEnvironment $ BindingProxy name ty env)
    _ -> Left (symbolVal name ++ " is defined twice.")

declareOrGet' :: Either String String
              -> TypeProxy ty
              -> Either String SomeEnvironment
              -> Either String SomeEnvironment
declareOrGet' v0 ty env0 = do
  v <- v0
  SomeEnvironment env <- env0
  SomeSymbol name <- pure (someSymbolVal v)
  case lookupEnv name ty env of
    Absent pf -> withKnownType ty $
      recallIsAbsent pf $ (pure . SomeEnvironment $ BindingProxy name ty env)
    Found _  -> env0
    WrongType {} -> Left (symbolVal name ++ " was defined at two different types.")

nonEmpty :: String -> Either String String
nonEmpty = \case
  "" -> Left "This value cannot be empty"
  s  -> pure s

isIdentifier :: String -> Either String String
isIdentifier s = case tokenize s of
  [SRToken (Identifier _) _] -> pure s
  _ -> Left "This value should be a valid variable name"

isInt :: String -> Either String String
isInt s = bimap (const "This value should be an integer") (const s) (readEither @Int s)

atMostOne :: String -> Either String String
atMostOne = \case
  (_:_:_) -> Left "This value should either be empty or a single letter"
  s -> pure s

pcode' :: Dyn (Either String SomeEnvironment)
       -> Dyn (Map String String)
       -> Dyn (Map String ParsedValue)
       -> Dyn (String -> Either (SourceRange, String) String)
pcode' denv dmacros dsplices =
  (\menv macros splices s -> case menv of
      Right (SomeEnvironment env) -> case substitute s macros of
        Left err -> Left err
        Right s' -> bimap (errorLocation &&& (unlines . pp)) (const s) $
          parseCode env splices s'
      Left err -> Left (NoSourceRange, err))
  <$> denv <*> dmacros <*> dsplices

pvalueC :: String -> Either String String
pvalueC s = bimap (`ppFullError` s) (const s)
  $ parseValue @'[] @'ComplexT Map.empty s

pvalueR :: String -> Either String String
pvalueR s = bimap (`ppFullError` s) (const s)
  $ parseValue @'[] @'RealT Map.empty s

-- | Like `watchDyn`, but ensures that the action
-- runs on the main UI thread. Automatically attaches
-- the halt action to the closing of `p`.
wxWatchDyn :: Window b -> Dyn a -> (a -> IO ()) -> IO ()
wxWatchDyn p dv action = do
  todo <- newMVar []
  halt <- watchDyn dv $ \x -> modifyMVar_ todo (\actions -> pure (action x : actions))
  _ <- timer p [ interval := 100
               , enabled := True
               , on command := tryTakeMVar todo >>= \case
                   Nothing -> pure ()
                   Just actions -> do
                     putMVar todo []
                     sequence_ (reverse actions)
               ]
  set p [ on closing :~ \previous -> halt >> previous ]
  -- Run the action once
  getDyn dv >>= action
  pure ()

{-
makeEditor :: DEnsemble -> Frame a -> IO ()
makeEditor DEnsemble{..} frame0 = do
  panel0 <- panel frame0 []

  -- Make a notebook with the setup and config tabs
  nb <- feed2 [ visible := True ] 0 $
        initialWindow $ \iD rect' ps s -> do
          e <- notebookCreate panel0 iD rect' s
          set e ps
          pure e

  let removeChildren p = do
        set p [ layout := glue ]
        get p children >>= mapM_ (`windowClose` True)
        windowDestroyChildren p

  setupPage <- panel nb []
  wxWatchDyn panel0 (Dyn densembleSetup) $ \case
    Nothing -> do
      removeChildren setupPage
      p <- panel setupPage []
      btn <- button p [ text := "Add setup stage..."
                      ,  on command := do
                          dcoTitle <- newDValue' "dcoTitle Setup" nonEmpty "Setup"
                          dcoSize <- (,) <$> newDValue' "dcoSize Setup X0" readPosInt "400"
                                         <*> newDValue' "dcoSize Setup Y0" readPosInt "400"
                          dempty <- newDynVar "empty list" []
                          nextId <- newMVar 1
                          let dcoFreshId = modifyMVar nextId (\n -> pure (n+1, n))
                          dcoContents <- newDynVar "dcoContents0 Setup" (DVertical 0 dempty)
                          setValue' densembleSetup (Just DConfiguration{..})
                      ]
      set setupPage [ layout := fill $ container p $ floatCentre $ widget btn ]
      void $ windowLayout frame0

    Just dconf -> do
      removeChildren setupPage
      p <- panel setupPage []
      ce <- configEditor frame0 True p (setValue' densembleSetup Nothing) dconf
      set setupPage [ layout := container p $ ce ]
      void $ windowLayout frame0

  configPage <- panel nb []
  wxWatchDyn panel0 (Dyn densembleConfiguration) $ \case
    Nothing -> do
      removeChildren configPage
      p <- panel configPage []
      btn <- button p [ text := "Add variables..."
                      ,  on command := do
                          dcoTitle <- newDValue' "dcoTitle Configuration" nonEmpty "Configuration"
                          dcoSize <- (,) <$> newDValue' "dcoSize Configuration X0" readPosInt "400"
                                         <*> newDValue' "dcoSize Configuration Y0" readPosInt "400"
                          dempty <- newDynVar "empty list" []
                          dcoContents <- newDynVar "dcoContents0 Configuration" (DVertical 0 dempty)
                          nextId <- newMVar 1
                          let dcoFreshId = modifyMVar nextId (\n -> pure (n+1, n))
                          setValue' densembleConfiguration (Just DConfiguration{..})
                      ]
      set configPage [ layout := fill $ container p $ floatCentre $ widget btn ]
      void $ windowLayout frame0

    Just dconf -> do
      removeChildren configPage
      p <- panel configPage []
      ce <- configEditor frame0 False p (setValue' densembleConfiguration Nothing) dconf
      set configPage [ layout := container p $ ce ]
      void $ windowLayout frame0


  notebookAddPage nb setupPage  "📋 Setup" True (-1)
  notebookAddPage nb configPage "⚙️ Variables" True (-1)

  -- TODO: manage individual viewers? when do we free the old ones
  -- if the viewer list is modified?
  wxWatchDyn frame0 (zip [2..] <$> Dyn densembleViewers) $ mapM_ $
    \(page, DComplexViewer{..}) -> do
      let toTitle = ("👁️ " ++ ) . fromRight (const "???") -- 🔍
      title <- toTitle <$> getDyn (asDyn dcvTitle)
      cve <- complexViewerEditor nb DComplexViewer{..}
      void $ notebookAddPage nb cve title True (-1)
      void $ wxWatchDyn cve (asDyn dcvTitle) (void . notebookSetPageText nb page . toTitle)

  notebookSetSelection nb 0

  okButton <- button panel0 [ text := "Go!"
                            , on command := do
                                yaml <- UTF8.toString . YAML.encode
                                        <$> toJson DEnsemble{..}
                                putStrLn ("-------------------------\n" ++ yaml)
                            ]

  set frame0 [ layout := fill $ margin 5 $
               container panel0 $ fill $ column 5 [ fill $ widget nb
                                                  , hfloatRight $ widget okButton ] ]
-}

makeEditor :: DEnsemble -> Frame a -> IO ()
makeEditor DEnsemble{..} frame0 = do
  panel0 <- panel frame0 []

  -- Make a notebook with the setup and config tabs
  nb <- feed2 [ visible := True ] 0 $
        initialWindow $ \iD rect' ps s -> do
          e <- notebookCreate panel0 iD rect' s
          set e ps
          pure e

  let removeChildren p = do
        set p [ layout := glue ]
        get p children >>= mapM_ (`windowClose` True)
        windowDestroyChildren p

  setupPage <- panel nb []
  wxWatchDyn panel0 (Dyn densembleSetup) $ \case
    Nothing -> do
      removeChildren setupPage
      p <- panel setupPage []
      btn <- button p [ text := "Add setup stage..."
                      ,  on command := do
                          dcoTitle <- newDValue' "dcoTitle Setup" nonEmpty "Setup"
                          dcoSize <- (,) <$> newDValue' "dcoSize Setup X0" readPosInt "400"
                                         <*> newDValue' "dcoSize Setup Y0" readPosInt "400"
                          dempty <- newDynVar "empty list" []
                          nextId <- newMVar 1
                          let dcoFreshId = modifyMVar nextId (\n -> pure (n+1, n))
                          dcoContents <- newDynVar "dcoContents0 Setup" (DVertical 0 dempty)
                          setValue' densembleSetup (Just DConfiguration{..})
                      ]
      set setupPage [ layout := fill $ container p $ floatCentre $ widget btn ]
      void $ windowLayout frame0

    Just dconf -> do
      removeChildren setupPage
      p <- panel setupPage []
      ce <- configEditor frame0 True p (setValue' densembleSetup Nothing) dconf
      set setupPage [ layout := container p $ ce ]
      void $ windowReLayout frame0

  configPage <- panel nb []
  wxWatchDyn panel0 (Dyn densembleConfiguration) $ \case
    Nothing -> do
      removeChildren configPage
      p <- panel configPage []
      btn <- button p [ text := "Add variables..."
                      ,  on command := do
                          dcoTitle <- newDValue' "dcoTitle Configuration" nonEmpty "Configuration"
                          dcoSize <- (,) <$> newDValue' "dcoSize Configuration X0" readPosInt "400"
                                         <*> newDValue' "dcoSize Configuration Y0" readPosInt "400"
                          dempty <- newDynVar "empty list" []
                          dcoContents <- newDynVar "dcoContents0 Configuration" (DVertical 0 dempty)
                          nextId <- newMVar 1
                          let dcoFreshId = modifyMVar nextId (\n -> pure (n+1, n))
                          setValue' densembleConfiguration (Just DConfiguration{..})
                      ]
      set configPage [ layout := fill $ container p $ floatCentre $ widget btn ]
      void $ windowReLayout frame0

    Just dconf -> do
      removeChildren configPage
      p <- panel configPage []
      ce <- configEditor frame0 False p (setValue' densembleConfiguration Nothing) dconf
      set configPage [ layout := container p $ ce ]
      void $ windowReLayout frame0


  notebookAddPage nb setupPage  "📋 Setup" True (-1)
  notebookAddPage nb configPage "⚙️ Variables" True (-1)

  -- TODO: manage individual viewers? when do we free the old ones
  -- if the viewer list is modified?
  wxWatchDyn frame0 (zip [2..] <$> Dyn densembleViewers) $ mapM_ $
    \(page, DComplexViewer{..}) -> do
      let toTitle = ("👁️ " ++ ) . fromRight (const "???") -- 🔍
      title <- toTitle <$> getDyn (asDyn dcvTitle)
      cve <- complexViewerEditor nb DComplexViewer{..}
      void $ notebookAddPage nb cve title True (-1)
      void $ wxWatchDyn cve (asDyn dcvTitle) (void . notebookSetPageText nb page . toTitle)

  notebookSetSelection nb 0

  okButton <- button panel0 [ text := "Go!"
                            , on command := do
                                yaml <- UTF8.toString . YAML.encode
                                        <$> toJson DEnsemble{..}
                                putStrLn ("-------------------------\n" ++ yaml)
                            ]

  set frame0 [ layout := fill $ margin 5 $
               container panel0 $ fill $ column 5 [ fill $ widget nb
                                                  , hfloatRight $ widget okButton ] ]
  windowReLayout frame0

imageNone :: Int
imageNone = (-1)

{-
parentMap :: DynamicLayout -> IO (Map Int Int)
parentMap = go \case
  DVertical iD dxs -> do
    xs <- getDyn (Dyn dxs)
    Map.unions <$> mapM parentMap xs
-}

data Feature = WithType | ImplicitType | CodeBox

configEditor :: Window a -> Bool -> Window b -> IO () -> DConfiguration -> IO WX.Layout
configEditor frame0 allowEnv p0 delConfig DConfiguration{..} = do

  p <- panel p0 []

  tc <- treeCtrl p []
  info <- panel p []
  top <- treeCtrlAddRoot tc "☰ Window" imageNone imageNone objectNull

  let editConfigVar :: Feature -> (String -> String -> IO ()) -> DConfigVar -> IO ()
      editConfigVar feature listener DConfigVar{..} = do
        ip <- panel info []
        lab <- textBox ip "Label: " (DValue (pure Right) dvarLabel)
        var <- textBox ip "Variable: " dvarVariable
        varRow <- case feature of
          WithType -> do
            ty  <- textBox ip "Type: " dvarType
            pure (row 10 [ var, ty ])
          _ -> pure var
        val <- case feature of
          -- FIXME, parsing code
          CodeBox -> codeBox ip ( Right <$> dvSource dvarValue
                                , case dvarValue of DValue _ v -> v)
          _       -> textBox ip "Value: " dvarValue
        env <- case allowEnv of
                 True  -> textBox ip "Environment: " dvarEnv
                 False -> pure glue
        delBtn <- button ip [ text := "Delete this variable..." ]
        set info [ layout := container ip $ fill $ column 10 $
                   [ lab, varRow, val, env, glue, hfloatLeft $ widget delBtn ]]
        wxWatchDyn ip ((,) <$> Dyn dvarLabel <*> dvSource dvarValue) (uncurry listener)
        void $ windowLayout frame0

  let addItem t s = treeCtrlAppendItem tc t s imageNone imageNone objectNull
      clip x = let x' = take 50 (takeWhile (/= '\n') x)
               in if length x' + 3 < length x then x' ++ "..." else x
      go :: TreeItem -> DynamicLayout -> IO (Map String (Int, IO ()))
      go t = \case
        DVertical _iD dxs -> do
          xs <- getDyn (Dyn dxs)
          Map.unions <$> mapM (go t) xs

        DHorizontal iD dxs -> do
          xs <- getDyn (Dyn dxs)
          h <- addItem t "👥 Side-by-side"
          m <- Map.unions <$> mapM (go h) xs
          treeCtrlExpand tc h
          pure (Map.insert (show h) (iD, pure ()) m)

        DPanel iD n x -> do
          g <- addItem t ""
          m <- go g x
          treeCtrlExpand tc g
          let setName = treeCtrlSetItemText tc g . ("🏷️ " ++)
              edit = do
                ip <- panel info []
                gn <- textBox ip "Group name: " (DValue (pure nonEmpty) n)
                ungroupBtn <- button ip [ text := "Ungroup" ]
                delBtn <- button ip [ text := "Delete group and children..." ]
                set info [ layout := container ip $ fill $
                           column 10 [ gn, glue, row 5
                                       [ hfloatLeft $ widget ungroupBtn
                                       , hfloatRight $ widget delBtn] ] ]
                wxWatchDyn ip (Dyn n) setName
                void $ windowReLayout frame0
          setName =<< getDyn (Dyn n)
          pure (Map.insert (show g) (iD, edit) m)

        DTabbed iD dxs -> do
          xs <- getDyn (Dyn dxs)
          tb <- addItem t "🗂️ Tab group"
          m <- fmap Map.unions . forM xs $ \(n, x) -> do
            name <- getDyn (Dyn n)
            it <- addItem tb name
            go it x
          treeCtrlExpand tc tb
          let edit = do
                ip <- panel info []
                addTabBtn <- button ip [ text := "Add new tab..." ]
                ungroupBtn <- button ip [ text := "Ungroup all tabs" ]
                delBtn <- button ip [ text := "Delete all tabs..." ]
                set info [ layout := container ip $ fill $ column 5
                           [ glue,
                             row 10 [ hfloatLeft $ widget addTabBtn
                                    , hfloatCentre $ widget ungroupBtn
                                    , hfloatRight $ widget delBtn ]]]
          pure (Map.insert (show tb) (iD, edit) m)

        DTextBox iD d -> do
          i <- addItem t ""
          let setName lab val =
                treeCtrlSetItemText tc i ("✏️ " ++ (clip $ lab ++ " ➤ " ++ val))
          lab0 <- getDyn (Dyn $ dvarLabel d)
          val0 <- getDyn (dvSource $ dvarValue d)
          setName lab0 val0
          pure (Map.singleton (show i) (iD, editConfigVar WithType setName d))

        DCheckBox iD d -> do
          i <- addItem t ""
          let setName lab _val = treeCtrlSetItemText tc i ("☑️ " ++ clip lab)
          lab0 <- getDyn (Dyn $ dvarLabel d)
          val0 <- getDyn (dvSource $ dvarValue d)
          setName lab0 val0
          pure (Map.singleton (show i) (iD, editConfigVar ImplicitType setName d))

        DColorPicker iD d -> do
          i <- addItem t ""
          let setName lab _val = treeCtrlSetItemText tc i ("🎨 " ++ clip lab)
          lab0 <- getDyn (Dyn $ dvarLabel d)
          val0 <- getDyn (dvSource $ dvarValue d)
          setName lab0 val0
          pure (Map.singleton (show i) (iD, editConfigVar ImplicitType setName d))

        DPlainText iD dtxt -> do
          i <- addItem t ""
          let setName = treeCtrlSetItemText tc i . ("📖 " ++) . clip
          setName =<< getDyn (Dyn dtxt)
          pure $ Map.singleton (show i) . (iD,) $ do
            ip <- panel info []
            te <- multilineTextBox ip "Text:" (DValue (pure Right) dtxt)
            delBtn <- button ip [ text := "Delete text..." ]
            set info [ layout := container ip $ fill $
                       column 5 [ te, glue, floatLeft (widget delBtn) ]]
            wxWatchDyn ip (Dyn dtxt) setName
            void $ windowReLayout frame0

        DButton iD db -> do
          bi <- addItem t ""
          let setButtonName = treeCtrlSetItemText tc bi . ("⏺️ " ++) . clip
              edit = do
                ip <- panel info []
                bn <- textBox ip "Button label: " (DValue (pure nonEmpty) db)
                delBtn <- button ip [ text := "Delete button..." ]
                set info [ layout := container ip $ fill $
                           column 5 [ bn, glue, floatLeft (widget delBtn) ]]
                wxWatchDyn ip (Dyn db) setButtonName
                void $ windowReLayout frame0
          setButtonName <$> getDyn (Dyn db)
          pure (Map.singleton (show bi) (iD, edit))

        DCodeBox iD d -> do
          i <- addItem t ""
          let setName _lab val = treeCtrlSetItemText tc i ("📝 ➤ " ++ clip val)
          lab0 <- getDyn (Dyn $ dvarLabel d)
          val0 <- getDyn (dvSource $ dvarValue d)
          setName lab0 val0
          pure (Map.singleton (show i) (iD, editConfigVar CodeBox setName d))

  let selectTop = do
        ip <- panel info []
        lo1 <- textBox ip "Window title" dcoTitle
        lo2 <- textBox ip "Initial width:" (fst dcoSize)
        lo3 <- textBox ip "Initial height:" (snd dcoSize)
        delBtn <- button ip [ text := "Delete all configuration variables..."
                            , on command := delConfig ]
        set info [ layout := container ip $ fill $ column 10
                   [ lo1, row 10 [ lo2, lo3], glue
                   , hfloatLeft $ margin 10 $ widget delBtn ]]
        void $ windowReLayout frame0

  let rebuildTree = do
        treeCtrlDeleteChildren tc top
        m0 <- go top =<< getDyn (Dyn dcoContents)
        treeCtrlExpand tc top

        let m = Map.insert (show top) (-1, selectTop) m0

        set tc [
          on treeEvent := \evt -> case evt of
              TreeBeginDrag ti _pt allow -> do
                allow
                putStrLn ("drag " ++ show ti)
              TreeEndDrag ti _pt -> do
                putStrLn ("end drag " ++ show ti)
              TreeSelChanged newTi oldTi -> do
                putStrLn ("change from " ++ show oldTi ++ " to " ++ show newTi)
                set info [ layout := glue ]
                windowDestroyChildren info
                maybe (pure ()) snd $ Map.lookup (show newTi) m
              TreeDeleteItem ti -> do
                putStrLn ("deleting " ++ show ti)
                propagateEvent
              _ -> propagateEvent
          ]

  let newDConfigVar typ varValue = do
        -- Use allocDummy here
        let varType = SomeType VoidType
            varEnv = Map.empty
            varVariable = ""
        DDummy d <- allocDummy "" (Dummy ConfigVar{..})
        setDValue' (dvarType d) typ
        pure d
      newVertical = DVertical <$> dcoFreshId <*> newDynVar "vertical layout" []
  let newThing =
        [ ("Variable", DTextBox <$> dcoFreshId <*> newDConfigVar "C" "")
        , ("Boolean variable", DCheckBox <$> dcoFreshId <*> newDConfigVar "Boolean" "true")
        , ("Color variable", DColorPicker <$> dcoFreshId <*> newDConfigVar "Color" "red")
        , ("Script", DCodeBox <$> dcoFreshId <*> newDConfigVar "Text" "")
        , ("Plain text", DPlainText <$> dcoFreshId <*> newDynVar "dplaintext" "")
        , ("Named group", DPanel <$> dcoFreshId <*> newDynVar "DPanel name" "" <*> newVertical)
        , ("Side-by-side group", DHorizontal <$> dcoFreshId <*> newDynVar "horizontal layout" [])
        , ("Tabbed group", DTabbed <$> dcoFreshId <*> newDynVar "tab group" [])
        ]
      thingMaker = Map.fromList . zip [1..] . map snd $ newThing
  ctrlp <- panel p []
  addThing <- choice ctrlp [ items := "Add..." : map fst newThing ]
  set addThing [
    on select := do
        i <- get addThing selection
        set addThing [ selection := 0 ]
        case Map.lookup i thingMaker of
          Nothing -> pure ()
          Just make -> do
            -- Add new variable as the last child of the top-level item. If the top-level
            -- item is not a vertical layout, make it one.
            topItem <- getDyn (Dyn dcoContents)
            newItem <- make
            case topItem of
              DVertical _ xs -> modifyValue xs (++ [newItem])
              _ -> do
                newTop <- DVertical <$> dcoFreshId
                                    <*> newDynVar "vertical layout" [ topItem, newItem ]
                setValue' dcoContents newTop
            -- Now refresh the view and select the added item
            rebuildTree
            treeCtrlSelectItem tc =<< treeCtrlGetLastChild tc top
    ]

  rebuildTree
  selectTop
  pure (container p $ fill $ column 5 [ fill $ widget tc
                                      , fill $ widget info
                                      , container ctrlp $
                                        hfloatLeft $ margin 5 $ widget addThing ])

complexViewerEditor :: Window a -> DComplexViewer -> IO (Panel ())
complexViewerEditor frame0 DComplexViewer{..} = do
  p0 <- panel frame0 []

  nb <- feed2 [ visible := True ] 0 $
        initialWindow $ \iD rect' ps s -> do
          e <- notebookCreate p0 iD rect' s
          set e ps
          pure e

  p1 <- panel nb []
  title <- textBox p1 "Viewer title" dcvTitle
  coord <- textBox p1 "Coordinate variable" dcvCoord
  pixel <- textBox p1 "Pixel size variable" dcvPixel

  ce <- codeBox p1 dcvCode
  set p1 [ layout := fill $ margin 5 $ column 5
           [ title
           , row 5 [ coord, pixel ]
           , ce
           ]]

  p2 <- panel nb []
  width  <- textBox p2 "Initial width"  (fst dcvSize)
  height <- textBox p2 "Initial height" (snd dcvSize)
  reSize <- boolBox p2 "Allow resizing?" dcvCanResize
  center <- textBox p2 "Initial center" dcvCenter
  pxSize <- textBox p2 "Initial pixel size" dcvPixelSize
  escape <- textBox p2 "Escape radius" dcvEscapeRadius
  vanish <- textBox p2 "Vanishing radius" dcvVanishRadius
  ilimit <- textBox p2 "Iteration limit" dcvIterationLimit

  del <- button p2 [ text := "Delete viewer...", color := red ]

  set p2 [ layout := fill $ margin 5 $ column 5
           [ boxed "Window dimensions"
             $ fill $ column 5 [ row 5 [ width, height ], hstretch $ reSize ]
           , boxed "Initial view"
             $ fill $ column 5 [ center, pxSize ]
           , boxed "Special values"
             $ fill $ column 5 [ ilimit, escape, vanish ]
           , hfloatLeft $ margin 5 $ widget del
           ]]

  notebookAddPage nb p1 "📝 Script" True (-1)
  notebookAddPage nb p2 "📐 Window" True (-1)

  -- Add a notebook page for each tool
  wxWatchDyn nb (zip [2..] <$> Dyn dcvTools) $ mapM_ $
    \(page, DComplexTool{..}) -> do
      let toTitle = ("🛠️ " ++ ) . fromRight (const "???")
      toolTitle <- toTitle <$> getDyn (asDyn dctName)
      cte <- complexToolEditor nb DComplexTool{..}
      void $ notebookAddPage nb cte toolTitle True (-1)
      void $ wxWatchDyn cte (asDyn dctName) (void . notebookSetPageText nb page . toTitle)

  notebookSetSelection nb 0


  set p0 [ layout := fill $ margin 5 $ widget nb ]
  windowReLayout frame0
  pure p0

complexToolEditor :: Window a -> DComplexTool -> IO (Panel ())
complexToolEditor frame0 DComplexTool{..} = do
  p <- panel frame0 []

  name <- textBox p "Tool name" dctName
  shortcut <- textBox p "Shortcut key" dctShortcut
  set p [ layout := fill $ margin 5 $ column 5
          [ name, shortcut ]]
  windowReLayout frame0
  pure p

convertSourceSpan :: String -> SourceSpan -> (Int, Int)
convertSourceSpan input = \case
  InLine linum s e -> let lo = lineOffset input linum + s - 1
                          hi = e - s + lo
                      in (lo, hi)
  InRows s e -> let lo = lineOffset input
                in (lo s, lo (e + 1))

lineOffset :: String -> Int -> Int
lineOffset input = let m = Map.fromList
                         . ((0,0):)
                         . zip [1..]
                         . map fst
                         . filter ((== '\n') . snd)
                         . zip [1..] $ input
                       li = length input
                   in \lo -> 1 + Map.findWithDefault li lo m

codeBox :: Window a
        -> (Dyn (Either (SourceRange, String) String), DynVar String)
        -> IO WX.Layout
codeBox p0 (dv, input) = do
  outer <- panel p0 []
  normalBG <- get outer bgcolor
  p <- panel outer []
  script <- fromRight (unlines . map ("# " ++) . lines . snd) <$> getDyn dv

  ce <- codeEditor p script
  -- Attach a timer checking for updates. Not really ideal.
  _ <- timer ce [ interval := 500, enabled := True
                , on command := styledTextCtrlGetText ce >>= setValue' input ]

  -- Make the error text
  errorMessage <- variable [ value := Nothing ]
  txt <- staticText p [ text := "" ]

  let doSyntaxColoring = do
        code <- get ce text
        let m = editorOffsetMap code
        styledTextCtrlStartStyling ce 0 0
        styledTextCtrlSetStyling ce ((m Map.! (length code - 1)) + 1) 0
        forM_ (commentRanges code) $ \(s, e) -> do
          styledTextCtrlStartStyling ce (m Map.! s) 0
          styledTextCtrlSetStyling ce (m Map.! (e + 1 - s)) 2
        pure m

      updateAlertStatus = do
        get errorMessage value >>= \case
          Nothing -> do
            set txt [ visible := False ]
            set outer [ bgcolor := normalBG ]
            void doSyntaxColoring
          Just (loc, err) -> do
            code <- get ce text
            case convertSourceSpan code <$> spanOfSourceRange loc of
              Nothing -> void doSyntaxColoring
              Just (s, e) -> do
                m <- doSyntaxColoring
                set outer [ bgcolor := rgb 255 0 (0 :: Int) ]
                styledTextCtrlStartStyling ce (m Map.! s) 0
                styledTextCtrlSetStyling ce (m Map.! (e + 1 - s)) 1
            set txt [ visible := True, text := "🛑 " ++ err ]

      setErrorMessage msg = do
        oldMsg <- get errorMessage value
        when (oldMsg /= msg) $ do
          set errorMessage [value := msg]
          updateAlertStatus
          windowRefresh p False

  wxWatchDyn p dv $ \case
    Left err -> setErrorMessage (Just err)
    Right code -> do
      setErrorMessage Nothing
      oldCode <- get ce text
      when (code /= oldCode) $ do
        set ce [ text := code ]
        void doSyntaxColoring
        windowRefresh p False

  void doSyntaxColoring
  pure (container outer $ margin 3 $ container p $
        fill $ column 5 [ fill $ widget ce, hstretch $ expand $ widget txt ])

boolBox :: Window a -> String -> DynVar Bool -> IO WX.Layout
boolBox p lab dv = do
  initial <- getDyn (Dyn dv)
  cb <- checkBox p [ text := lab
                   , checkable := True
                   , checked := initial
                   , visible := True ]
  set cb [ on command := get cb checked >>= setValue dv ]
  wxWatchDyn p (Dyn dv) (\tf -> set cb [ checked := tf ])
  pure (widget cb)

textBox, multilineTextBox :: Window a -> String -> DValue' String -> IO WX.Layout
textBox = textBox_ False
multilineTextBox = textBox_ True

textBox_ :: Bool -> Window a -> String -> DValue' String -> IO WX.Layout
textBox_ multiline p lab dv = do
  initial <- fromRight (const "") <$> getDyn (asDyn dv)
  te <- textEntry p ([ text := initial, processEnter := True ] ++
                    [ style := wxTE_MULTILINE | multiline ] ++
                    [ font := fontFixed | not multiline ]) -- FIXME ignored on macos?
  -- Make the error popup
  normalBG <- get te bgcolor
  errorMessage <- variable [ value := Nothing ]
  errorPopup <- frame
    [ visible := False
    , style := wxFRAME_TOOL_WINDOW .+. wxNO_BORDER
    , position := Point 0 0 ]
  ep <- panel errorPopup [ bgcolor := rgb 255 200 (200 :: Int) ]
  txt <- staticText ep [ text := ""
                       , font := fontFixed
                       , fontSize := 12
                       , color := black ]

  isFocused <- variable [ value := False ]

  let updateAlertStatus = do
        get errorMessage value >>= \case
          Nothing -> set errorPopup [ visible := False ]
          Just err -> do
            Point wx wy <- windowGetScreenPosition te
            set txt [ text := err ]
            set errorPopup [ layout := fill $ container ep $ margin 15 $ widget txt
                           , position := Point (wx + 30) (wy + 30) ]
            -- Don't use `set errorPopup [ visible := True ]` because it also raises the window
            get isFocused value >>= \case
              True  -> void (windowShow errorPopup)
              False -> void (windowHide errorPopup)

      setErrorMessage msg = do
        oldMsg <- get errorMessage value
        when (oldMsg /= msg) $ do
          set errorMessage [value := msg]
          case msg of
            Just {} -> set te [ bgcolor := rgb 180 80 (80 :: Int)]
            Nothing -> set te [ bgcolor := normalBG ]

        updateAlertStatus

  set te [ on WX.update := do
             newText <- get te text
             setDValue' dv newText
         , on focus := \case
             True -> get isFocused value >>= \case
               True -> pure ()
               False -> do
                 set isFocused [ value := True ]
                 updateAlertStatus
             False -> get isFocused value >>= \case
               False -> pure ()
               True -> do
                 set isFocused [ value := False ]
                 newText <- get te text
                 setDValue' dv newText
         ]

  -- Hook up the listener so that the text entry is updated whenver
  -- the dynamic value changes
  wxWatchDyn p (asDyn dv) $ \case
    Left msg -> setErrorMessage (Just msg)
    Right s  -> do
      setErrorMessage Nothing
      s' <- get te text
      when (s /= s') (set te [ text := s ])

  setDValue' dv initial

  pure (row 5 [ margin 3 $ label lab, hstretch $ expand $ widget te ])

substitute :: String -> Map String String -> Either (SourceRange, String) String
substitute input0 splices = fmap (unlines . concat) . mapM substituteLine . zip [0..] . lines $ input0
  where
    substituteLine (linum, il) =
      let leadingSpaces = takeWhile isSpace il
          il' = drop (length leadingSpaces) il
      in case il' of
        ('$' : input) ->
          let name = takeWhile (/= '$') input
              input' = drop (length name + 1) input
          in case Map.lookup name splices of
            Nothing -> Left ( SourceRange (Pos linum (length leadingSpaces + 1))
                                          (Pos linum (length leadingSpaces + length name + 3))
                            , "No expression named " ++ show name ++ " has been defined in the Setup tab.")
            Just s -> (map (leadingSpaces ++) (lines s) ++) . (:[]) . (leadingSpaces ++)
                      <$> go linum (length leadingSpaces + length name + 2) "" input'
        _ -> (:[]) <$> go linum 0 "" il

    go linum colnum acc = \case
      ('$' : input) -> do
        (newcol, input') <- splice linum (colnum + 1) input
        go linum newcol acc input'
      (c : input) -> go linum (colnum + 1) (c : acc) input
      [] -> pure (reverse acc)

    splice linum colnum input =
      let name = takeWhile (/= '$') input
          input' = drop (length name + 1) input
      in case Map.lookup name splices of
           Nothing -> Left ( SourceRange (Pos linum (colnum - 1)) (Pos linum (colnum + length name))
                           , "No expression named " ++ show name ++ " has been defined in the Setup tab.")
           Just s  -> Right (colnum + length name + 1 - length s - 2, "{" ++ s ++ "}" ++ input')
-}
