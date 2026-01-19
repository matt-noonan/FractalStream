{-# language AllowAmbiguousTypes, UndecidableInstances #-}
module Data.Codec
  ( deserialize
  , deserializeWith
  , deserializeYAML
  , serialize
  , Codec(..)
  , CodecWith(..)
  , CodecBuilder(..)
  , Fragment(..)
  , codecWith
  , build
  , key
  , optionalKey
  , keyWithDefault
  , keyWithDefaultValue
  , zoom
  , (-<)
  , mapBy
  , mapped
  , Whole
  , Part
  , Optionally(..)
  ) where

import FractalStream.Prelude
import Data.DynamicValue

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Yaml as YAML
import Data.Void

combineObjects :: [Value] -> Value
combineObjects vs0 =
  let vs = filter (\case { Null -> False; _ -> True }) vs0
  in case vs of
    [] -> Null
    [v] -> v
    _ -> Object . foldl' KM.union KM.empty
          $ map (\case { Object km -> km
                       ; _ -> error "expected JSON object" }) vs

type Whole f a = Part f a a
data Part f s a where
  PartX :: forall s a f. a -> Part f s a
  PartD :: forall s a. a -> Part D s a
  PartE :: forall s a. (s -> IO Value) -> Part E s a

instance Functor (Part f s) where
  fmap f = \case
    PartX x -> PartX (f x)
    PartD x -> PartD (f x)
    PartE g -> PartE g

unE :: Part E s a -> s -> IO Value
unE = \case
  PartX _ -> const (pure Null)
  PartE g -> g

unD :: Part D s a -> a
unD = \case
  PartX x -> x
  PartD x -> x

instance Applicative (Part f s) where
  pure = PartX
  PartX f <*> PartX x = PartX (f x)
  PartX f <*> PartD x = PartD (f x)
  PartD f <*> PartX x = PartD (f x)
  PartD f <*> PartD x = PartD (f x)
  PartX _ <*> PartE g = PartE g
  PartE f <*> PartX _ = PartE f
  PartE f <*> PartE x = PartE $ \s -> do
    f' <- f s
    x' <- x s
    pure (combineObjects [f', x'])

-- | If @a@ is a part of @s'@ and we have a projection
-- from @s@ to @s'@, then @zoom@ lets us realize @a@ as
-- a part of @s@ as well.
zoom :: (s -> s') -> Part f s' a -> Part f s a
zoom proj = \case
  PartX x -> PartX x
  PartD x -> PartD x
  PartE f -> PartE (f . proj)

-- | A @Fragment@ of @s@ is a prism into some part of @s@, along with a codec builder
-- for that part. We don't actually use a @Prism@ here because we want to avoid pulling
-- in the whole huge @profunctors@ dependency.
data Fragment f s where
  Fragment :: forall a f s. (a -> s) -> (s -> Maybe a) -> f (Whole f a) -> Fragment f s

-- | @CodecBuilder@ is a monad that can be used to simultaneously construct an encoder
-- and a decoder for a type. Instances of @CodecBuilder@ cannot be created outside of
-- this module, so you can think of @CodecBuilder@ as a defining a specific monad rather than
-- a typeclass.
class (IsPrivateCodecBuilderInstance f ~ 'True, Monad f) => CodecBuilder f where
  aeson  :: forall a. (FromJSON a, ToJSON a) => f (Whole f a)
  mustBe :: forall a s t. (FromJSON a, ToJSON a) => String -> a -> f (Part f s t) -> f (Part f s t)
  manyOf :: forall a. f (Whole f a) -> f (Whole f [a])
  optOf  :: forall a. f (Whole f a) -> f (Whole f (Maybe a))
  newOf  :: forall a. f (Whole f a) -> f (Whole f (Variable a))
  mapOf  :: forall a. f (Whole f a) -> f (Whole f (Map String a))
  field  :: forall a. String -> f (Whole f a) -> f (Whole f a)
  match  :: forall a. [Fragment f a] -> f (Whole f a)
  optionalField :: forall a. String -> IO a -> (a -> IO Bool)
                -> f (Whole f a) -> f (Whole f a)
  build_  :: forall ctor s. ctor -> ArgsOf f ctor s -> f (Whole f s)
  codecWithImpl :: forall a s ctx. CodecWith ctx a => Proxy a -> Part f s ctx -> f (Whole f a)
  purely :: forall t
          . ((forall s a q. AsDynamic q => Part f s (q a) -> Dynamic a) -> Dynamic t)
         -> f (Whole f (Dynamic t))
  cut :: f ()
  debugDump :: String -> f ()

codecWith :: forall a s ctx f. (CodecBuilder f, CodecWith ctx a) => Part f s ctx -> f (Whole f a)
codecWith = codecWithImpl (Proxy @a)

type family IsPrivateCodecBuilderInstance f where
  IsPrivateCodecBuilderInstance D = 'True
  IsPrivateCodecBuilderInstance E = 'True
  IsPrivateCodecBuilderInstance _ = 'False

data ArgsOf f g s where
  Arg :: forall a b s f. Part f s a -> ArgsOf f b s -> ArgsOf f (a -> b) s
  End :: forall s f. ArgsOf f s s

-- | Decoder monad
newtype D a = D (ReaderT Value (ExceptT String IO) a)
  deriving (Functor, Applicative, Monad)

instance CodecBuilder D where
  debugDump what = D . ReaderT $ \v ->
    liftIO $ putStrLn (">>> Decoding " ++ what ++ " from " ++ show v)
  aeson = D . ReaderT $ \v -> ExceptT . pure $ case fromJSON v of
    Error err -> Left err
    Success v' -> Right (PartD v')
  mustBe fld x (D (ReaderT cont)) = D . ReaderT $ \case
    Object o -> case KM.lookup (fromString fld) o of
      Just v' -> if toJSON x == v' then cont (Object o) else throwError "wrong tag value"
      Nothing -> throwError ("no field named `" ++ fld ++ "`")
    _ -> throwError "not an object"
  newOf xsch = do
    x <- unD <$> xsch
    D . ReaderT $ \_ -> ExceptT (Right . PartD <$> newVariable "" x)
  optOf (D (ReaderT xsch)) = D . ReaderT $ \case
    Null -> pure (PartD Nothing)
    v    -> do
      x <- unD <$> xsch v
      pure (PartD $ Just x)
  manyOf (D (ReaderT xsch)) = D . ReaderT $ \case
    Array vs -> PartD <$> mapM (fmap unD . xsch) (V.toList vs)
    _ -> throwError "not an array"
  mapOf (D (ReaderT xsch)) = D . ReaderT $ \case
    Object o -> fmap (PartD . Map.fromList) $ forM (KM.toList o) $ \(k, vv) ->
      (Key.toString k,) . unD <$> xsch vv
    _ -> throwError "not an object"
  field fld (D (ReaderT f)) = D . ReaderT $ \case
    Object o -> case KM.lookup (fromString fld) o of
      Just v  -> PartD . unD <$> f v
      Nothing -> throwError ("no field named `" ++ fld ++ "`")
    _ -> throwError "not an object"
  optionalField fld mkDefault _ (D (ReaderT f)) = D . ReaderT $ \case
    Object o -> case KM.lookup (fromString fld) o of
      Just v  -> PartD . unD <$> f v
      Nothing -> ExceptT (Right . PartD <$> mkDefault)
    _ -> throwError "not an object"
  build_ (ctor :: c) (args :: ArgsOf D c s) = D . ReaderT $ \_ -> ExceptT $ do
    let go :: forall g. g -> ArgsOf D g s -> Either String s
        go f = \case
          Arg x more -> go (f $ unD x) more
          End -> pure f
    pure (PartD <$> go ctor args)
  codecWithImpl (_ :: Proxy a) (ctx :: Part D s ctx) =
    codecWith_ @ctx @a (PartD $ unD ctx)
  purely action = pure (PartD $ action (dyn . unD))
  cut = pure () -- TODO
  match xs = asum (map (\(Fragment f _ x) -> PartD . f . unD <$> x) xs)

instance Alternative D where
  empty = D . ReaderT $ \_ -> ExceptT (pure $ Left "No matching decoder")
  D x <|> D y = D (x <|> y)

-- | Encoder monad
newtype E a = E a deriving Functor

instance Applicative E where
  pure = E
  E f <*> E x = E (f x)

instance Monad E where
  E x >>= f = f x

instance CodecBuilder E where
  debugDump _ = E ()
  aeson = E . PartE $ (pure . toJSON)
  mustBe fld x (E cont) = E . PartE $ \y -> do
    v <- unE cont y
    pure (combineObjects [v, Object (KM.singleton (fromString fld) (toJSON x))])
  newOf (E f) = E . PartE $ \d -> getDynamic (Dynamic d) >>= unE f
  optOf (E f) = E . PartE $ \m -> maybe (pure Null) (unE f) m
  manyOf (E f) = E . PartE $ \xs -> Array . V.fromList <$> mapM (unE f) xs
  mapOf (E f) = E . PartE $ \xs -> fmap (Object . KM.fromList) $
    forM (Map.toList xs) $ \(k, v) -> (Key.fromString k,) <$> unE f v
  field fld (E f) = E . PartE $ fmap (Object . KM.singleton (fromString fld)) . unE f
  optionalField fld _ testDefault (E f) = E . PartE $ \s -> testDefault s >>= \case
    True  -> pure Null
    False -> Object . KM.singleton (fromString fld) <$> unE f s
  build_ (_ :: c) (args :: ArgsOf E c s) = E . PartE $ \o -> do
    let go :: forall g. ArgsOf E g s -> IO [Value]
        go = \case
          Arg fn more -> (:) <$> unE fn o <*> go more
          End -> pure []
    combineObjects <$> go args
  codecWithImpl (_ :: Proxy a) (_ :: Part E s ctx) =
    codecWith_ @ctx @a (PartX $ error "Internal error: CodecWith instance has an illegal use of the context.")
  purely _action = E (PartE $ const (pure Null))
  cut = pure ()
  match (xs :: [Fragment f a]) = pure $ PartE $ \x ->
    case mapMaybe (\(Fragment _ inspect (E action)) -> unE action <$> inspect x) xs of
      (action : _) -> action
      [] -> pure Null

-- | The main typeclass for associating a codec with a type.
class Codec a where
  codec :: forall f. CodecBuilder f => f (Whole f a)

-- | Sometimes we can't deserialize a value without some additional context.
-- @CodecWith@ is like @Codec@ but allows for that extra context to be passed in.
class CodecWith ctx a | a -> ctx where
  codecWith_ :: forall f. CodecBuilder f => Part f a ctx -> f (Whole f a)

instance Codec Void where codec = match []
instance Codec Bool where codec = aeson
instance Codec Int where codec = aeson
instance {-# OVERLAPS #-} Codec String where codec = aeson
instance {-# OVERLAPPABLE #-} Codec a => Codec [a] where codec = manyOf codec
instance Codec a => Codec (Variable a) where codec = newOf codec
instance Codec a => Codec (Maybe a) where codec = optOf codec
instance Codec v => Codec (Map String v) where codec = mapOf codec
instance (Codec a, Codec b) => Codec (Either a b) where
  codec = match [ Fragment Left  (either Just (const Nothing)) codec
                , Fragment Right (either (const Nothing) Just) codec ]

instance (CodecWith ctx b, b ~ a) => CodecWith ctx [a] where codecWith_ ctx = manyOf (codecWith ctx)
instance (CodecWith ctx b, b ~ a) => CodecWith ctx (Variable a) where codecWith_ ctx = newOf (codecWith ctx)
instance (CodecWith ctx b, b ~ a) => CodecWith ctx (Maybe a) where codecWith_ ctx = optOf (codecWith ctx)
instance (CodecWith ctx b, b ~ a) => CodecWith ctx (Map String a) where codecWith_ ctx = mapOf (codecWith ctx)

-- | @key "foo"@ describes a required value that can be found at the object key "foo".
key :: forall a f. (CodecBuilder f, Codec a) => String -> f (Whole f a)
key fld = field fld codec

-- | @optionalKey "foo"@ describes an optional value that can be found at the object key "foo".
optionalKey :: forall a f. (CodecBuilder f, Codec a, Optionally a) => String -> f (Whole f a)
optionalKey fld = optionalField fld makeDefault isDefault codec

-- | @keyWithDefaultValue d "foo"@ describes a value that can be found at the object key "foo".
-- If there is no such key "foo", the default value @d@ is used instead.
keyWithDefaultValue :: forall a f. (CodecBuilder f, Codec a, Eq a)
               => a -> String -> f (Whole f (Variable a))
keyWithDefaultValue d fld = optionalField fld (newVariable "" d) (fmap (== d) . getDynamic) codec

keyWithDefault :: forall a f. (CodecBuilder f, Codec a)
               => String -> IO a -> (a -> IO Bool) -> f (Whole f a)
keyWithDefault fld mk chk = optionalField fld mk chk codec

(-<) :: Functor f => (s -> s') -> f (Part f s' a) -> f (Part f s a)
proj -< action = zoom proj <$> action
infixr 0 -<

class Optionally a where
  makeDefault :: IO a
  isDefault :: a -> IO Bool

instance Optionally (Maybe a) where
  makeDefault = pure Nothing
  isDefault = pure . isNothing

instance Optionally a => Optionally (Variable a) where
  makeDefault = makeDefault >>= newVariable ""
  isDefault v = getDynamic v >>= isDefault

instance Optionally [a] where
  makeDefault = pure []
  isDefault = pure . null

instance Optionally (Map k v) where
  makeDefault = pure Map.empty
  isDefault = pure . Map.null

mapBy :: forall src result f
       . (CodecBuilder f, Codec src)
      => (src -> result) -> f (Whole f (Mapped src result))
mapBy f = mapped codec (\_ -> pure f)

serialize :: forall a. Codec a => a -> IO ByteString
serialize x =
  let E f = codec @a
  in BS.toStrict . encode <$> unE f x

deserialize :: forall a. Codec a => ByteString -> IO (Either String a)
deserialize bs = case eitherDecodeStrict' bs of
  Left err -> pure (Left err)
  Right v  -> let D (ReaderT f) = codec @a
              in runExceptT (f v <&> unD)

deserializeWith :: forall ctx a. CodecWith ctx a => ctx -> ByteString -> IO (Either String a)
deserializeWith ctx bs = case eitherDecodeStrict' bs of
  Left err -> pure (Left err)
  Right v  -> let D (ReaderT f) = codecWith @_ @a (pure ctx)
              in runExceptT (f v <&> unD)

deserializeYAML :: forall a. Codec a => ByteString -> IO (Either String a)
deserializeYAML bs = case YAML.decodeEither bs of
  Left err -> pure (Left $ show err)
  Right v  -> let D (ReaderT f) = codec @a
              in runExceptT (f v <&> unD)

mapped :: forall src result f
        . CodecBuilder f
       => f (Whole f (Variable src))
       -> ((forall s a q. AsDynamic q => Part f s (q a) -> Dynamic a)
           -> Dynamic (src -> result))
       -> f (Whole f (Mapped src result))
mapped makeSrc makeMapper = do
  src  <-source-< makeSrc
  test <-mapper-< purely makeMapper
  build Mapped test src

-- | @build@ applies a function to several @Part@s, producing a @Whole@
-- result. It uses some trickery to act as a variadic function, so it
-- is easier to understand via usage examples instead of reading its
-- type signature.
--
-- > data Foo { foo :: Int, bar :: Int }
-- >
-- > instance Codec Foo where
-- >   codec = do
-- >     f <-foo-< key "foo"
-- >     b <-bar-< key "bar"
-- >     build Foo f b
build :: forall f ctor s
       . (CodecBuilder f, s ~ CollectResult ctor, LiftCtor f ctor s)
      => ctor
      -> Fun f (CollectArgs ctor) (CollectResult ctor)
build ctor = liftCtor @f @ctor @s (build_ ctor)

type family CollectArgs t :: [Type] where
  CollectArgs (a -> b) = a ': CollectArgs b
  CollectArgs r = '[]

type family CollectResult t :: Type where
  CollectResult (a -> b) = CollectResult b
  CollectResult r = r

type family BuilderOf f (args :: [Type]) t where
  BuilderOf f '[] t = t
  BuilderOf f (x ': args) t = f x -> BuilderOf f args t

type Fun f args s = BuilderOf (Part f s) args (f (Whole f s))

class LiftCtor f ctor s where
  liftCtor :: (ArgsOf f ctor s -> f (Whole f s)) -> Fun f (CollectArgs ctor) s

instance LiftCtor f b s => LiftCtor f (a -> b) s where
  liftCtor f x = liftCtor (f . Arg x)

instance (CollectArgs s ~ '[]) => LiftCtor f s s where
  liftCtor f = f End
