module Data.DynamicValue
  ( Dynamic(..)
  , Variable(..)
  , Mapped(..)
  , AsDynamic(..)
  , newVariable
  , clone
  , setValue
  , setValue'
  , modifyValue
  , watchDynamic
  , getDynamic
  , type DynamicValue
  , Dynamic_
  , Variable_
  ) where

import FractalStream.Prelude
import Language.Type

import qualified Data.Map as Map
import Control.Concurrent

data Variable a = Variable String (MVar (a, Int, Map Int (a -> IO ())))

instance Eq (Variable a) where
  Variable _ m == Variable _ m' = m == m'

data Dynamic a where
  Dynamic :: forall a. Variable a -> Dynamic a
  Ap :: forall a b. Dynamic (a -> b) -> Dynamic a -> Dynamic b
  Pure :: forall a. a -> Dynamic a
  Join :: forall a. Dynamic (Dynamic a) -> Dynamic a

instance Functor Dynamic where
  fmap f = \case
    Pure x -> Pure (f x)
    x -> Ap (Pure f) x

instance Applicative Dynamic where
  Pure f <*> Pure x = Pure (f x)
  f <*> x = Ap f x
  pure = Pure

instance Monad Dynamic where
  Pure x >>= f = f x
  dx >>= f = Join (f <$> dx)

data Mapped src a = Mapped
  { mapper :: Dynamic (src -> a)
  , source :: Variable src
  }

newVariable :: MonadIO io => String -> a -> io (Variable a)
newVariable n x = Variable n <$> liftIO (newMVar (x, 0, Map.empty))

clone :: MonadIO io => Variable a -> io (Variable a)
clone v = newVariable "" =<< getDynamic v

class AsDynamic f where
  dyn :: forall a. f a -> Dynamic a

instance AsDynamic Dynamic where dyn = id
instance AsDynamic Variable where dyn = Dynamic
instance AsDynamic (Mapped s) where dyn (Mapped f x) = f <*> dyn x

-- | @DynamicValue@ is a type family that represents
-- a named, dynamic version of the Haskell type corresponding to
-- the given 'FSType'.
data DynamicValue :: Symbol -> FSType -> Exp Type
type instance Eval (DynamicValue name ty) = Dynamic (HaskellType ty)

getDynamic :: (AsDynamic f, MonadIO io) => f a -> io a
getDynamic d = case dyn d of
  Dynamic (Variable _ mvar) -> (\(x,_,_) -> x) <$> liftIO (readMVar mvar)
  Ap f x   -> getDynamic f <*> getDynamic x
  Pure x   -> pure x
  Join ddx -> getDynamic ddx >>= getDynamic

setValue :: (MonadIO io, Eq a)
         => Variable a
         -> a
         -> io ()
setValue (Variable _ mvar) x' = liftIO $ do
  (x, n, actions) <- readMVar mvar
  when (x /= x') $ do
    modifyMVar_ mvar (\_ -> pure (x', n, actions))
    void $ traverse ($ x') actions

setValue' :: MonadIO io
          => Variable a
          -> a
          -> io ()
setValue' (Variable _ mvar) x' = liftIO $ do
  actions <- modifyMVar mvar (\(_, n, actions) -> pure ((x', n, actions), actions))
  void $ traverse ($ x') actions

modifyValue :: MonadIO io
            => Variable a
            -> (a -> a)
            -> io ()
modifyValue (Variable _ mvar) f = liftIO $ do
  (fx, actions) <- modifyMVar mvar (\(x, n, actions) ->
                                      let fx = f x
                                      in pure ((fx, n, actions), (fx, actions)))
  void $ traverse ($ fx) actions

watchDynamic :: (MonadIO io, AsDynamic f) => f a -> (a -> IO ()) -> io (IO ())
watchDynamic d = liftIO . (`go` dyn d)
  where
    go :: forall t. (t -> IO ()) -> Dynamic t -> IO (IO ())
    go action = \case
      Pure _ -> pure (pure ())

      Ap f x -> do
        update <- newEmptyMVar
        tid <- forkIO $
          let next = takeMVar update >>= \case
                Just (Left  fv) -> do
                  fx <- fv <$> getDynamic x
                  action fx
                  next
                Just (Right xv) -> do
                  fx <- getDynamic f <&> ($ xv)
                  action fx
                  next
                Nothing -> pure ()
          in next
        stopF <- go (void . putMVar update . Just . Left) f
        stopX <- go (void . putMVar update . Just . Right) x
        pure (stopF >> stopX >> putMVar update Nothing >> killThread tid)

      Dynamic (Variable _ mvar) -> do
        n <- modifyMVar mvar $ \(x, n, m) ->
          pure ((x, n + 1, Map.insert n action m), n)
        pure (modifyMVar_ mvar $ \(x', n', m) -> pure (x', n', Map.delete n m))

      Join ddx -> do
        inner <- newMVar (pure ())
        outerStop <- watchDynamic ddx $ \dx -> do
          -- If the inner dynamic value has changed, run the old
          -- stop action (if any), then set the new stop action.
          tryTakeMVar inner >>= \case
            Nothing -> do
              istop <- watchDynamic dx action
              putMVar inner istop
            Just oldStop -> do
              oldStop
              istop <- watchDynamic dx action
              putMVar inner istop
        pure (join (takeMVar inner) >> outerStop)


  {-
  ( Dynamic(..)
  , SomeDynamic(..)
  , UIValue
  , newUIValue
  , modifyUIValue
  , setUIValue
  , getUIValue
  , type DynamicValue
  , SomeUIExpr(..)
  , SomeUIValue(..)
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Value.Parser (ParsedValue(..))

import Control.Concurrent.MVar

-- | A type constructor @f@ is @Dynamic@ when it supports
-- impure reads and writes, and can inform listeners when its
-- value changes.
--
-- NOTE: there is not yet any provision for UN-registering listeners!
class Dynamic (e :: Type -> Type) where
  getDynamic :: e t -> IO t
  setDynamic :: e t -> t -> IO (Maybe String)
  listenWith :: e t -> (t -> t -> IO ()) -> IO ()

instance Dynamic UIValue where
  getDynamic = getUIValue
  setDynamic d v = setUIValue d v >> pure Nothing
  listenWith = onChange

-- | A @SomeDynamic t@ is an instance of @Dynamic@, but we
-- aren't sure /which/ instance. In other words, it's an
-- existential closure over some dynamic value of type @t@.
data SomeDynamic t where
  SomeDynamic :: forall dyn t. Dynamic dyn => dyn t -> SomeDynamic t

instance Dynamic SomeDynamic where
  getDynamic (SomeDynamic d) = getDynamic d
  setDynamic (SomeDynamic d) = setDynamic d
  listenWith (SomeDynamic d) = listenWith d

-- | @UIValue@ is some simple glue that allows us to dynamically
-- read and write configuration values.
newtype UIValue t = UIValue (MVar (t, [t -> t -> IO ()]))

-- | Create a new @UIValue@ with no listeners and the given initial value.
newUIValue :: MonadIO m => t -> m (UIValue t)
newUIValue v = UIValue <$> liftIO (newMVar (v, []))

-- | Register a listener for changes to some @UIValue@.
onChange :: MonadIO m => UIValue t -> (t -> t -> IO ()) -> m ()
onChange (UIValue glue) action =
  liftIO (modifyMVar_ glue (\(v, actions) -> pure (v, action:actions)))

-- | Update a @UIValue@ using the given function, triggering update listeners.
--
-- NOTE: This takes a lock while the listeners are being invoked. If
-- one of the listeners attempts to update this @UIValue@ again, it will
-- deadlock! If you may need to update the @UIValue@ in a handler, do it
-- asynchronously by forking a thread and not waiting for completion.
modifyUIValue :: MonadIO m => UIValue t -> (t -> t) -> m ()
modifyUIValue (UIValue glue) f = liftIO $ modifyMVar_ glue $ \(old, actions) -> do
  let !new = f old
  forM_ actions (\action -> action old new)
  pure (new, actions)

-- | Write a @UIValue@, triggering update listeners.
setUIValue :: MonadIO m => UIValue t -> t -> m ()
setUIValue v = liftIO . modifyUIValue v . const

-- | Read a @UIValue@.
getUIValue :: MonadIO m => UIValue t -> m t
getUIValue (UIValue glue) = fst <$> liftIO (readMVar glue)

-- | @DynamicValue@ is a type family that represents
-- a named, dynamic version of the Haskell type corresponding to
-- the given 'FSType'.
data DynamicValue :: Symbol -> FSType -> Exp Type
type instance Eval (DynamicValue name ty) = SomeDynamic (HaskellType ty)

data SomeUIValue where
  SomeUIValue :: forall name ty
               . (KnownSymbol name)
              => Proxy name
              -> TypeProxy ty
              -> SomeDynamic (HaskellType ty)
              -> SomeUIValue

data SomeUIExpr where
  SomeUIExpr :: forall name ty
               . (KnownSymbol name)
              => Proxy name
              -> TypeProxy ty
              -> IO ParsedValue
              -> SomeUIExpr
-}


-- | @Dynamic_@ is a type family that represents
-- a named, dynamic version of the Haskell type corresponding to
-- the given 'FSType'.
data Dynamic_ :: Symbol -> FSType -> Exp Type
type instance Eval (Dynamic_ name ty) = Dynamic (HaskellType ty)

-- | @Variable_@ is a type family that represents
-- a named, variable version of the Haskell type corresponding to
-- the given 'FSType'.
data Variable_ :: Symbol -> FSType -> Exp Type
type instance Eval (Variable_ name ty) = Variable (HaskellType ty)
