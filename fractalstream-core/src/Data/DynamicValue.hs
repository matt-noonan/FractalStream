module Data.DynamicValue
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

import Language.Type
import qualified Language.Untyped.Value as U

import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind
import Control.Concurrent.MVar
import Control.Monad
import GHC.TypeLits
import Data.Proxy
import Fcf

class Dynamic (e :: Type -> Type) where
  getDynamic :: e t -> IO t
  setDynamic :: e t -> t -> IO (Maybe String)
  listenWith :: e t -> (t -> t -> IO ()) -> IO ()

instance Dynamic UIValue where
  getDynamic = getUIValue
  setDynamic d v = setUIValue d v >> pure Nothing
  listenWith = onChange

data SomeDynamic t where
  SomeDynamic :: forall dyn t. Dynamic dyn => dyn t -> SomeDynamic t

instance Dynamic SomeDynamic where
  getDynamic (SomeDynamic d) = getDynamic d
  setDynamic (SomeDynamic d) = setDynamic d
  listenWith (SomeDynamic d) = listenWith d

-- | Allocate UI glue to read and write to configuration values
newtype UIValue t = UIValue (MVar (t, [t -> t -> IO ()]))

newUIValue :: MonadIO m => t -> m (UIValue t)
newUIValue v = UIValue <$> liftIO (newMVar (v, []))

onChange :: MonadIO m => UIValue t -> (t -> t -> IO ()) -> m ()
onChange (UIValue glue) action =
  liftIO (modifyMVar_ glue (\(v, actions) -> pure (v, action:actions)))

modifyUIValue :: MonadIO m => UIValue t -> (t -> t) -> m ()
modifyUIValue (UIValue glue) f = liftIO $ modifyMVar_ glue $ \(old, actions) -> do
  -- Run the on-changed handlers while holding the lock, which will
  -- prevent another change to this element from performing its callbacks
  -- until this one is finished.
  -- NOTE: if any of these handlers change this same element, then they
  --       they will deadlock. If they must set this element, they should
  --       fork a thread to do it asynchronously, and not wait for completion.
  let !new = f old
  forM_ actions (\action -> action old new)
  pure (new, actions)

setUIValue :: MonadIO m => UIValue t -> t -> m ()
setUIValue v = liftIO . modifyUIValue v . const

getUIValue :: MonadIO m => UIValue t -> m t
getUIValue (UIValue glue) = fst <$> liftIO (readMVar glue)

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
              -> IO U.Value
              -> SomeUIExpr
