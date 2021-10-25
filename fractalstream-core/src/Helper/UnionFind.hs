module Helper.UnionFind
  ( fresh
  , union
  , unionWith
  , find
  , findParent
  , modify
  , UF
  , STRef
  , module Control.Monad.ST
  ) where

import Control.Monad.ST
import Data.STRef
import Control.Monad (when)

data UF s t = UF
  { ufPayload :: STRef s (Either (UF s t) t)
  , ufRank    :: STRef s Int
  }

-- | Pointer equality, like 'STRef'.
instance Eq (UF s t) where
  uf1 == uf2 = ufPayload uf1 == ufPayload uf2

modify :: UF s t -> (t -> t) -> ST s ()
modify uf f = do
  UF{..} <- findParent uf
  modifySTRef' ufPayload (fmap f)

fresh :: t -> ST s (UF s t)
fresh v = do
  ufPayload <- newSTRef (Right v)
  ufRank    <- newSTRef 0
  pure UF{..}

find :: UF s t -> ST s t
find uf = do
  UF{..} <- findParent uf
  readSTRef ufPayload >>= \case
    Right v -> pure v
    Left _  -> error "internal error in `find`"

findParent :: UF s t -> ST s (UF s t)
findParent uf@UF{..} = do
  payload <- readSTRef ufPayload
  case payload of
    Right _   -> pure uf
    Left next -> do
      p <- findParent next
      writeSTRef ufPayload (Left p)
      pure p

union :: Semigroup t => UF s t -> UF s t -> ST s ()
union = unionWith (\x y -> pure (x <> y))

unionWith :: (t -> t -> ST s t) -> UF s t -> UF s t -> ST s ()
unionWith (#) lhs rhs = do
  lhsRoot <- findParent lhs
  lhsRank <- readSTRef (ufRank lhsRoot)
  rhsRoot <- findParent rhs
  rhsRank <- readSTRef (ufRank rhsRoot)
  let (root, oldRoot) = if lhsRank <= rhsRank
                        then (lhsRoot, rhsRoot)
                        else (rhsRoot, lhsRoot)
  when (oldRoot /= root) $ readSTRef (ufPayload oldRoot) >>= \case
    Left  _ -> error "internal error in `union`"
    Right v -> do
      oldRank <- readSTRef (ufRank oldRoot)
      modifySTRef' (ufRank root) (+ oldRank)
      writeSTRef (ufPayload oldRoot) (Left root)
      readSTRef (ufPayload root) >>= \case
        Left  _ -> error "internal error in `union`"
        Right w -> do
          wv <- w # v
          writeSTRef (ufPayload root) (Right wv)
