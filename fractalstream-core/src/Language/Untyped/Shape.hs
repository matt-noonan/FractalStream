module Language.Untyped.Shape
  ( inferShape
  , ValueWithShape
  , Shape(..)
  , TypeVar(..)
  , ShapeError(..)
  , toTypeShape
  , shapeToTS
  , STShape(..)
  ) where

import qualified Language.Untyped.Value as U
import Language.Untyped.Value (ValueWith)
import Helper.UnionFind
import Data.Recursive
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Coerce
import Language.Untyped.Constraints
import Control.Monad.State hiding (foldM)
import Control.Monad.Except hiding (foldM)
import Data.STRef

---------------------------------------------------------------------------------
-- Shapes
---------------------------------------------------------------------------------

-- | The lattice of shapes is formed from the lattice of types by
-- collapsing the < relation to equality, except at top and bottom.
-- Shapes can then be inferred by unification; later, the shapes will
-- be used to introduce type variables and perform a < sensitive type
-- inference pass.
data Shape s
  = Unknown
  | ShapeError ShapeError
  | BoolShape
  | NumShape
  | ColorShape
  | PairShape s s
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ShapeError
  = Indeterminate
  | Inconsistent
  | InconsistentVar String
  | Unbound String
  | Infinite
  deriving (Eq, Ord, Show)

newtype STShape s = STShape (Shape (UF s (STShape s)))

-- | Unify two shapes. Shapes morally form a monoid under this
-- operation, except we have to operate in the ST monad to
-- allow for unification of shape variables.
unifyShapes :: STShape s -> STShape s -> ST s (STShape s)
unifyShapes (STShape x0) (STShape y0) = STShape <$> case (x0, y0) of
  (Unknown, y) -> pure y
  (x, Unknown) -> pure x
  (PairShape x y, PairShape x' y') -> do
    unionWith unifyShapes x x'
    unionWith unifyShapes y y'
    x'' <- findParent x
    y'' <- findParent y
    pure (PairShape x'' y'')
  (BoolShape, BoolShape) -> pure BoolShape
  (NumShape, NumShape) -> pure NumShape
  (ColorShape, ColorShape) -> pure ColorShape
  (ShapeError e, _) -> pure (ShapeError e)
  (_, ShapeError e) -> pure (ShapeError e)
  _ -> pure (ShapeError Inconsistent)

---------------------------------------------------------------------------------
-- Shape-annotated values
---------------------------------------------------------------------------------

type ValueWithShape s = ValueWith (UF s (STShape s))

-- | Shape inference is the first phase of type inference. Here,
-- we associate a shape variable to each AST node, and unify
-- shape variables as needed. Shapes collapse the subtype relation
-- between non-top and non-bottom types into an equivalence, smooshing
-- together Z, R, and C but leaving other types distinct.
inferShape :: Map String (UF s (STShape s))
           -> U.Value
           -> ST s (ValueWithShape s)
inferShape env = foldM $ \v -> do
  let freshShape = fresh (STShape Unknown)
  sv <- freshShape

  let shapeIs s = do
        sv' <- fresh (STShape s)
        unify sv sv'
      unify = unionWith unifyShapes
      unaryop x = do
        shapeIs NumShape
        unify x sv
      binop lhs rhs = do
        shapeIs NumShape
        unify lhs rhs
        unify lhs sv

  case annotation <$> v of
    U.ConstB _ -> shapeIs BoolShape
    U.ConstI _ -> shapeIs NumShape
    U.ConstF _ -> shapeIs NumShape
    U.ConstC{} -> shapeIs NumShape
    U.ConstColor _ -> shapeIs ColorShape
    U.Var n -> case Map.lookup n env of
      Just vn -> unify vn sv
      Nothing -> shapeIs (ShapeError (Unbound n))
    U.PairV lhs rhs -> shapeIs (PairShape lhs rhs)
    U.ProjV1 p -> do
      p1 <- freshShape
      p2 <- freshShape
      p' <- fresh (STShape $ PairShape p1 p2)
      unify p p'
      unify sv p1
    U.ProjV2 p -> do
      p1 <- freshShape
      p2 <- freshShape
      p' <- fresh (STShape $ PairShape p1 p2)
      unify p p'
      unify sv p2
    U.Arith _ lhs rhs -> binop lhs rhs
    U.Ap1 _ x -> unaryop x
    U.Or lhs rhs -> do
      shapeIs BoolShape
      unify lhs rhs
      unify lhs sv
    U.And lhs rhs -> do
      shapeIs BoolShape
      unify lhs rhs
      unify lhs sv
    U.Not x -> do
      shapeIs BoolShape
      unify sv x
    U.ITE c yes no -> do
      unify yes no
      unify yes sv
      b <- fresh (STShape BoolShape)
      unify c b
    U.RGB r g b -> do
      unify r g
      unify g b
      n <- fresh (STShape NumShape)
      unify n b
      shapeIs ColorShape
    U.Blend s c1 c2 -> do
      shapeIs ColorShape
      unify c1 c2
      unify c1 sv
      n <- fresh (STShape NumShape)
      unify s n
    U.InvertRGB c -> do
      unify sv c
      shapeIs ColorShape
    U.Eql lhs rhs -> do
      shapeIs BoolShape
      unify lhs rhs
    U.NEq lhs rhs -> do
      shapeIs BoolShape
      unify lhs rhs
    U.Cmp _ lhs rhs -> do
      shapeIs BoolShape
      unify lhs rhs
      n <- fresh (STShape NumShape)
      unify lhs n
  pure (Ann sv v)

---------------------------------------------------------------------------------
-- Post-shape analyis results
---------------------------------------------------------------------------------

data TypeVar
  = BoolTV
  | NumTV !TV
  | ColorTV
  | PairTV TypeVar TypeVar
  | ErrorTV ShapeError
  deriving (Eq, Ord, Show)

toTypeShape :: forall s
             . STRef s TV
            -> ValueWithShape s
            -> StateT (Map String TypeVar) (ST s) (Either ShapeError (ValueWith TypeVar))
toTypeShape nextTV = runExceptT . (foldM $ \(AnnF uf v) -> do
  case v of
    U.Var n -> do
      env <- get
      case Map.lookup n env of
        Nothing -> throwError (Unbound n)
        Just tv -> pure (Ann tv v)
    _ -> Ann <$> lift (lift (shapeToTS nextTV [] uf)) <*> pure v)

-- | Convert a 'Shape' to a 'TypeVar'. When converting pairs,
-- any backedge in the traversal should result in a 'InfiniteShape',
-- indicating an infinite type.
-- 'NumShape' is converted to 'NumTV' by allocating a fresh 'TV'.
shapeToTS :: STRef s TV
          -> [UF s (STShape s)]
          -> UF s (STShape s)
          -> ST s TypeVar
shapeToTS nextTV seen uf = do
  root <- findParent uf
  if root `elem` seen
    then pure (ErrorTV Infinite)
    else (coerce <$> find root) >>= \case
      BoolShape    -> pure BoolTV
      NumShape     -> do
        tv <- readSTRef nextTV
        modifySTRef' nextTV (+ 1)
        pure (NumTV tv)
      ColorShape   -> pure ColorTV
      Unknown      -> pure (ErrorTV Indeterminate)
      ShapeError e -> pure (ErrorTV e)
      PairShape uf1 uf2 -> do
        ts1 <- shapeToTS nextTV (root : seen) uf1
        ts2 <- shapeToTS nextTV (root : seen) uf2
        pure (PairTV ts1 ts2)
