{-# language PolyKinds #-}
module Data.Indexed.Functor
  ( IFunctor(..)
  , (:.:)
  , (:+:)
  , (:*:)
  , ITraversable(..)
  , FIX
  , indexedFold
  , indexedFoldM
  , indexedFoldWithOriginal
  , indexedFoldWithOriginalM
  , indexedUnfold
  , indexedUnfoldM
  ) where

import FractalStream.Prelude

{-| Functors over indexed datatypes.

The straightforward approach would use the kind @(k -> Type) -> (k -> Type)@,
which describes indexed functors on @Hask@. This makes defining the datatypes
easier, but it makes operating with them harder in some annoying ways.

We make a different tradeoff here, using a more complex kind for our indexed
functors, which allows for simpler uses.

Instead of using the kind @(k -> Type) -> (k -> Type)@, we use the kind
@(k -> Exp Type) -> (k -> Type)@, where @Exp@ is from the @first-class-families@
package. So instead of the first parameter of @f@ being an indexed Haskell type,
it will be an indexed type family. Applying @Eval@ to that type family produces
the actual type.

It is also important that we can recover the index @i@ from a value of type
@f a i@, which we do through the @IndexProxy@ type family and @toIndex@ function.
-}
class IFunctor (f :: (k -> Exp Type) -> (k -> Type)) where
  type IndexProxy f :: k -> Type
  imap :: forall a b i
        . (forall j. IndexProxy f j -> Eval (a j) -> Eval (b j))
       -> f a i
       -> f b i
  toIndex :: forall a i. f a i -> IndexProxy f i

-- | Compose an indexed type family with a normal type constructor.
data (:.:) (m :: Type -> Type) (a :: k -> Exp Type) (i :: k) :: Exp Type
type instance Eval ((m :.: a) i) = m (Eval (a i))

-- | Sum two indexed type families pointwise.
data (:+:) (a :: k -> Exp Type) (b :: k -> Exp Type) (i :: k) :: Exp Type
type instance Eval ((a :+: b) i) = Either (Eval (a i)) (Eval (b i))

-- | Pair two indexed type families pointwise.
data (:*:) (a :: k -> Exp Type) (b :: k -> Exp Type) (i :: k) :: Exp Type
type instance Eval ((a :*: b) i) = (Eval (a i), Eval (b i))

-- | @ITraversable@ is to @IFunctor@ as @Traversable@ is to @Functor@:
-- it allows you to map an applicative action instead of mapping a pure
-- function.
class IFunctor f => ITraversable (f :: (k -> Exp Type) -> (k -> Type)) where

  isequence :: forall i m a
             . Applicative m
            => f (m :.: a) i
            -> m (f a i)
  isequence = itraverse (const id)

  itraverse :: forall i m a b
             . Applicative m
            => (forall j. IndexProxy f j -> Eval (a j) -> m (Eval (b j)))
            -> f a i
            -> m (f b i)
  itraverse f = isequence . imap f

-- | @FIX f@ is an indexed type family that represents the least fixpoint
-- of the indexed functor @f@.
data FIX :: ((k -> Exp Type) -> k -> Type) -> k -> Exp Type
type instance Eval (FIX f i) = f (FIX f) i


{-| Fold a function over an indexed recursive datatype.

@
  -----------------------------------------------------
  -- An index to determines the type of an AST:
  -- either an integer, or a boolean.
  -----------------------------------------------------
  data T = IntT | BoolT

  data TProxy (ty :: T) where
    IsInt  :: TProxy 'IntT
    IsBool :: TProxy 'BoolT

  -----------------------------------------------------
  -- An indexed functor describing an expression AST.
  -- This is built mechanically from the a normal recursive
  -- Ast type by adding a type parameter `ast` of kind
  -- T -> Exp Type, where T is the index type, and replacing
  -- all recursive uses of `Ast i` with `Eval (ast i)`.
  -----------------------------------------------------

  type Ast = AstF (FIX AstF)

  data AstF (ast :: T -> Exp Type) (i :: T) where
    Yes    :: forall ast. AstF ast 'BoolT
    No     :: forall ast. AstF ast 'BoolT
    Number :: forall ast. Int -> AstF ast 'IntT
    Equals :: forall ast t. TProxy t -> Eval (ast t) -> Eval (ast t) -> AstF ast 'BoolT
    Plus   :: forall ast. Eval (ast 'IntT) -> Eval (ast 'IntT) -> AstF ast 'IntT
    Times  :: forall ast. Eval (ast 'IntT) -> Eval (ast 'IntT) -> AstF ast 'IntT

  -----------------------------------------------------
  -- The IFunctor instance for AstF. We must define
  -- a type that acts as a value-level proxy (singleton)
  -- for the type index.
  -----------------------------------------------------

  instance IFunctor AstF where
    type IndexProxy AstF = TProxy

    imap f = \case
      Yes        -> Yes
      No         -> No
      Number n   -> Number n
      Plus x y   -> Plus (f IsInt x) (f IsInt y)
      Times x y  -> Times (f IsInt x) (f IsInt y)
      Equals t x y -> Equals t (f t x) (f t y)

    toIndex = \case
      Yes -> IsBool
      No  -> IsBool
      Number {} -> IsInt
      Plus {}   -> IsInt
      Times {}  -> IsInt
      Equals {} -> IsBool

  -----------------------------------------------------
  -- An indexed family of types, describing a
  -- Haskell type that can be used as the evaluation
  -- target for a given index.
  -----------------------------------------------------
  data HaskellType_ :: T -> Exp Type
  type instance Eval (HaskellType_ 'IntT)  = Int
  type instance Eval (HaskellType_ 'BoolT) = Bool

  type HaskellType t = Eval (HaskellType_ t)

  -----------------------------------------------------
  -- An evaluator, from Asts to values of the
  -- corresponding Haskell type. Computed using
  -- an indexed fold, so the recursive values are
  -- "already computed" and have the right type.
  -----------------------------------------------------
  eval :: Ast t -> HaskellType t
  eval = indexedFold @HaskellType_ $ \case
    Yes        -> True
    No         -> False  -- Note that the resulting values
    Number n   -> n      -- can have different Haskell types!
    Plus x y   -> x + y
    Times x y  -> x * y
    Equals t x y ->
      case t of           -- Must match on index to get
        IsInt  -> x == y  -- the correct Eq instance for
        IsBool -> x == y  -- each possible type
@

>>> eval (Plus (Number 6) (Number 7))
42

>>> eval (Equals IsInt (Plus (Number 1) (Number 2)) (Number 3))
True

Note: the type parameter `a` is generally not inferrable here,
so this should either be used as `indexFold f` where `f`
has been given a type signature, or as `indexedFold @X` to
explicitly instantiate the type variable `a` as `X`.
-}
indexedFold :: forall a f i. IFunctor f
            => (forall j. f a j -> Eval (a j))
            -> Eval (FIX f i)
            -> Eval (a i)
indexedFold f x =
  let go :: forall k. IndexProxy f k -> Eval (FIX f k) -> Eval (a k)
      go _ = f . imap go
  in go (toIndex x) x

indexedFoldM :: forall a f i m
              . (ITraversable f, Monad m)
             => (forall j. f a j -> m (Eval (a j)))
             -> Eval (FIX f i)
             -> m (Eval (a i))
indexedFoldM f x =
  let go :: forall k. IndexProxy f k -> Eval (FIX f k) -> m (Eval (a k))
      go _ = f <=< itraverse go
  in go (toIndex x) x

indexedUnfold :: forall a f i
               . IFunctor f
              => (forall j. IndexProxy f j -> Eval (a j) -> f a j)
              -> IndexProxy f i
              -> Eval (a i)
              -> Eval (FIX f i)
indexedUnfold f =
  let go :: forall k. IndexProxy f k -> Eval (a k) -> Eval (FIX f k)
      go = \k -> imap go . f k
  in go

indexedUnfoldM :: forall a f i m
               . (IFunctor f, ITraversable f, Monad m)
              => (forall j. IndexProxy f j -> Eval (a j) -> m (f a j))
              -> IndexProxy f i
              -> Eval (a i)
              -> m (Eval (FIX f i))
indexedUnfoldM f =
  let go :: forall k. IndexProxy f k -> Eval (a k) -> m (Eval (FIX f k))
      go = \k x -> itraverse go =<< f k x
  in go

-- | Perform an @indexedFold@, but also make the original sub-ASTs
-- available during the fold.
indexedFoldWithOriginal
  :: forall a f i
   . IFunctor f
  => (forall j. f (FIX f :*: a) j -> Eval (a j))
  -> Eval (FIX f i)
  -> Eval (a i)
indexedFoldWithOriginal f =
  snd . indexedFold (\x -> ((imap (const fst) x), f x))

-- | Perform an @indexedFoldM@, but also make the original sub-ASTs
-- available during the fold.
indexedFoldWithOriginalM
  :: forall a f i m
   . (Monad m, ITraversable f)
  => (forall j. f (FIX f :*: a) j -> m (Eval (a j)))
  -> Eval (FIX f i)
  -> m (Eval (a i))
indexedFoldWithOriginalM f =
  fmap snd . indexedFoldM (\x -> ((imap (const fst) x),) <$> f x)
