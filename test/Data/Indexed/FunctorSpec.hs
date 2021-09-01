module Data.Indexed.FunctorSpec (spec) where

import Test.Hspec
import Fcf
import Data.Indexed.Functor

-----------------------------------------------------
-- An indexed AST for simple arithmetic statements.
-- The index determines the type of the subtree:
-- either an integer, or a boolean.
-----------------------------------------------------
data T = IntT | BoolT

data Ast (ty :: T) where
  Yes    :: Ast 'BoolT
  No     :: Ast 'BoolT
  Number :: Int -> Ast 'IntT
  Equals :: forall t. IndexProxy t -> Ast t -> Ast t -> Ast 'BoolT
  Plus   :: Ast 'IntT -> Ast 'IntT -> Ast 'IntT
  Times  :: Ast 'IntT -> Ast 'IntT -> Ast 'IntT

-----------------------------------------------------
-- An indexed functor describing the same tree.
-- This is built mechanically from the Ast type
-- by adding a type parameter `ast` of kind T -> Exp *,
-- where T is the index type, and replacing all
-- recursive uses of Ast with `ast`.
-----------------------------------------------------

data AstF (ast :: T -> Exp *) (i :: T) where
  Yes_    :: forall ast. AstF ast 'BoolT
  No_     :: forall ast. AstF ast 'BoolT
  Number_ :: forall ast. Int -> AstF ast 'IntT
  Equals_ :: forall ast t. IndexProxy t -> Eval (ast t) -> Eval (ast t) -> AstF ast 'BoolT
  Plus_   :: forall ast. Eval (ast 'IntT) -> Eval (ast 'IntT) -> AstF ast 'IntT
  Times_  :: forall ast. Eval (ast 'IntT) -> Eval (ast 'IntT) -> AstF ast 'IntT

-----------------------------------------------------
-- The IFunctor instance for AstF. We must define
-- a type that acts as a value-level proxy (singleton)
-- for the type index.
-----------------------------------------------------

data TProxy (ty :: T) where
  IsInt  :: TProxy 'IntT
  IsBool :: TProxy 'BoolT

instance IFunctor AstF where
  type IndexProxy = TProxy

  imap f = \case
    Yes_        -> Yes_
    No_         -> No_
    Number_ n   -> Number_ n
    Plus_ x y   -> Plus_ (f IsInt x) (f IsInt y)
    Times_ x y  -> Times_ (f IsInt x) (f IsInt y)
    Equals_ t x y -> Equals_ t (f t x) (f t y)

  toIndex = \case
    Yes_ -> IsBool
    No_  -> IsBool
    Number_ {} -> IsInt
    Plus_ {}   -> IsInt
    Times_ {}  -> IsInt
    Equals_ {} -> IsBool

-----------------------------------------------------
-- The IFixpoint instance, tying together Ast and
-- AstF. This is fairly mechanical, just replacing
-- Ast constructors with AstF constructors (or vice
-- versa)
-----------------------------------------------------

instance IFixpoint Ast AstF where
  unrollIx = \case
    Yes -> Yes_
    No  -> No_
    Number n -> Number_ n
    Plus x y -> Plus_ x y
    Times x y -> Times_ x y
    Equals t x y -> Equals_ t x y
  rerollIx = \case
    Yes_ -> Yes
    No_  -> No
    Number_ n -> Number n
    Plus_ x y -> Plus x y
    Times_ x y -> Times x y
    Equals_ t x y -> Equals t x y

-----------------------------------------------------
-- An indexed family of types, describing a
-- Haskell type that can be used as the evaluation
-- target for a given index.
-----------------------------------------------------
data HaskellType_ :: T -> Exp *
type instance Eval (HaskellType_ 'IntT)  = Int
type instance Eval (HaskellType_ 'BoolT) = Bool

type HaskellType t = Eval (HaskellType_ t)

-----------------------------------------------------
-- An evaluator, from Asts to values of the
-- corresponding Haskell type. Computed using
-- an indexed fold, so the recursive values are
-- "already computed".
-----------------------------------------------------
eval :: Ast t -> HaskellType t
eval = indexedFold @HaskellType_ $ \case
  Yes_        -> True
  No_         -> False  -- Note that the resulting values
  Number_ n   -> n      -- can have different Haskell types!
  Plus_ x y   -> x + y
  Times_ x y  -> x * y
  Equals_ t x y ->
    case t of           -- Must match on index to get
      IsInt  -> x == y  -- the correct Eq instance for
      IsBool -> x == y  -- each possible type

-----------------------------------------------------

spec :: Spec
spec = do

  describe "Type-safe evaluation of ASTs" $ do

    it "computes the expected Haskell values, of type Bool" $ do
      eval (Equals IsInt (Number 3 `Plus` Number 7) (Number 10)) `shouldBe` True

    it "computes the expected Haskell values, of type Int" $ do
      eval ((Number 3 `Plus` Number 7) `Plus` Number 5) `shouldBe` 15
