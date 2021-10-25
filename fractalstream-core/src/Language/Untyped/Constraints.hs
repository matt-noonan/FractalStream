module Language.Untyped.Constraints
  ( TCError(..)
  , Ty(..)
  , NumTy(..)
  , toTy
  , TCState(..)
  , initialTCState
  , bindTypeVar
  , getTypeVar
  , freshTypeVar
  , addConstraint
  , addConstraints
  , TVC(..)
  , TV(..)
  , allocateTVC
  , withType
  , joinTy
  , meetTy
  , joinNumTy
  , meetNumTy
  , numTyToTy
  , TypeConstraint(..)
  ) where

import Language.Type

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State.Strict
import Control.Monad.Except

data TCError
  = UnboundVariable String
  | ShadowedVariable String
  | UnboundTypeVariable TV
  | TypeMismatch  Ty Ty
  | TypeMismatch' SomeType SomeType
  | InconsistentType
  | UnderdeterminedType
  | ShapeErr String
  deriving Show

data NumTy = Num_T | Z_T | R_T | C_T | Top_T
  deriving (Eq, Ord, Show)

meetNumTy :: NumTy -> NumTy -> NumTy
meetNumTy = min

joinNumTy :: NumTy -> NumTy -> NumTy
joinNumTy = max

data TypeConstraint
  = Generalizes TV TV
  | IsUsedLike TV NumTy
  | IsDefinedLike TV NumTy
  deriving (Eq, Ord, Show)

initialTCState :: TCState
initialTCState = TCState
  { tvcNode = Map.empty
  , tvcEdges = Set.empty
  , tvcBindings = Map.empty
  , tvcFresh = TV 0
  }

data TCState = TCState
  { tvcNode     :: !(Map TVC Int)
  , tvcEdges    :: !(Set TypeConstraint)
  , tvcBindings :: !(Map String TV)
  , tvcFresh    :: !TV
  }

addConstraint :: MonadState TCState m
              => TypeConstraint
              -> m ()
addConstraint c =
  modify' (\tcs -> tcs { tvcEdges = Set.insert c (tvcEdges tcs) })

addConstraints :: MonadState TCState m
               => [TypeConstraint]
               -> m ()
addConstraints cs = do
  --traceM ("adding constraints: " ++ show cs)
  modify' (\tcs -> tcs {
              tvcEdges = Set.union (Set.fromList cs) (tvcEdges tcs) })

freshTypeVar :: MonadState TCState m
             => m TV
freshTypeVar = do
  tcs <- get
  let tv = tvcFresh tcs
  put (tcs { tvcFresh = tv + 1 })
  pure tv

bindTypeVar :: (MonadState TCState m, MonadError TCError m)
            => String
            -> m TV
bindTypeVar s = do
  tcs <- get
  case Map.lookup s (tvcBindings tcs) of
    Just _  -> throwError (ShadowedVariable s)
    Nothing -> do
      tv <- freshTypeVar
      modify' (\st -> st { tvcBindings = Map.insert s tv (tvcBindings st) })
      pure tv

getTypeVar :: (MonadState TCState m, MonadError TCError m)
           => String
           -> m TV
getTypeVar s = do
  tcs <- get
  case Map.lookup s (tvcBindings tcs) of
    Just tv -> pure tv
    Nothing -> throwError (UnboundVariable s)

allocateTVC :: TVC -> State (Map TVC Int) ()
allocateTVC tvc = do
  m <- get
  case Map.lookup tvc m of
    Just _  -> pure ()
    Nothing -> modify' (Map.insert tvc (Map.size m))

newtype TV = TV Int deriving (Eq, Ord, Num, Show, Enum)

data TVC
  = TyVar TV
  | TyConstHi NumTy
  | TyConstLo NumTy
  deriving (Eq, Ord, Show)

-- | Nearly the same as 'Language.Type', with the addition
-- of a Top type, and bottom and near-bottom types.
data Ty
  = UnknownTy
  | UnknownNumeric
  | Top
  | BooleanTy
  | IntegerTy
  | RealTy
  | ComplexTy
  | ColorTy
  | UnknownPair
  | PairTy Ty Ty
  deriving (Eq, Ord, Show)

numTyToTy :: NumTy -> Ty
numTyToTy = \case
  Num_T -> UnknownNumeric
  Z_T   -> IntegerTy
  R_T   -> RealTy
  C_T   -> ComplexTy
  Top_T -> Top

toTy :: ScalarProxy t -> Ty
toTy = \case
  BooleanProxy -> BooleanTy
  IntegerProxy -> IntegerTy
  RealProxy    -> RealTy
  ComplexProxy -> ComplexTy
  ColorProxy   -> ColorTy
  PairProxy t1 t2 -> PairTy (toTy t1) (toTy t2)
  _ -> UnknownTy

-- | Lift a 'Ty' type to the type level, as a 'ScalarProxy'
withType :: forall a m
          . MonadError TCError m
         => Ty
         -> (forall t. KnownType t => ScalarProxy t -> m a)
         -> m a
withType t k = case t of
  BooleanTy -> k BooleanProxy
  IntegerTy -> k IntegerProxy
  RealTy    -> k RealProxy
  ComplexTy -> k ComplexProxy
  ColorTy   -> k ColorProxy
  PairTy t1 t2 ->
    withType t1 (\ty1 -> withType t2 (\ty2 -> k (PairProxy ty1 ty2)))
  Top -> throwError InconsistentType
  UnknownNumeric -> k IntegerProxy -- default to Integer
  UnknownPair    -> error ("unknown pair") >> throwError UnderdeterminedType
  UnknownTy      -> throwError UnderdeterminedType


{-| Join two 'Ty' types, according to this lattice:

                      Top
                   /   |  \__
                  /    |     \____
                _/     |           \__
              _/       |           /   \
             /         |          /     \
     ComplexTy         |         /       \
        |              |         \PairTy /
      RealTy           |          \     /
        |              |           \   /
     IntegerTy         |            \ /
        |              |             |
   UnknownNumeric   BooleanTy   UnknownPair
               \       |       /
                \      |      /
                 \     |     /
                  \    |    /
                   UnknownTy
-}
joinTy :: Ty -> Ty -> Ty
joinTy lhs rhs
  | lhs >  rhs = joinTy rhs lhs
  | lhs == rhs = lhs
  | otherwise  = case (lhs, rhs) of
      (UnknownTy, x)              -> x
      (UnknownNumeric, IntegerTy) -> IntegerTy
      (UnknownNumeric, RealTy)    -> RealTy
      (UnknownNumeric, ComplexTy) -> ComplexTy
      (UnknownNumeric, _)         -> Top
      (Top, _)                    -> Top
      (BooleanTy, _)              -> Top
      (IntegerTy, RealTy)         -> RealTy
      (IntegerTy, ComplexTy)      -> ComplexTy
      (IntegerTy, _)              -> Top
      (RealTy, ComplexTy)         -> ComplexTy
      (RealTy, _)                 -> Top
      (ComplexTy, _)              -> Top
      (ColorTy, _)                -> Top
      (UnknownPair, PairTy x y)   -> PairTy x y
      (PairTy x y, PairTy x' y')  -> PairTy (joinTy x x') (joinTy y y')
      _ -> error "internal error: did you add a Ty constructor?"

-- | Take the meet of two 'Ty' types, according to the lattice
-- in the documentation of 'joinTy'.
meetTy :: Ty -> Ty -> Ty
meetTy lhs rhs
  | lhs >  rhs = meetTy rhs lhs
  | lhs == rhs = lhs
  | otherwise  = case (lhs, rhs) of
      (UnknownTy, _)              -> UnknownTy
      (UnknownNumeric, IntegerTy) -> UnknownNumeric
      (UnknownNumeric, RealTy)    -> UnknownNumeric
      (UnknownNumeric, ComplexTy) -> UnknownNumeric
      (UnknownNumeric, _)         -> UnknownTy
      (Top, x)                    -> x
      (BooleanTy, _)              -> UnknownTy
      (IntegerTy, RealTy)         -> IntegerTy
      (IntegerTy, ComplexTy)      -> IntegerTy
      (IntegerTy, _)              -> UnknownTy
      (RealTy, ComplexTy)         -> RealTy
      (RealTy, _)                 -> UnknownTy
      (ComplexTy, _)              -> UnknownTy
      (ColorTy, _)                -> UnknownTy
      (UnknownPair, _)            -> UnknownPair
      (PairTy x y, PairTy x' y')  -> PairTy (meetTy x x') (meetTy y y')
      _ -> error "internal error: did you add a Ty constructor?"
