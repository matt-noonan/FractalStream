{-# language AllowAmbiguousTypes #-}
{-# options_ghc -Wno-unused-imports #-}
module Language.Value.Parser
  ( parseValue
  , parseValueFromTokens
  , parseUntypedValue
  , parseUntypedValueFromTokens
  , BadParse(..)
  , value_
  , valueRules
  , tokenize
  , untypedValue_
  , Token(..)
  , PriorityParser(..)
  , parsePrio
  , infixL
  , infixR
  ) where

import Language.Type
import Language.Value
import Language.Parser
import Data.Indexed.Functor
import qualified Data.Recursive as U
import Data.Color

import Fcf (Eval, Pure1)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TypeLits
import Data.Ord

import qualified Language.Untyped.Value as U

import Helper.UnionFind
import Data.STRef
import Control.Monad.State
import qualified Data.Map as Map
import GHC.TypeLits
import Control.Monad.Except
import Data.Functor ((<&>))
import Language.Untyped.Shape
import Language.Untyped.Infer
import Language.Untyped.Constraints (initialTCState)

import Debug.Trace

---------------------------------------------------------------------------------
-- Top-level entry points
---------------------------------------------------------------------------------

parseValue :: EnvironmentProxy env
  -> Context (Splice env) splices
           -> TypeProxy t
           -> String
           -> Either (Int, BadParse) (Value '(env, t))
parseValue env splices t input = parseValueFromTokens env splices t (tokenize input)

parseValueFromTokens
  :: forall env splices t
   . EnvironmentProxy env
  -> Context (Splice env) splices
  -> TypeProxy t
  -> [Token]
  -> Either (Int, BadParse) (Value '(env, t))
parseValueFromTokens env splices t toks
   = withEnvironment env
   $ withKnownType t
   $ parse (value_ splices) toks

value_ :: forall t env splices
        . (KnownEnvironment env, KnownType t)
       => Context (Splice env) splices
       -> Parser (Value '(env, t))
value_ _splices = case envProxy (Proxy @env) of
  env -> case typeProxy @t of
    rt -> do
      --traceM "about to parse an untyped value"
      uv <- dbg "untyped value" $ untypedValue_
      --traceM ("uv = " ++ show uv)
      let result = do
            -- Infer the value's shape
            (v, ctx') <- runST $ do
              ctx <- fmap Map.fromList $ fromEnvironmentM env $ \name ty -> do
                let go :: forall ty s. TypeProxy ty -> ST s (UF s (STShape s))
                    go = \case
                      BooleanType -> fresh (STShape BoolShape)
                      IntegerType -> fresh (STShape NumShape)
                      RealType    -> fresh (STShape NumShape)
                      ComplexType -> fresh (STShape NumShape)
                      ColorType   -> fresh (STShape ColorShape)
                      PairType x y -> do
                        ps <- PairShape <$> go x <*> go y
                        fresh (STShape ps)
                      _ -> error "missing case"
                t <- go ty
                pure (symbolVal name, t)

              s <- inferShape ctx uv

              nextTV <- newSTRef 0
              ctx' <- forM ctx (shapeToTS nextTV [])
              evalStateT (toTypeShape nextTV s) ctx' <&> \case
                Left  e -> Left . (-1,) $ case e of
                  Unbound n -> UnboundVariable n
                  _         -> Other (show e)
                Right v -> pure (v, ctx')

            -- traceM ("v = " ++ show v)
            -- Infer the value's type
            case evalStateT (infer env ctx' rt v) initialTCState of
              Right typedValue -> pure typedValue
              Left e -> throwError . (-1,) $ case e of
                _ -> Other (show e)
      case result of
        Right v -> pure v
        Left (pos,e)  -> parseError (FancyError pos (Set.singleton (ErrorCustom e)))

parseUntypedValue :: String
                  -> Either (Int, BadParse) U.Value
parseUntypedValue = parseUntypedValueFromTokens . tokenize

parseUntypedValueFromTokens :: [Token]
                            -> Either (Int, BadParse) U.Value
parseUntypedValueFromTokens = parse untypedValue_

untypedValue_ :: Parser U.Value
untypedValue_ = parsePrio (valueRules U.Fix)

---------------------------------------------------------------------------------
-- Rules
---------------------------------------------------------------------------------

valueRules :: forall value
            . (U.ValueF value -> value)
           -> PriorityParser value
valueRules inj = mconcat rules
  where
    op2 c x y = inj (c x y)
    op1 c x   = inj (c x)
    mkAtom  name p = leveledParser 0 (\_ -> dbg name ((inj <$> p) <?> name))
    mkAtom' name p = leveledParser 0 (\step -> dbg name ((inj <$> p (pFull step)) <?> name))
    color = inj . U.ConstColor
    twoThirds = inj (U.ConstF 0.66)

    rules :: [PriorityParser value]
    rules =
      ---------------------------------------------------------------------
      -- Boolean operators
      ---------------------------------------------------------------------

      [ infixL Or_      3002 "disjunction" (op2 U.Or)
      , infixL And_     2902 "conjunction" (op2 U.And)
      , infixL Equal    2802 "equality" (op2 U.Eql)
      , infixL NotEqual 2801 "inequality" (op2 U.NEq)
      , leveledParser 2800 $ \ParserStep{..} -> dbg "boolean operator" (do
          lhs <- pNext
          (do
              opTok <- satisfy (`Map.member` bops)
              inj <$> (bops Map.! opTok) lhs <$> pNext) <|> pure lhs
        ) <?> "comparison"
      , prefixOp Not_   2602 "boolean negation" (op1 U.Not)

      ---------------------------------------------------------------------
      -- Arithmetic operators
      ---------------------------------------------------------------------

      , infixL Plus    2000 "addition"    (op2 (U.Arith U.Add))
      , infixL Minus   1900 "subtraction" (op2 (U.Arith U.Sub))
      , infixL Divide  1800 "division"    (op2 (U.Arith U.Div))
      , infixL Times   1700 "multiplication" (op2 (U.Arith U.Mul))
      , leveledParser  1600 $ \ParserStep{..} -> dbg "implicit multiplication" $
          (foldl1 (op2 (U.Arith U.Mul)) <$> do
              -- Look ahead to figure out which factors are function applications
              -- that elide the parentheses around their argument. We'll allow
              -- this to occur in the last factor only, and raise an ambiguous
              -- parse error otherwise. This means that we will parse things
              -- like `2 cos x` but not `cos 2x`; in the latter case, there is
              -- ambiguity between `cos (2x)` or `(cos 2) x`.
              let peekNoParenFun =
                    lookAhead (try $ do
                                  _ <- satisfy $ \case
                                         Identifier n -> Map.member n functions
                                         _            -> False
                                  notFollowedBy (tok_ OpenParen)
                                  pure True) <|> pure False
              let factor = (,) <$> peekNoParenFun <*> pNext
              factors <- some (notFollowedBy (tok_ Minus) *> try factor)
              if any fst (init factors)
                then bad AmbiguousParse
                else pure (map snd factors))
          <|> pNext
      , prefixOp Minus 1500 "negation"    (op1 (U.Ap1 U.Neg))
      , infixR Caret   1400 "power"       (op2 (U.Arith U.Pow))

      ---------------------------------------------------------------------
      -- If/then/else
      ---------------------------------------------------------------------

      , PriorityParser . Map.singleton (Down 1000) $ \ParserStep{..} ->
          dbg "conditional" ((do
            tok_ If
            cond <- pFull
            tok_ Then
            yes <- pFull
            tok_ Else
            no  <- pFull
            pure (inj (U.ITE cond yes no)))
                             <|> pNext
                             <?> "conditional")

      ---------------------------------------------------------------------
      -- Color operators
      ---------------------------------------------------------------------

      , prefixOp (Identifier "dark") 0 "dark" $ \col ->
          inj (U.Blend twoThirds col (color black))

      , prefixOp (Identifier "light") 0 "light" $ \col ->
          inj (U.Blend twoThirds col (color white))

      , prefixOp (Identifier "invert") 0 "invert" (op1 U.InvertRGB)

      , mkAtom' "blend operator" $ \top -> do
          tok_ (Identifier "blend")
          tok_ OpenParen
          s  <- top <* tok_ Comma
          c1 <- top <* tok_ Comma
          c2 <- top <* tok_ CloseParen
          pure (U.Blend s c1 c2)

      , mkAtom' "color constructor" $ \top -> do
          tok_ (Identifier "rgb")
          tok_ OpenParen
          r <- top <* tok_ Comma
          g <- top <* tok_ Comma
          b <- top <* tok_ CloseParen
          pure (U.RGB r g b)

      , mkAtom "named color" $ do
          Identifier c <- satisfy (\case { Identifier c -> Map.member c colors
                                         ; _ -> False })
          pure (U.ConstColor (colors Map.! c))

      ---------------------------------------------------------------------
      -- Function application
      ---------------------------------------------------------------------

      , PriorityParser . Map.singleton (Down 10) $ \step ->
          dbg "function of one variable" ((do
              Identifier f <- satisfy (\case { Identifier f -> Map.member f functions
                                             ; _ -> False })
              case Map.lookup f functions of
                Just fun -> inj . U.Ap1 fun <$> pSame step
                Nothing  -> error "internal error, should be unreachable")
                                          <|> pNext step)

      ---------------------------------------------------------------------
      -- Atoms
      ---------------------------------------------------------------------

      -- Boolean constants
      , mkAtom "true"  (tok_ True_  >> pure (U.ConstB True))
      , mkAtom "false" (tok_ False_ >> pure (U.ConstB False))

      -- Integer constants
      , mkAtom "integer constant" $ do
          NumberI n <- satisfy (\case { NumberI _ -> True; _ -> False })
          pure (U.ConstI (fromIntegral n))

      -- Real constants
      , mkAtom "real constant" $ do
          NumberF c <- satisfy (\case { NumberF _ -> True
                                      ; _         -> False })
          pure (U.ConstF c)

      , mkAtom "Euler's constant" (U.ConstF (exp 1) <$ tok_ Euler)

      , mkAtom "pi" (U.ConstF pi <$ tok_ Pi)

      -- Complex constants
      , mkAtom "imaginary unit" (U.ConstC 0 1 <$ tok_ I)

      -- mod functions
      , mkAtom' "modulo operator" $ \top -> do
          tok_ (Identifier "mod") >> tok_ OpenParen
          x <- top <* tok_ Comma
          y <- top <* tok_ CloseParen
          pure (U.Arith U.Mod x y)

      -- Absolute value bars
      , PriorityParser . Map.singleton (Down 0) $ \step ->
          dbg "integer absolute value" (do
              tok_ Bar
              v <- pFull step
              tok_ Bar
              pure (inj (U.Ap1 U.Abs v))) <?> "absolute value"

      -- Variables
      , PriorityParser . Map.singleton (Down 0) $ \_ ->
          dbg "variable" $ do
            Identifier n <- satisfy (\case { Identifier _ -> True
                                           ; _            -> False })
            pure (inj (U.Var n))

      -- Ordered pairs and parenthesized subexpressions
      , PriorityParser . Map.singleton (Down 0) $ \step -> do
          dbg "ordered pair or parenthesized expression" (do
              tok_ OpenParen
              x <- pFull step
              ((do tok_ Comma
                   y <- pFull step <* tok_ CloseParen
                   pure (inj (U.PairV x y))) <?> "ordered pair") <|>
                (tok_ CloseParen >> pure x <?> "parenthesized expression"))
      ]

colors :: Map String Color
colors = Map.fromList
  [ ("red", red), ("green", green), ("blue", blue)
  , ("black", black), ("white", white), ("grey", grey), ("gray", grey)
  , ("orange", orange), ("yellow", yellow), ("purple", purple), ("violet", violet) ]


functions :: Map String U.Fun
functions = Map.fromList
  [ ("exp", U.Exp), ("log", U.Log), ("abs", U.Abs), ("sqrt", U.Sqrt)
  , ("cos", U.Cos), ("sin", U.Sin), ("tan", U.Tan)
  , ("arccos", U.Arccos), ("arcsin", U.Arcsin), ("arctan", U.Arctan)
  , ("acos", U.Arccos), ("asin", U.Arcsin), ("atan", U.Arctan)
  , ("cosh", U.Cosh), ("sinh", U.Sinh), ("tanh", U.Tanh)
  , ("arccosh", U.Arccosh), ("arcsinh", U.Arcsinh), ("arctanh", U.Arctanh)
  , ("acosh", U.Arccosh), ("asinh", U.Arcsinh), ("atanh", U.Arctanh)
  , ("re", U.Re), ("im", U.Im), ("arg", U.Arg)
  ]

bops :: forall value. Map Token (value -> value -> U.ValueF value)
bops = Map.fromList
  [ (GreaterThan, U.Cmp U.GT), (GreaterThanOrEqual, U.Cmp U.GE)
  , (LessThan, U.Cmp U.LT), (LessThanOrEqual, U.Cmp U.LE)
  , (Equal, U.Eql), (NotEqual, U.NEq) ]
