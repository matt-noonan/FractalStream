{-# language AllowAmbiguousTypes #-}
module Language.Value.Parser
  ( parseValue
  , parseValueFromTokens
  , BadParse(..)
  , value_
  , valueRules
  , tokenize
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
import Data.Color

import Fcf (Eval)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TypeLits
import Data.Ord

---------------------------------------------------------------------------------
-- Top-level entry points
---------------------------------------------------------------------------------

parseValue :: EnvironmentProxy env
           -> ScalarProxy t
           -> String
           -> Either (Int, BadParse) (Value env t)
parseValue env t input = parseValueFromTokens env t (tokenize input)

parseValueFromTokens
  :: forall env t
   . EnvironmentProxy env
  -> ScalarProxy t
  -> [Token]
  -> Either (Int, BadParse) (Value env t)
parseValueFromTokens env t toks
   = withEnvironment env
   $ withKnownType t
   $ parse value_ toks

value_ :: forall t env
        . (KnownEnvironment env, KnownType t)
       => Parser (Value env t)
value_ = runIParser (parsePrio (valueRules @_ @env Fix)) (typeProxy @t)

---------------------------------------------------------------------------------
-- Rules
---------------------------------------------------------------------------------

valueRules :: forall value env
            . (KnownEnvironment env)
           => (forall i. ValueF env value i -> Eval (value i))
           -> PriorityParser value
valueRules inj = mconcat rules
  where
    op2 :: forall i. (Eval (value i) -> Eval (value i) -> ValueF env value i)
      -> Eval (value i) -> Eval (value i) -> Eval (value i)
    op2 c x y = inj (c x y)
    op1 :: forall i. (Eval (value i) -> ValueF env value i)
        -> Eval (value i) -> Eval (value i)
    op1 c x = inj (c x)
    mkAtom :: ScalarProxy i -> String -> Parser (ValueF env value i) -> PriorityParser value
    mkAtom t name p = leveledParser 0 t $ \_ -> dbg name ((inj <$> p) <?> name)
    mkAtom' :: ScalarProxy i -> String -> (IParser value -> Parser (ValueF env value i)) -> PriorityParser value
    mkAtom' t name p = leveledParser 0 t $ \step -> dbg name ((inj <$> p (pFull step)) <?> name)
    funNames = Map.keysSet funR2R `Set.union` Map.keysSet funC2R

    funNameR = void $ satisfy $ \case
      Identifier f -> f `Set.member` funNames
      _            -> False
    funNameC = void $ satisfy $ \case
      Identifier f -> f `Map.member` funC2C
      _            -> False
    color = inj . Const . Scalar ColorProxy
    twoThirds = inj (Const (Scalar RealProxy 0.66))

    rules :: [PriorityParser value]
    rules =

      ---------------------------------------------------------------------
      -- Boolean operators
      ---------------------------------------------------------------------

      [ infixL Or_    3002 BooleanProxy "boolean disjunction" (op2 Or)
      , infixL And_   2902 BooleanProxy "boolean conjunction" (op2 And)

      , leveledParser 2802 BooleanProxy $ \ParserStep{..} -> dbg "equality" $ choice
          [ try (inj <$> (Eql IntegerProxy
                      <$> (runIParser pFull IntegerProxy <* tok_ Equal)
                      <*> (runIParser pFull IntegerProxy)))
          , try (inj <$> (Eql RealProxy
                      <$> (runIParser pFull RealProxy <* tok_ Equal)
                      <*> (runIParser pFull RealProxy)))
          , try (inj <$> (Eql ComplexProxy
                      <$> (runIParser pFull ComplexProxy <* tok_ Equal)
                      <*> (runIParser pFull ComplexProxy)))
          , do
              lhs <- runIParser pNext BooleanProxy
              (inj . Eql BooleanProxy lhs
               <$> (tok_ Equal *> runIParser pNext BooleanProxy)) <|> pure lhs
          ]
      , leveledParser 2801 BooleanProxy $ \ParserStep{..} -> dbg "inequality" $ choice
          [ try (inj <$> (NEq IntegerProxy
                      <$> (runIParser pFull IntegerProxy <* tok_ NotEqual)
                      <*> (runIParser pFull IntegerProxy)))
          , try (inj <$> (NEq RealProxy
                      <$> (runIParser pFull RealProxy <* tok_ NotEqual)
                      <*> (runIParser pFull RealProxy)))
          , try (inj <$> (NEq ComplexProxy
                      <$> (runIParser pFull ComplexProxy <* tok_ NotEqual)
                      <*> (runIParser pFull ComplexProxy)))
          , do
              lhs <- runIParser pNext BooleanProxy
              (inj . NEq BooleanProxy lhs
               <$> (tok_ NotEqual *> runIParser pNext BooleanProxy)) <|> pure lhs
          ]

      , leveledParser 2702 BooleanProxy $ \ParserStep{..} -> dbg "comparison" (choice
          [ try $ do
              lhs <- runIParser pFull IntegerProxy
              opTok <- satisfy (`Map.member` opsI)
              inj <$> (opsI Map.! opTok) lhs <$> runIParser pFull IntegerProxy
          , try $ do
              lhs <- runIParser pFull RealProxy
              opTok <- satisfy (`Map.member` opsF)
              inj <$> (opsF Map.! opTok) lhs <$> runIParser pFull RealProxy
          , runIParser pNext BooleanProxy
          ] <?> "comparison")

      , prefixOp Not_ 2602 BooleanProxy "boolean negation" (op1 Not)

      ---------------------------------------------------------------------
      -- Arithmetic operators
      ---------------------------------------------------------------------

      -- Integer arithmetic
      , infixL Plus    2000 IntegerProxy "addition"    (op2 AddI)
      , infixL Minus   1900 IntegerProxy "subtraction" (op2 SubI)
      , infixL Divide  1800 IntegerProxy "division"    (op2 DivI)
      , infixL Times   1700 IntegerProxy "multiplication" (op2 MulI)
      , leveledParser  1600 IntegerProxy $ \ParserStep{..} -> dbg "implicit multiplication" $
          (foldl1 (\x y -> inj (MulI x y)) <$> do
              -- No integer-valued functions that have optional
              -- parentheses, but we'll keep the form in case we
              -- want to add some later on.
              let factor = (False,) <$> runIParser pNext IntegerProxy
              factors <- some (notFollowedBy (tok_ Minus) *> try factor)
              if any fst (init factors)
                then bad AmbiguousParse
                else pure (map snd factors))
          <|> runIParser pNext IntegerProxy
      , prefixOp Minus 1500 IntegerProxy "negation"    (op1 NegI)
      , infixR Caret   1400 IntegerProxy "power"       (op2 PowI)

      -- Real arithmetic
      , infixL Plus    2001 RealProxy "addition"    (op2 AddF)
      , infixL Minus   1901 RealProxy "subtraction" (op2 SubF)
      , infixL Divide  1801 RealProxy "division"    (op2 DivF)
      , infixL Times   1701 RealProxy "multiplication" (op2 MulF)
      , leveledParser  1601 RealProxy $ \ParserStep{..} -> dbg "implicit multiplication" $ do
          (foldl1 (\x y -> inj (MulF x y)) <$> do
              -- If any factor except the last is a non-parenthesized
              -- function call, complain about an ambiguous parse.
              let factor = do
                    isNonParenFun <-
                      (True <$ lookAhead (try (funNameR >> notFollowedBy (tok_ OpenParen))))
                      <|> pure False
                    (isNonParenFun,) <$> runIParser pNext RealProxy
              factors <- some (notFollowedBy (tok_ Minus) *> try factor)
              if any fst (init factors)
                then bad AmbiguousParse
                else pure (map snd factors))
           <|> runIParser pNext RealProxy
      , prefixOp Minus 1501 RealProxy "negation"    (op1 NegF)
      , infixR Caret   1401 RealProxy "power"       (op2 PowF)

      -- Complex arithmetic
      , infixL Plus    2002 ComplexProxy "addition"    (op2 AddC)
      , infixL Minus   1902 ComplexProxy "subtraction" (op2 SubC)
      , infixL Divide  1802 ComplexProxy "division"    (op2 DivC)
      , infixL Times   1702 ComplexProxy "multiplication" (op2 MulC)
      , leveledParser  1602 ComplexProxy $ \ParserStep{..} -> dbg "implicit multiplication" $ do
          (foldl1 (\x y -> inj (MulC x y)) <$> do
              -- If any factor except the last is a non-parenthesized
              -- function call, complain about an ambiguous parse.
              let factor = do
                    isNonParenFun <-
                      (True <$ lookAhead (try (funNameC >> notFollowedBy (tok_ OpenParen))))
                      <|> pure False
                    (isNonParenFun,) <$> runIParser pNext ComplexProxy
              factors <- some (notFollowedBy (tok_ Minus) *> try factor)
              if any fst (init factors)
                then bad AmbiguousParse
                else pure (map snd factors))
           <|> runIParser pNext ComplexProxy
      , prefixOp Minus 1502 ComplexProxy "negation"    (op1 NegC)
      , infixR Caret   1402 ComplexProxy "power"       (op2 PowC)

      -- Polymorphic operators
      , PriorityParser . Map.singleton (Down 1000) $ \ParserStep{..} ->
          IParser $ \t -> dbg "conditional" ((do
            tok_ If
            cond <- runIParser pFull BooleanProxy
            tok_ Then
            yes <- runIParser pFull t
            tok_ Else
            no  <- runIParser pFull t
            pure (inj (ITE t cond yes no)))
                          <|> runIParser pNext t
                          <?> "conditional")

      ---------------------------------------------------------------------
      -- Color operators
      ---------------------------------------------------------------------

      , prefixOp (Identifier "dark") 0 ColorProxy "dark" $ \col ->
          inj (Blend twoThirds col (color black))

      , prefixOp (Identifier "light") 0 ColorProxy "light" $ \col ->
          inj (Blend twoThirds col (color white))

      , prefixOp (Identifier "invert") 0 ColorProxy "invert" (op1 InvertRGB)

      , mkAtom' ColorProxy "blend operator" $ \top -> do
          tok_ (Identifier "blend")
          tok_ OpenParen
          s  <- runIParser top RealProxy  <* tok_ Comma
          c1 <- runIParser top ColorProxy <* tok_ Comma
          c2 <- runIParser top ColorProxy <* tok_ CloseParen
          pure (Blend s c1 c2)

      , mkAtom' ColorProxy "color constructor" $ \top -> do
          tok_ (Identifier "rgb")
          tok_ OpenParen
          r <- runIParser top RealProxy <* tok_ Comma
          g <- runIParser top RealProxy <* tok_ Comma
          b <- runIParser top RealProxy <* tok_ CloseParen
          pure (RGB r g b)

      , mkAtom ColorProxy "named color" $ do
          Identifier c <- satisfy (\case { Identifier c -> Map.member c colors
                                         ; _ -> False })
          pure (Const (Scalar ColorProxy (colors Map.! c)))

      ---------------------------------------------------------------------
      -- Function application and implicit coercions
      ---------------------------------------------------------------------

      -- Real functions of one variable (real or complex)
      , PriorityParser . Map.singleton (Down 10) $ \step ->
          IParser $ \case
            RealProxy -> dbg "real function of one variable" $ do
              Identifier f <- satisfy (\case { Identifier f -> Set.member f funNames
                                             ; _ -> False })
              case Map.lookup f funR2R of
                Just fun -> inj . fun <$> runIParser (pSame step) RealProxy
                Nothing -> case Map.lookup f funC2R of
                  Just fun -> inj . fun <$> runIParser (pSame step) ComplexProxy
                  Nothing -> error "internal error, should be unreachable"
            _ -> mzero

      -- Complex functions of one complex variable
      , PriorityParser . Map.singleton (Down 10) $ \step ->
          IParser $ \case
            ComplexProxy -> dbg "complex function of one variable" $ do
              Identifier f <- satisfy (\case { Identifier f -> Map.member f funC2C
                                             ; _ -> False })
              case Map.lookup f funC2C of
                Just fun -> inj . fun <$> runIParser (pSame step) ComplexProxy
                Nothing -> error "internal error, should be unreachable"
            _ -> mzero

      -- Implicit coercions
      , PriorityParser . Map.singleton (Down 10) $ \ParserStep{..} ->
          IParser $ \case
            ComplexProxy -> dbg "complex" (choice
              [ try (runIParser pNext ComplexProxy)
              , try (inj . R2C <$> runIParser pNext RealProxy)
              , inj . R2C . inj . I2R <$> runIParser pNext IntegerProxy
              ] <?> "basic complex value")
            RealProxy -> dbg "real" (choice
              [ try (runIParser pNext RealProxy)
              , inj . I2R <$> runIParser pNext IntegerProxy
              ] <?> "basic real value")
            t -> runIParser pNext t

      ---------------------------------------------------------------------
      -- Atoms
      ---------------------------------------------------------------------

      -- Boolean constants
      , mkAtom BooleanProxy "true"  (tok_ True_  >> pure (Const (Scalar BooleanProxy True)))
      , mkAtom BooleanProxy "false" (tok_ False_ >> pure (Const (Scalar BooleanProxy False)))

      -- Integer constants
      , mkAtom IntegerProxy "integer constant" $ do
          NumberI n <- satisfy (\case { NumberI _ -> True; _ -> False })
          pure (Const (Scalar IntegerProxy (fromIntegral n)))

      -- Real constants, including promoted integer constants
      , mkAtom RealProxy "real constant" $ do
          c <- satisfy (\case { NumberI _ -> True
                              ; NumberF _ -> True
                              ; _         -> False })
          case c of
            NumberI n -> pure (Const (Scalar RealProxy (fromIntegral n)))
            NumberF n -> pure (Const (Scalar RealProxy n))
            _         -> mzero

      , mkAtom RealProxy "Euler's constant"
        (Const (Scalar RealProxy (exp 1)) <$ tok_ Euler)

      , mkAtom RealProxy "pi"
        (Const (Scalar RealProxy pi) <$ tok_ Pi)

      -- Complex constants, including promoted real and integer constants
      , mkAtom ComplexProxy "complex constant" $ do
          c <- satisfy (\case { NumberI _ -> True
                              ; NumberF _ -> True
                              ; _         -> False })
          case c of
            NumberI n -> pure (Const (Scalar ComplexProxy (fromIntegral n)))
            NumberF n -> pure (Const (Scalar ComplexProxy (n :+ 0)))
            _         -> mzero

      , mkAtom ComplexProxy "imaginary unit"
        (Const (Scalar ComplexProxy (0 :+ 1)) <$ tok_ I)

      -- mod functions
      , mkAtom' IntegerProxy "integer modulo operator" $ \top -> do
          tok_ (Identifier "mod") >> tok_ OpenParen
          x <- runIParser top IntegerProxy <* tok_ Comma
          y <- runIParser top IntegerProxy <* tok_ CloseParen
          pure (ModI x y)
      , mkAtom' RealProxy "real modulo operator" $ \top -> do
          tok_ (Identifier "mod") >> tok_ OpenParen
          x <- runIParser top RealProxy <* tok_ Comma
          y <- runIParser top RealProxy <* tok_ CloseParen
          pure (ModF x y)

      -- Absolute value bars
      , PriorityParser . Map.singleton (Down 0) $ \step ->
          IParser $ \case
            IntegerProxy -> dbg "integer absolute value" (do
              tok_ Bar
              v <- runIParser (pFull step) IntegerProxy
              tok_ Bar
              pure (inj (AbsI v))) <?> "integer absolute value"
            RealProxy -> dbg "real or complex absolute value" $ do
              tok_ Bar
              -- Could be a real or a complex argument, try both
              try ((do
                      v <- runIParser (pFull step) RealProxy <* tok_ Bar
                      pure (inj (AbsF v))) <?> "real absolute value")
               <|> ((do
                      v <- runIParser (pFull step) ComplexProxy <* tok_ Bar
                      pure (inj (AbsC v))) <?> "complex magnitude")
            _ -> mzero

      -- Variables
      , PriorityParser . Map.singleton (Down 0) $ \_ ->
          IParser $ \t -> dbg "variable" (do
            Identifier n <- satisfy (\case { Identifier _ -> True; _ -> False })
            case someSymbolVal n of
              SomeSymbol name ->
                case lookupEnv name t (envProxy (Proxy @env)) of
                  Found pf  -> pure (inj (Var name t pf))
                  Absent _  -> bad (UnboundVariable n)
                  WrongType -> bad (MismatchedType n)) <?> "variable"

      -- Ordered pairs
      , PriorityParser . Map.singleton (Down 0) $ \step -> do
          IParser $ \case
            p@(PairProxy t1 t2) -> dbg "ordered pair" (do
              tok_ OpenParen
              x <- runIParser (pFull step) t1 <* tok_ Comma
              y <- runIParser (pFull step) t2 <* tok_ CloseParen
              pure (inj (PairV p x y))) <?> "ordered pair"
            _ -> mzero

      -- Parenthesized subexpressions
      , PriorityParser . Map.singleton (Down 0) $ \step -> do
          IParser $ \t -> dbg "parenthesized subexpression"
            (tok_ OpenParen *> runIParser (pFull step) t <* tok_ CloseParen)
            <?> "parenthesized expression"
      ]

---------------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------------

funR2R :: Map String (Eval (value 'RealT) -> ValueF env value 'RealT)
funR2R = Map.fromList
  [ ("exp", ExpF), ("log", LogF), ("abs", AbsF)
  , ("cos", CosF), ("sin", SinF), ("tan", TanF)
  , ("arccos", ArccosF), ("arcsin", ArcsinF), ("arctan", ArctanF)
  , ("acos", ArccosF), ("asin", ArcsinF), ("atan", ArctanF)
  , ("cosh", CoshF), ("sinh", SinhF), ("tanh", TanhF)
  , ("arccosh", ArccoshF), ("arcsinh", ArcsinhF), ("arctanh", ArctanhF)
  , ("acosh", ArccoshF), ("asinh", ArcsinhF), ("atanh", ArctanhF)
  ]

funC2C :: Map String (Eval (value 'ComplexT) -> ValueF env value 'ComplexT)
funC2C = Map.fromList
  [ ("exp", ExpC), ("log", LogC)
  , ("cos", CosC), ("sin", SinC), ("tan", TanC)
  , ("cosh", CoshC), ("sinh", SinhC), ("tanh", TanhC)
  , ("bar", ConjC), ("conj", ConjC)
  ]

funC2R :: Map String (Eval (value 'ComplexT) -> ValueF env value 'RealT)
funC2R = Map.fromList
  [ ("re", ReC), ("im", ImC), ("arg", ArgC) ]

colors :: Map String Color
colors = Map.fromList
  [ ("red", red), ("green", green), ("blue", blue)
  , ("black", black), ("white", white), ("grey", grey), ("gray", grey)
  , ("orange", orange), ("yellow", yellow), ("purple", purple), ("violet", violet) ]

opsI :: forall value env
      . Map Token (   Eval (value 'IntegerT)
                   -> Eval (value 'IntegerT)
                   -> ValueF env value 'BooleanT)
opsI = Map.fromList
  [ (GreaterThan, GTI), (GreaterThanOrEqual, GEI)
  , (LessThan, LTI), (LessThanOrEqual, LEI)
  ] -- , (Equal, Eql IntegerProxy), (NotEqual, NEq IntegerProxy) ]

opsF :: forall value env
      . Map Token (   Eval (value 'RealT)
                   -> Eval (value 'RealT)
                   -> ValueF env value 'BooleanT)
opsF = Map.fromList
  [ (GreaterThan, GTF), (GreaterThanOrEqual, GEF)
  , (LessThan, LTF), (LessThanOrEqual, LEF)
  ] -- , (Equal, Eql RealProxy), (NotEqual, NEq RealProxy) ]
