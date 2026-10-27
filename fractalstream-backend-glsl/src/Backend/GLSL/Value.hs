-- | Value expression → GLSL codegen.
--
-- 'valueToGlsl' walks a typed 'Value' AST and returns a GLSL expression
-- string.  The carrier monad 'GlslM' accumulates any needed temporary
-- variable declarations (e.g. for 'LocalLet' nodes) into the same 'Writer'
-- stream that the code fold uses, so declarations always appear in the right
-- order relative to their uses.
module Backend.GLSL.Value
  ( GlslM
  , runGlslM
  , sanitizeVar
  , valueToGlsl
  ) where

import Language.Value
import Data.Color (colorToRGB)
import Backend.GLSL.Types

import Control.Monad.State
import Control.Monad.Writer.Strict
import Data.Char      (isAlphaNum, isDigit)
import Data.Complex   (realPart, imagPart)
import Data.List      (isPrefixOf)
import GHC.TypeLits   (symbolVal)

-- | Codegen monad: fresh-name counter (for 'LocalLet' temps) + collected
-- GLSL statement lines.
type GlslM = StateT Int (Writer [String])

-- | A pair-projection selector: first or second component.
data Sel = Fst | Snd

-- | Run a 'GlslM' action from scratch, returning the result and all emitted
-- GLSL lines.
runGlslM :: GlslM a -> (a, [String])
runGlslM m = runWriter (evalStateT m 0)

-- | Generate a unique temporary name.
freshTmp :: GlslM String
freshTmp = do
  n <- get
  put (n + 1)
  pure ("_tmp" ++ show n)

-- ---------------------------------------------------------------------------
-- Variable name sanitisation
-- ---------------------------------------------------------------------------

-- | Map an FS variable name to a valid GLSL identifier.
--
-- Internal FS names (containing spaces/brackets) are mapped to underscore
-- forms that match the uniform names already present in the OpenGL viewer's
-- fragment shader header.
sanitizeVar :: String -> String
sanitizeVar s
  | s == "[internal] iteration count"  = "_iter_count"
  | s == "[internal] stuck"            = "_stuck"
  | s == "[internal] iteration limit"  = "_max_iterations"
  | s == "[internal] escape radius"    = "_escape_radius"
  | s == "[internal] vanishing radius" = "_convergence_radius"
  | s == "[internal] x"               = "_x"
  | s == "[internal] y"               = "_y"
  | s == "[internal] dx"              = "_dx"
  | s == "[internal] dy"              = "_dy"
  | freshPrefix `isPrefixOf` s        = "_f" ++ drop (length freshPrefix) s
  | otherwise                         = sanitizeIdent s
  where freshPrefix = "[internal] fresh #"

-- | Coerce an arbitrary FS variable name into a valid GLSL identifier:
-- non-alphanumeric/underscore characters become @_@, and a leading digit is
-- prefixed with @_@.  (E.g. the wheel desugaring's @#wheel-arg@ → @_wheel_arg@;
-- a @#@ would otherwise be read as a GLSL preprocessor directive.)
sanitizeIdent :: String -> String
sanitizeIdent s =
  case map repl s of
    out@(c:_) | isDigit c -> '_' : out
              | otherwise -> out
    []                    -> "_"
  where repl c = if isAlphaNum c || c == '_' then c else '_'

-- ---------------------------------------------------------------------------
-- Constant rendering
-- ---------------------------------------------------------------------------

constGlsl :: ComplexRep -> TypeProxy t -> HaskellType t -> String
constGlsl cr ty v = case ty of
  BooleanType  -> if v then "true" else "false"
  IntegerType  -> show v
  RealType     -> showF v
  ComplexType  ->
    let re = realPart v
        im = imagPart v
    in crLit cr re im
  ColorType    ->
    let (r, g, b) = colorToRGB v
    in "vec4("
         ++ showF (fromIntegral r / 255.0) ++ ", "
         ++ showF (fromIntegral g / 255.0) ++ ", "
         ++ showF (fromIntegral b / 255.0) ++ ", 1.0)"
  PairType{}   -> error "GLSL backend: pair constants not supported"
  VoidType     -> error "GLSL backend: void constant"
  ImageType    -> error "GLSL backend: image constant"
  TextType     -> error "GLSL backend: text constant"
  RationalType -> error "GLSL backend: rational constant"
  ListType{}   -> error "GLSL backend: list constant"

-- ---------------------------------------------------------------------------
-- Value fold
-- ---------------------------------------------------------------------------

-- | Compile a 'Value' expression to a GLSL expression string.
-- Any needed temporary declarations are emitted via 'tell' into the shared
-- 'GlslM' writer stream.
valueToGlsl :: ComplexRep -> Value '(env, t) -> GlslM String
valueToGlsl cr = go
  where
    go :: forall env' t'. Value '(env', t') -> GlslM String
    go = \case
      Const (Scalar t v) -> pure (constGlsl cr t v)

      Var name _ _ -> pure (sanitizeVar (symbolVal name))

      -- LocalLet: emit a declaration and then compile the body expression.
      LocalLet name vty _ v _ e -> do
        vExpr <- go v
        let varName = sanitizeVar (symbolVal name)
        case glslType cr vty of
          Left err -> error ("GLSL backend: LocalLet with unsupported type: " ++ err)
          Right ty -> tell [ty ++ " " ++ varName ++ " = " ++ vExpr ++ ";"]
        go e

      -- ---------------------------------------------------------------
      -- Real arithmetic
      -- ---------------------------------------------------------------
      AddF x y  -> binM "+" x y
      SubF x y  -> binM "-" x y
      MulF x y  -> binM "*" x y
      DivF x y  -> binM "/" x y
      ModF x y  -> call2M "mod" x y
      PowF x y  -> call2M "pow" x y
      AbsF x    -> call1M "abs" x
      NegF x    -> go x >>= \a -> pure ("(-" ++ a ++ ")")

      -- Rounding
      RoundF x  -> go x >>= \a -> pure ("int(round(" ++ a ++ "))")
      FloorF x  -> go x >>= \a -> pure ("int(floor(" ++ a ++ "))")
      CeilingF x -> go x >>= \a -> pure ("int(ceil("  ++ a ++ "))")

      -- Real transcendentals
      ExpF  x -> call1M "exp"  x
      LogF  x -> call1M "log"  x
      SqrtF x -> call1M "sqrt" x
      SinF  x -> call1M "sin"  x
      CosF  x -> call1M "cos"  x
      TanF  x -> call1M "tan"  x
      SinhF x -> call1M "sinh" x
      CoshF x -> call1M "cosh" x
      TanhF x -> call1M "tanh" x
      ArcsinF x   -> call1M "asin"  x
      ArccosF x   -> call1M "acos"  x
      ArctanF x   -> call1M "atan"  x
      Arctan2F y x -> do { a <- go y; b <- go x; pure ("atan(" ++ a ++ ", " ++ b ++ ")") }
      ArcsinhF x  -> call1M "asinh" x
      ArccoshF x  -> call1M "acosh" x
      ArctanhF x  -> call1M "atanh" x

      -- ---------------------------------------------------------------
      -- Complex arithmetic
      -- ---------------------------------------------------------------
      AddC x y -> do { a <- go x; b <- go y; pure (crAdd cr a b) }
      SubC x y -> do { a <- go x; b <- go y; pure (crSub cr a b) }
      MulC x y -> do { a <- go x; b <- go y; pure (crMul cr a b) }
      DivC x y -> do { a <- go x; b <- go y; pure (crDiv cr a b) }
      NegC x   -> go x >>= \a -> pure (crNeg cr a)
      PowC x y -> do -- complex^complex: exp(log(base) * exponent)
        a <- go x; b <- go y
        pure ("_cExp(_cMul(_cLog(" ++ a ++ "), " ++ b ++ "))")

      -- Complex analysis functions
      AbsC x  -> go x >>= \a -> pure ("length(" ++ a ++ ")")
      ArgC x  -> go x >>= \a -> pure ("atan(" ++ a ++ ".y, " ++ a ++ ".x)")
      ReC  x  -> go x >>= \a -> pure (a ++ ".x")
      ImC  x  -> go x >>= \a -> pure (a ++ ".y")
      ConjC x -> go x >>= \a -> pure (crConj cr a)
      ExpC  x -> go x >>= \a -> pure ("_cExp(" ++ a ++ ")")
      LogC  x -> go x >>= \a -> pure ("_cLog(" ++ a ++ ")")
      SqrtC x -> go x >>= \a -> pure ("_sqrt(" ++ a ++ ")")

      -- Complex trig: implement via real arithmetic
      SinC x -> do
        a <- go x
        tmp <- freshTmp
        tell [ crTypeName cr ++ " " ++ tmp ++ " = " ++ a ++ ";" ]
        pure ("vec2(sin(" ++ tmp ++ ".x)*cosh(" ++ tmp ++ ".y), cos(" ++ tmp ++ ".x)*sinh(" ++ tmp ++ ".y))")
      CosC x -> do
        a <- go x
        tmp <- freshTmp
        tell [ crTypeName cr ++ " " ++ tmp ++ " = " ++ a ++ ";" ]
        pure ("vec2(cos(" ++ tmp ++ ".x)*cosh(" ++ tmp ++ ".y), -sin(" ++ tmp ++ ".x)*sinh(" ++ tmp ++ ".y))")
      SinhC x -> do
        a <- go x
        tmp <- freshTmp
        tell [ crTypeName cr ++ " " ++ tmp ++ " = " ++ a ++ ";" ]
        pure ("vec2(sinh(" ++ tmp ++ ".x)*cos(" ++ tmp ++ ".y), cosh(" ++ tmp ++ ".x)*sin(" ++ tmp ++ ".y))")
      CoshC x -> do
        a <- go x
        tmp <- freshTmp
        tell [ crTypeName cr ++ " " ++ tmp ++ " = " ++ a ++ ";" ]
        pure ("vec2(cosh(" ++ tmp ++ ".x)*cos(" ++ tmp ++ ".y), sinh(" ++ tmp ++ ".x)*sin(" ++ tmp ++ ".y))")
      TanC x -> do
        a <- go x
        tmp <- freshTmp
        tell [ crTypeName cr ++ " " ++ tmp ++ " = " ++ a ++ ";" ]
        let s = "vec2(sin(" ++ tmp ++ ".x)*cosh(" ++ tmp ++ ".y), cos(" ++ tmp ++ ".x)*sinh(" ++ tmp ++ ".y))"
            c = "vec2(cos(" ++ tmp ++ ".x)*cosh(" ++ tmp ++ ".y), -sin(" ++ tmp ++ ".x)*sinh(" ++ tmp ++ ".y))"
        pure (crDiv cr s c)
      TanhC x -> do
        a <- go x
        tmp <- freshTmp
        tell [ crTypeName cr ++ " " ++ tmp ++ " = " ++ a ++ ";" ]
        let s = "vec2(sinh(" ++ tmp ++ ".x)*cos(" ++ tmp ++ ".y), cosh(" ++ tmp ++ ".x)*sin(" ++ tmp ++ ".y))"
            c = "vec2(cosh(" ++ tmp ++ ".x)*cos(" ++ tmp ++ ".y), sinh(" ++ tmp ++ ".x)*sin(" ++ tmp ++ ".y))"
        pure (crDiv cr s c)

      -- ---------------------------------------------------------------
      -- Integer arithmetic
      -- ---------------------------------------------------------------
      AddI x y -> binM "+" x y
      SubI x y -> binM "-" x y
      MulI x y -> binM "*" x y
      DivI x y -> binM "/" x y
      ModI x y -> do { a <- go x; b <- go y; pure ("(" ++ a ++ " % " ++ b ++ ")") }
      PowI x y -> do
        a <- go x; b <- go y
        pure ("int(pow(float(" ++ a ++ "), float(" ++ b ++ ")))")
      AbsI x   -> call1M "abs" x
      NegI x   -> go x >>= \a -> pure ("(-" ++ a ++ ")")

      -- ---------------------------------------------------------------
      -- Type conversions
      -- ---------------------------------------------------------------
      I2R x  -> go x >>= \a -> pure ("float(" ++ a ++ ")")
      R2C x  -> go x >>= \a -> pure (crFromReal cr a)
      C2R2 x -> go x >>= \a -> pure ("vec2(" ++ a ++ ".x, " ++ a ++ ".y)")
      ToText _ _ -> error "GLSL backend: ToText not supported"

      -- ---------------------------------------------------------------
      -- Boolean operations
      -- ---------------------------------------------------------------
      Or  x y -> binM "||" x y
      And x y -> binM "&&" x y
      Not x   -> go x >>= \a -> pure ("(!" ++ a ++ ")")

      -- Comparisons
      Eql _ x y -> binM "==" x y
      NEq _ x y -> binM "!=" x y
      LTI x y   -> binM "<"  x y
      LTF x y   -> binM "<"  x y

      -- If-then-else expression (ternary)
      ITE _ cond yes no -> do
        c <- go cond; t <- go yes; f <- go no
        pure ("(" ++ c ++ " ? " ++ t ++ " : " ++ f ++ ")")

      -- ---------------------------------------------------------------
      -- Color operations
      -- ---------------------------------------------------------------
      RGB r g b -> do
        rs <- go r; gs <- go g; bs <- go b
        pure ("vec4(" ++ rs ++ ", " ++ gs ++ ", " ++ bs ++ ", 1.0)")

      -- mixColors pct c1 c2 = pct*c1 + (1-pct)*c2
      -- GLSL mix(x,y,a) = x*(1-a) + y*a, so mix(c2, c1, t)
      Blend t c1 c2 -> do
        ts <- go t; c1s <- go c1; c2s <- go c2
        pure ("mix(" ++ c2s ++ ", " ++ c1s ++ ", " ++ ts ++ ")")
      InvertRGB c -> go c >>= \cs ->
        pure ("vec4(1.0 - " ++ cs ++ ".rgb, 1.0)")

      -- ---------------------------------------------------------------
      -- Pairs: GLSL has no tuples, but the color-wheel desugaring only
      -- builds a pair to project it straight back out (the pair never
      -- escapes as a value), so we push the projection down to the PairV
      -- leaves rather than materialising the pair.  A bare PairV reaching
      -- output would mean a pair was used as a real value — unsupported.
      -- ---------------------------------------------------------------
      PairV _ _ _    -> error "GLSL backend: pair value cannot be rendered"
      ProjV1 _ p     -> goProj [Fst] p
      ProjV2 _ p     -> goProj [Snd] p

      -- ---------------------------------------------------------------
      -- List are not supported
      -- ---------------------------------------------------------------
      List _ _       -> error "GLSL backend: lists not supported"
      ConcatText _   -> error "GLSL backend: ConcatText not supported"
      Join {}        -> error "GLSL backend: Join not supported"
      Remove {}      -> error "GLSL backend: Remove not supported"
      Find {}        -> error "GLSL backend: Find not supported"
      Transform {}   -> error "GLSL backend: Transform not supported"
      Range {}       -> error "GLSL backend: Range not supported"
      Length {}      -> error "GLSL backend: Length not supported"
      Index {}       -> error "GLSL backend: Index not supported"

    -- Push pair-projection selectors down through ITE branches and nested
    -- projections until they reach a PairV leaf, then select the component
    -- and resume normal codegen.  'sels' is outermost-first.
    goProj :: forall e' t'. [Sel] -> Value '(e', t') -> GlslM String
    goProj sels v = case v of
      PairV _ a b -> case sels of
        (Fst : rest) -> goProj rest a
        (Snd : rest) -> goProj rest b
        []           -> error "GLSL backend: un-projected pair value"
      ProjV1 _ inner -> goProj (Fst : sels) inner
      ProjV2 _ inner -> goProj (Snd : sels) inner
      ITE _ c x y -> do
        cs <- go c
        ts <- goProj sels x
        fs <- goProj sels y
        pure ("(" ++ cs ++ " ? " ++ ts ++ " : " ++ fs ++ ")")
      other -> case sels of
        []     -> go other
        (_:_)  -> error "GLSL backend: projection applied to a non-pair value"

    -- helpers — explicit signatures make these polymorphic in the env/type indices,
    -- which is necessary when used inside GADT case branches where the env is an
    -- existential that can't unify with the outer rigid type variables.
    binM  :: String -> Value '(e1, s1) -> Value '(e2, s2) -> GlslM String
    binM  op x y = do { a <- go x; b <- go y; pure ("(" ++ a ++ " " ++ op ++ " " ++ b ++ ")") }
    call1M :: String -> Value '(e1, s1) -> GlslM String
    call1M f x   = go x >>= \a -> pure (f ++ "(" ++ a ++ ")")
    call2M :: String -> Value '(e1, s1) -> Value '(e2, s2) -> GlslM String
    call2M f x y = do { a <- go x; b <- go y; pure (f ++ "(" ++ a ++ ", " ++ b ++ ")") }
