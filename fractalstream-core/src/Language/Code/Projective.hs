-- | Points of ℙ¹ and Ĉ (the Riemann sphere), made into pairs of complex
-- numbers, their homogeneous coordinates, before elaboration. Only typing
-- ("Language.Value.Typing") knows the two types: everything after this
-- sees a point @[a : b]@ as the pair @(a, b)@, so no backend has to know
-- about them.
--
-- __Arithmetic in Ĉ__ is done in homogeneous coordinates, with the powers
-- of each point's second coordinate kept track of while an expression is
-- put together, so that the result is the expression's homogeneous form,
-- as one would write it by hand. With @w = [a : b]@,
--
-- > w - (w^3 - 1) / (3 w^2)     is     [2 a^3 + b^3 : 3 a^2 b]
--
-- which is right at @w = ∞ = [1 : 0]@ as well, where working out each
-- operation's value in turn would meet @∞ / ∞@. (A variable's value is a
-- point, so this is within one expression: across statements, each value
-- is the point it was worked out to be.) The result of each assignment is
-- scaled so that its larger coordinate is 1, so that iterating a map
-- never overflows.
--
-- __Homogeneous coordinates__, @[a : b] := p@, are only defined up to
-- scale, so @a@ and @b@ can only be used in ways that don't depend on it:
-- in a point @[F : G]@ with @F@ and @G@ homogeneous of the same degree,
-- or in a ratio of the same degree, like @a / b@. Anything else is an
-- error (pointing to @affine(1, p)@).
--
-- Comparisons use the chordal distance, the straight-line distance
-- between the points of the unit sphere: @p = q@ when it's less than the
-- vanishing radius. @p escapes@ when @|a| > R |b|@ (as @|a / b| > R@, but
-- right at ∞), and @p vanishes@ when @|a| < r |b|@.
module Language.Code.Projective
  ( expandProjective
  , lowerValueAt
  , showPoint
  , showSurfaceValue
  , showFieldValue
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Type
import Language.Typecheck
import Language.Parser.SourceRange
import Language.Value.Syntax
import Language.Value.Builtins (internalEscapeRadius, internalVanishingRadius)
import Language.Value.Typing (TypeEnv(..), synth, check, strictTypeEnv, isProjective)
import Language.Code.Syntax
import Language.Code.Typecheck.Functions (FunDef(..), Funs, expandCalls)

-- | Make every point of ℙ¹ or Ĉ in the code a pair of complex numbers.
-- Code without any is left exactly as it is.
expandProjective :: TypeEnv -> Stmt -> TC Stmt
expandProjective tenv s
  | not (any hasProjective (tyVars tenv)) && not (mentionsSphere s) = pure s
  | otherwise = evalStateT (stmtIn (Scope (tyVars tenv) Map.empty) s) (PS 0 [] Map.empty)
  where
    funs = allFunctions s

    ------------------------------------------------------
    -- Statements
    ------------------------------------------------------

    stmtIn :: Scope -> Stmt -> M Stmt
    stmtIn scope st@(Stmt sr node) = case node of
      SBlock body -> Stmt sr . SBlock <$> blockIn scope body
      SDecl{} -> one
      SDeclHomogeneous{} -> one
      -- (a one-line function is expanded where it's called)
      SFun{} -> pure st
      SSet n e -> do
        when (Map.member n (scCoords scope)) $
          lift (advise sr ("`" ++ n ++ "` is one of a point's homogeneous coordinates, which can't be \
                           \changed: make a new point, `[F : G]`, instead."))
        Stmt sr . SSet n <$> valueAt scope (Map.lookup n (scTypes scope)) e
      SIterate p@(PVar _ n) e isWhile c lim -> do
        let t = Map.lookup n (scTypes scope)
        e' <- valueAt scope t e
        c' <- valueAt scope Nothing c
        lim' <- traverse (valueAt scope Nothing) lim
        pure (Stmt sr (SIterate p e' isWhile c' lim'))
      SIf c yes no -> Stmt sr <$> (SIf <$> valueAt scope Nothing c <*> stmtIn scope yes <*> stmtIn scope no)
      SLoop kind c lim body ->
        Stmt sr <$> (SLoop kind <$> valueAt scope Nothing c <*> traverse (valueAt scope Nothing) lim
                                <*> stmtIn scope body)
      SForEach item list body -> do
        let scope' = withItem item list scope
        Stmt sr . SForEach item list <$> stmtIn scope' body
      SWithFirst item p list body fallback -> do
        let scope' = withItem item list scope
        Stmt sr <$> (SWithFirst item <$> valueAt scope' Nothing p <*> pure list
                                     <*> stmtIn scope' body <*> traverse (stmtIn scope) fallback)
      -- (a point of the sphere is drawn where it is in the plane: on a
      -- sphere viewer, that's where it is on the globe)
      SDraw d -> Stmt sr . SDraw <$> case d of
        DrawPointAt p -> DrawPointAt <$> drawnAt scope p
        DrawCircleAt filled c r unit -> DrawCircleAt filled <$> drawnAt scope c <*> valueAt scope Nothing r <*> pure unit
        DrawGeodesicFrom p q -> DrawGeodesicFrom <$> drawnAt scope p <*> drawnAt scope q
        DrawRectFrom filled p q -> DrawRectFrom filled <$> drawnAt scope p <*> drawnAt scope q
        DrawLineFrom p q -> DrawLineFrom <$> drawnAt scope p <*> drawnAt scope q
        WriteAt t p -> WriteAt <$> valueAt scope Nothing t <*> drawnAt scope p
        UseStroke c -> UseStroke <$> valueAt scope Nothing c
        UseFill c -> UseFill <$> valueAt scope Nothing c
        EraseAll -> pure EraseAll
      _ -> mapStmtShallow (valueAt scope Nothing) st
      where one = Stmt sr . SBlock <$> blockIn scope [st]

    -- A declaration scopes over the rest of its block, so the statements
    -- it's made into go in the same block.
    blockIn :: Scope -> [Stmt] -> M [Stmt]
    blockIn scope = \case
      [] -> pure []
      Stmt sr (SDecl n mty e) : rest -> do
        -- (a type that couldn't be inferred is left for elaboration to
        -- report, unless it's one of the sphere's)
        mt <- case mty of
          Just t -> pure (Just t)
          Nothing -> lift ((Just <$> typeOf scope e) `catchError` \err ->
                             if involves scope e then throwError err else pure Nothing)
        case mt of
          Nothing -> (Stmt sr (SDecl n Nothing e) :) <$> blockIn scope rest
          Just t -> do
            e' <- valueAt scope (Just t) e
            let ty' = if isNothing mty && not (hasProjective t) then Nothing else Just (lowerType t)
            (Stmt sr (SDecl n ty' e') :) <$> blockIn (declare n t scope) rest
      Stmt sr (SDeclHomogeneous ns e) : rest -> do
        (a, b) <- case ns of
          [a, b] -> pure (a, b)
          _ -> lift (advise sr "A point of ℙ¹ has two homogeneous coordinates, `[a : b] := p`: \
                               \the higher-dimensional projective spaces aren't there yet.")
        e1 <- lift (expandCalls funs e)
        lift (homogeneityCheck scope e1)
        te <- lift (synth (typeEnv scope) e1)
        unless (isProjective (exprType te)) $
          lift (advise (synRange e) ("`[" ++ a ++ " : " ++ b ++ "] := p` takes apart a point of ℙ¹ \
                                     \or Ĉ, but this is " ++ describeType (exprType te) ++ "."))
        p <- materialize te
        k <- freshName "coordinates"
        let src = Syn sr (EVar k)
            decls = [ Stmt sr (SDecl k (Just pairT) p)
                    , Stmt sr (SDecl a (Just ComplexT) (Syn sr (EProj False src)))
                    , Stmt sr (SDecl b (Just ComplexT) (Syn sr (EProj True src))) ]
            scope' = (declare a ComplexT . declare b ComplexT) scope
                       { scCoords = Map.insert a k (Map.insert b k (scCoords scope)) }
        (decls ++) <$> blockIn scope' rest
      st : rest -> (:) <$> stmtIn scope st <*> blockIn scope rest

    -- a point to draw at: a point of the sphere, where it is in the plane
    drawnAt :: Scope -> Expr -> M Expr
    drawnAt scope p = do
      p1 <- lift (expandCalls funs p)
      isPoint <- if involves scope p1
        then lift ((isProjective . exprType <$> synth (typeEnv scope) p1) `catchError` \_ -> pure False)
        else pure False
      valueAt scope Nothing (if isPoint then Syn (synRange p) (EAffine (Syn (synRange p) (ELit "1" (SomeHaskellType IntegerType 1))) p) else p)

    withItem item list scope = case Map.lookup list (scTypes scope) of
      Just (ListT t) -> declare item t scope
      _ -> scope

    ------------------------------------------------------
    -- Values
    ------------------------------------------------------

    -- An expression, given to something of the given type (or, for
    -- 'Nothing', to something that isn't a point of the sphere).
    valueAt :: Scope -> Maybe FSType -> Expr -> M Expr
    valueAt scope mty e = do
      e1 <- lift (expandCalls funs e)
      lift (homogeneityCheck scope e1)
      let target = fromMaybe BooleanT mty
      if not (involves scope e1 || hasProjective target) then pure e else do
        te <- lift $ case mty of
          Just t -> check (typeEnv scope) t e1
          Nothing -> synth (typeEnv scope) e1
        when (isNothing mty && hasProjective (exprType te)) $
          lift (advise (synRange e) "This is a point of the Riemann sphere, which isn't a number: \
                                    \for the complex number it is in the plane, use `affine(1, ...)`.")
        if isProjective target
          then do
            p <- materialize te
            -- (a value worked out by arithmetic is scaled, so that
            -- iterating a map doesn't overflow)
            if isAtomic te then pure p else normalized (synRange e) p
          else lowerAs (fromMaybe (exprType te) mty) te

    -- A typed expression, as one of the given type, with every point of
    -- the sphere in it a pair.
    lowerAs :: FSType -> TExpr -> M Expr
    lowerAs target te
      | isProjective target = materialize te
      | otherwise = structural target te

    -- (the node itself, put back together, for a type that isn't a point
    -- of the sphere, or for a node of one that isn't arithmetic: an item
    -- of a list of points, and the like)
    structural :: FSType -> TExpr -> M Expr
    structural target (Syn (sr, own) node) = case node of
          -- (the chart where the point is nearest the origin)
          EChart p -> do
            p' <- pointOf p
            withPoint sr p' $ \a b ->
              let absE x = Syn sr (EUn OpAbs x)
              in Syn sr (EITE (Syn sr (EBin OpLe (absE a) (absE b)))
                              (Syn sr (EPair (Syn sr (EBin OpDiv a b)) (intE sr 1)))
                              (Syn sr (EPair (Syn sr (EBin OpDiv b a)) (intE sr 0))))
          EAffine k p -> do
            k' <- lowerAs IntegerT k
            p' <- pointOf p
            withPoint sr p' $ \a b -> case constantInteger k of
              -- (a chart given by its number is picked here)
              Just 0 -> Syn sr (EBin OpDiv b a)
              Just _ -> Syn sr (EBin OpDiv a b)
              Nothing -> Syn sr (EITE (Syn sr (EBin OpApproxEq k' (intE sr 0)))
                                      (Syn sr (EBin OpDiv b a)) (Syn sr (EBin OpDiv a b)))
          EChordal p q -> do
            p' <- pointOf p
            q' <- pointOf q
            withPoints sr p' q' (chordal sr)
          EBin op x y
            | op `elem` [OpApproxEq, OpApproxNEq, OpExactEq]
            , any (isProjective . exprType) [x, y] -> do
                p <- pointOf x
                q <- pointOf y
                withPoints sr p q $ \a b c d ->
                  let near = Syn sr (EBin OpLt (chordal sr a b c d) (Syn sr (EVar internalVanishingRadius)))
                  in case op of
                    OpApproxEq -> near
                    OpApproxNEq -> Syn sr (EUn OpNot near)
                    _ -> Syn sr (EBin OpExactEq (Syn sr (EBin OpSub (mul sr a d) (mul sr b c))) (cnum sr 0))
          EUn op x
            | op `elem` [OpEscapes, OpVanishes], isProjective (exprType x) -> do
                p <- pointOf x
                withPoint sr p $ \a b ->
                  let absE v = Syn sr (EUn OpAbs v)
                      radius n = Syn sr (EVar n)
                  in if op == OpEscapes
                       then Syn sr (EBin OpLt (mul sr (radius internalEscapeRadius) (absE b)) (absE a))
                       else Syn sr (EBin OpLt (absE a) (mul sr (radius internalVanishingRadius) (absE b)))
          EPair x y | Pair t1 t2 <- target -> fmap (Syn sr) (EPair <$> lowerAs t1 x <*> lowerAs t2 y)
          EList xs | ListT t <- target -> Syn sr . EList <$> traverse (lowerAs t) xs
          EITE c yes no -> fmap (Syn sr) (EITE <$> lowerAs BooleanT c <*> lowerAs target yes <*> lowerAs target no)
          ELet n v body -> fmap (Syn sr) (ELet n <$> lowerAs (exprType v) v <*> lowerAs target body)
          EAppend lst x | ListT t <- target -> fmap (Syn sr) (EAppend <$> lowerAs target lst <*> lowerAs t x)
          EPrepend lst x | ListT t <- target -> fmap (Syn sr) (EPrepend <$> lowerAs target lst <*> lowerAs t x)
          EFind lst n p d -> fmap (Syn sr) (EFind <$> lowerAs (ListT target) lst <*> pure n
                                                  <*> lowerAs BooleanT p <*> lowerAs target d)
          ECast x t -> fmap (Syn sr) (ECast <$> lowerAs t x <*> pure (lowerType t))
          EWiden t x -> fmap (Syn sr) (EWiden (lowerType t) <$> lowerAs (exprType x) x)
          _ | hasProjective target, not (hasProjective own) ->
                lift (advise sr ("This is " ++ describeType own ++ ", where "
                                 ++ describeType target ++ " is needed."))
            | otherwise -> Syn sr <$> traverse (\x -> lowerAs (exprType x) x) node

    -- A point of the sphere (or a number, as one), as a pair.
    pointOf :: TExpr -> M Expr
    pointOf = materialize

    ------------------------------------------------------
    -- Homogeneous forms
    ------------------------------------------------------

    -- The point an expression of ℙ¹ or Ĉ (or a number, as one) is, as a
    -- pair, with whatever the expression needs bound around it.
    materialize :: TExpr -> M Expr
    materialize te = do
      outer <- gets psBinds
      modify (\st -> st { psBinds = [] })
      h <- homog Map.empty te
      p <- finalize (synRange te) h
      bs <- gets psBinds
      modify (\st -> st { psBinds = outer })
      let sr = synRange te
      pure (foldl' (\body (n, v) -> Syn sr (ELet n v body)) p bs)

    homog :: Map String H -> TExpr -> M H
    homog lets te@(Syn (sr, t) node)
      -- (a number, made complex, so that the point is a pair of them)
      | not (isProjective t) = numberH . asComplex <$> lowerAs t te
      | otherwise = case node of
          EVar n | Just h <- Map.lookup n lets -> pure h
                 | otherwise -> source sr (Syn sr (EVar n))
          EInfinity -> do
            modify (\st -> st { psB = Map.insert infinityKey (cnum sr 0) (psB st) })
            pure (H (cnum sr 1) (cnum sr 1) (Map.singleton infinityKey (-1)))
          ELit{} -> numberH . asComplex <$> lowerAs ComplexT te
          ECast x _ -> homog lets x
          EWiden _ x -> homog lets x
          EUn OpNeg x -> (\h -> h { hNum = Syn sr (EUn OpNeg (hNum h)) }) <$> homog lets x
          EBin OpAdd x y -> addH sr False <$> homog lets x <*> homog lets y <*> gets psB
          EBin OpSub x y -> addH sr True <$> homog lets x <*> homog lets y <*> gets psB
          EBin OpMul x y -> mulH sr <$> homog lets x <*> homog lets y
          EBin OpDiv x y -> divH sr <$> homog lets x <*> homog lets y
          EBin OpPow x y -> case constantInteger (stripTypes y) of
            Just k -> powH sr k <$> homog lets x
            Nothing -> lift (advise (synRange y) "A power of a point of Ĉ needs a whole number for \
                                                 \its exponent, written as a number.")
          ELet n v body
            | isProjective (exprType v) -> do
                hv <- homog lets v
                xn <- freshName "numerator"
                xd <- freshName "denominator"
                bind xn (hNum hv)
                bind xd (hDen hv)
                homog (Map.insert n hv { hNum = Syn sr (EVar xn), hDen = Syn sr (EVar xd) } lets) body
            | otherwise -> do
                v' <- lowerAs (exprType v) v
                bind n v'
                homog lets body
          EHomogeneous [a, b] -> do
            p <- Syn sr <$> (EPair <$> lowerAs ComplexT a <*> lowerAs ComplexT b)
            source sr p
          EITE c yes no -> do
            p <- Syn sr <$> (EITE <$> lowerAs BooleanT c <*> materialize yes <*> materialize no)
            source sr p
          -- (an item of a list of points, and the like: a pair, once
          -- what it's made from is)
          _ -> structural t te >>= source sr

    -- A point given as a pair: its own variable's coordinates, @[a : b]@,
    -- as @a / b@, with the power of @b@ kept track of.
    source :: SourceRange -> Expr -> M H
    source sr p = do
      k <- case p of
        Syn _ (EVar n) -> pure n
        _ -> do
          k <- freshName "point"
          bind k p
          pure k
      let v = Syn sr (EVar k)
      modify (\st -> st { psB = Map.insert k (Syn sr (EProj True v)) (psB st) })
      pure (H (Syn sr (EProj False v)) (cnum sr 1) (Map.singleton k (-1)))

    finalize :: SourceRange -> H -> M Expr
    finalize sr (H num den pows) = do
      bOf <- gets psB
      let factor sign = foldl' (mul sr) (cnum sr 1)
            [ powE sr b (abs e) | (k, e) <- Map.toList pows, signum e == sign
                                , let b = Map.findWithDefault (cnum sr 1) k bOf ]
      pure (Syn sr (EPair (complexE (mul sr num (factor 1))) (complexE (mul sr den (factor (-1))))))

    -- (each term times the powers of the second coordinates it has more
    -- of than the other, so that the sum has the fewest)
    addH sr isSub (H n1 d1 e1) (H n2 d2 e2) bOf =
      let keys = Map.keysSet e1 <> Map.keysSet e2
          at k m = Map.findWithDefault 0 k m
          low = Map.fromSet (\k -> min (at k e1) (at k e2)) keys
          extra e = [ powE sr (Map.findWithDefault (cnum sr 1) k bOf) x
                    | (k, m) <- Map.toList low, let x = at k e - m, x /= 0 ]
          t1 = foldl' (mul sr) (mul sr n1 d2) (extra e1)
          t2 = foldl' (mul sr) (mul sr n2 d1) (extra e2)
      in H (Syn sr (EBin (if isSub then OpSub else OpAdd) t1 t2)) (mul sr d1 d2) (Map.filter (/= 0) low)
    mulH sr (H n1 d1 e1) (H n2 d2 e2) =
      H (mul sr n1 n2) (mul sr d1 d2) (Map.filter (/= 0) (Map.unionWith (+) e1 e2))
    divH sr (H n1 d1 e1) (H n2 d2 e2) =
      H (mul sr n1 d2) (mul sr d1 n2) (Map.filter (/= 0) (Map.unionWith (+) e1 (negate <$> e2)))
    powH :: SourceRange -> Integer -> H -> H
    powH sr k (H n d e)
      | k == 0 = H (cnum sr 1) (cnum sr 1) Map.empty
      | k < 0 = powH sr (negate k) (H d n (negate <$> e))
      | otherwise = H (powE sr n (fromIntegral k)) (powE sr d (fromIntegral k)) ((* fromIntegral k) <$> e)

    ------------------------------------------------------
    -- Types and scope
    ------------------------------------------------------

    typeEnv scope = strictTypeEnv (scTypes scope)

    typeOf scope e = do
      e1 <- expandCalls funs e
      exprType <$> synth (typeEnv scope) e1

    -- Does the expression have anything of the sphere in it?
    involves scope e = mentionsSphereE e || any (\n -> maybe False hasProjective (Map.lookup n (scTypes scope)))
                                                (Set.toList (exprNames e))

    -- A's coordinates can only be used in ways that don't depend on
    -- their scale.
    homogeneityCheck :: Scope -> Expr -> TC ()
    homogeneityCheck scope e
      | Map.null (scCoords scope) || Set.null (exprNames e `Set.intersection` Map.keysSet (scCoords scope)) = pure ()
      | otherwise = case degree scope Map.empty e of
          Left (sr, msg) -> advise sr msg
          Right d | Map.null d -> pure ()
                  | otherwise -> advise (synRange e) (notHomogeneous scope (Map.keys d))

    ------------------------------------------------------
    -- Helpers
    ------------------------------------------------------

    -- @p@'s coordinates, given to @f@: those of its variable, or of a
    -- local holding it.
    withPoint :: SourceRange -> Expr -> (Expr -> Expr -> Expr) -> M Expr
    withPoint sr p f = case p of
      Syn _ (EVar _) -> pure (f (Syn sr (EProj False p)) (Syn sr (EProj True p)))
      Syn _ (EPair a b) | simple a, simple b -> pure (f a b)
      _ -> do
        k <- freshName "point"
        let v = Syn sr (EVar k)
        pure (Syn sr (ELet k p (f (Syn sr (EProj False v)) (Syn sr (EProj True v)))))
      where simple = \case { Syn _ (EVar _) -> True; Syn _ ELit{} -> True; _ -> False }

    withPoints :: SourceRange -> Expr -> Expr -> (Expr -> Expr -> Expr -> Expr -> Expr) -> M Expr
    withPoints sr p q f = do
      kp <- freshName "point"
      kq <- freshName "point"
      let vp = Syn sr (EVar kp)
          vq = Syn sr (EVar kq)
          proj b v = Syn sr (EProj b v)
      pure (Syn sr (ELet kp p (Syn sr (ELet kq q (f (proj False vp) (proj True vp) (proj False vq) (proj True vq))))))

    bind :: String -> Expr -> M ()
    bind n v = modify (\st -> st { psBinds = (n, v) : psBinds st })

    freshName :: String -> M String
    freshName what = do
      k <- gets psCounter
      modify (\st -> st { psCounter = k + 1 })
      pure ("[internal] sphere." ++ what ++ " #" ++ show k)

    -- @p@ scaled so that its larger coordinate is 1
    normalized :: SourceRange -> Expr -> M Expr
    normalized sr p = do
      k <- freshName "scaled"
      let v = Syn sr (EVar k)
          a = Syn sr (EProj False v)
          b = Syn sr (EProj True v)
          absE x = Syn sr (EUn OpAbs x)
          bigger = Syn sr (EBin OpGt (absE a) (absE b))
      pure (Syn sr (ELet k p (Syn sr (EITE bigger (Syn sr (EPair (cnum sr 1) (Syn sr (EBin OpDiv b a))))
                                                   (Syn sr (EPair (Syn sr (EBin OpDiv a b)) (cnum sr 1)))))))

-- | An expression, given to something of a type with points of the
-- sphere in it (a field's value), with them made into pairs.
lowerValueAt :: TypeEnv -> FSType -> Expr -> TC Expr
lowerValueAt tenv ty e = do
  let n = "[internal] sphere.value"
  Stmt _ out <- expandProjective tenv (Stmt (synRange e) (SBlock [Stmt (synRange e) (SDecl n (Just ty) e)]))
  case out of
    SBlock [Stmt _ (SDecl _ _ e')] -> pure e'
    _ -> throwError (Internal (Advice (synRange e) "lowerValueAt: unexpected expansion"))

-- | A point of Ĉ (or, with 'True', of ℙ¹), by its homogeneous
-- coordinates, as it's written: @∞@, a complex number, or @[a : b]@.
showPoint :: Bool -> (Complex Double -> String) -> (Complex Double, Complex Double) -> String
showPoint isProjectiveLine showC (a, b)
  | isProjectiveLine = "[" ++ showC a ++ " : " ++ showC b ++ "]"
  | b == 0 && a /= 0 = "∞"
  | otherwise = showC (a / b)

-- | A value, with points of the sphere in it written as they are in a
-- script (see 'showPoint'), given its type as the script has it: the rest
-- as 'toUserString' writes it.
showSurfaceValue :: FSType -> TypeProxy t -> HaskellType t -> String
showSurfaceValue surf ty v = case (surf, ty) of
  (_, PairType ComplexType ComplexType)
    | isProjective surf -> showPoint (surf == ProjectiveT) (toUserString ComplexType) v
  (Pair s1 s2, PairType t1 t2) -> "(" ++ showSurfaceValue s1 t1 (fst v) ++ ", " ++ showSurfaceValue s2 t2 (snd v) ++ ")"
  (ListT s, ListType t) -> "[" ++ intercalate ", " (map (showSurfaceValue s t) v) ++ "]"
  _ -> toUserString ty v

-- | A field's value as its text box shows it (a list without its
-- brackets, as 'toUserString' writes one).
showFieldValue :: Maybe FSType -> TypeProxy t -> HaskellType t -> String
showFieldValue msurf ty v = case msurf of
  Just surf@(ListT _) -> let s = showSurfaceValue surf ty v in reverse (drop 1 (reverse (drop 1 s)))
  Just surf -> showSurfaceValue surf ty v
  Nothing -> toUserString ty v

-- | What the homogeneous form of an expression is put together from: the
-- value is @num / den@ times each point's second coordinate to the given
-- power (by the name of the point's variable).
data H = H { hNum :: Expr, hDen :: Expr, _hPow :: Map String Int }

asComplex :: Expr -> Expr
asComplex e = Syn (synRange e) (ECast e ComplexT)

-- | A coordinate, as a complex number (unless it's written as one).
complexE :: Expr -> Expr
complexE e = case e of
  Syn _ (ELit _ (SomeHaskellType ComplexType _)) -> e
  Syn _ (ECast _ ComplexT) -> e
  _ -> asComplex e

numberH :: Expr -> H
numberH e = H e (cnum (synRange e) 1) Map.empty

infinityKey :: String
infinityKey = "[internal] sphere.infinity"

data Scope = Scope
  { scTypes :: Map String FSType
  , scCoords :: Map String String
    -- ^ each homogeneous coordinate, and the point it's one of
  }

declare :: String -> FSType -> Scope -> Scope
declare n t scope = scope { scTypes = Map.insert n t (scTypes scope) }

data PS = PS
  { psCounter :: Int
  , psBinds :: [(String, Expr)]
    -- ^ the local bindings an expression's homogeneous form needs, the
    -- latest first
  , psB :: Map String Expr
    -- ^ each point's second coordinate, by its variable
  }

type M = StateT PS TC

pairT :: FSType
pairT = Pair ComplexT ComplexT

-- | A type with every point of the sphere in it a pair.
lowerType :: FSType -> FSType
lowerType = \case
  ProjectiveT -> pairT
  ExtComplexT -> pairT
  Pair a b -> Pair (lowerType a) (lowerType b)
  ListT t -> ListT (lowerType t)
  TableT n t -> TableT n (lowerType t)
  t -> t

hasProjective :: FSType -> Bool
hasProjective = \case
  ProjectiveT -> True
  ExtComplexT -> True
  Pair a b -> hasProjective a || hasProjective b
  ListT t -> hasProjective t
  TableT _ t -> hasProjective t
  _ -> False

describeType :: FSType -> String
describeType = \case
  ProjectiveT -> "a point of ℙ¹"
  ExtComplexT -> "a point of Ĉ"
  IntegerT -> "an integer"
  RealT -> "a real number"
  ComplexT -> "a complex number"
  BooleanT -> "a truth value"
  _ -> "something else"

-- | A value that's already a point, which needn't be scaled again.
isAtomic :: TExpr -> Bool
isAtomic te = case synNode te of
  EVar _ -> True
  EInfinity -> True
  ECast x _ -> isAtomic x
  _ -> False

mul :: SourceRange -> Expr -> Expr -> Expr
mul sr a b
  | isOne a = b
  | isOne b = a
  | otherwise = Syn sr (EBin OpMul a b)

powE :: SourceRange -> Expr -> Int -> Expr
powE sr b k
  | k == 1 = b
  | isOne b = b
  | otherwise = Syn sr (EBin OpPow b (intE sr (fromIntegral k)))

isOne :: Expr -> Bool
isOne = \case
  Syn _ (ELit _ (SomeHaskellType ComplexType (1 :+ 0))) -> True
  e -> integerLiteral e == Just 1

-- | An integer, for an exponent.
intE :: SourceRange -> Integer -> Expr
intE sr n = Syn sr (ELit (show n) (SomeHaskellType IntegerType (fromIntegral n)))

-- | A whole number, as a complex number: a coordinate (as integers, a
-- pair of them wouldn't be a pair of complex numbers everywhere one is
-- needed).
cnum :: SourceRange -> Integer -> Expr
cnum sr n = Syn sr (ELit (show n) (SomeHaskellType ComplexType (fromIntegral n :+ 0)))

-- | The chordal distance between two points (by their coordinates):
-- with @p = [a : b]@ and @q = [c : d]@,
-- @2 |a d - b c| / sqrt((|a|² + |b|²) (|c|² + |d|²))@.
chordal :: SourceRange -> Expr -> Expr -> Expr -> Expr -> Expr
chordal sr a b c d =
  let absE x = Syn sr (EUn OpAbs x)
      sq x = let m = absE x in Syn sr (EBin OpMul m m)
      add x y = Syn sr (EBin OpAdd x y)
      cross = Syn sr (EBin OpSub (mul sr a d) (mul sr b c))
      norms = Syn sr (EApp "sqrt" (Syn sr (EBin OpMul (add (sq a) (sq b)) (add (sq c) (sq d)))))
  in Syn sr (EBin OpDiv (Syn sr (EBin OpMul (intE sr 2) (absE cross))) norms)

------------------------------------------------------
-- Homogeneity
------------------------------------------------------

-- | The degree of an expression in each point's homogeneous coordinates
-- (by the point), or where it isn't homogeneous, and why.
degree :: Scope -> Map String (Map String Int) -> Expr -> Either (SourceRange, String) (Map String Int)
degree scope lets (Syn sr node) = case node of
  EVar n | Just d <- Map.lookup n lets -> pure d
         | Just k <- Map.lookup n (scCoords scope) -> pure (Map.singleton k 1)
         | otherwise -> pure Map.empty
  EBin op x y
    | op `elem` [OpAdd, OpSub] -> do
        dx <- go x
        dy <- go y
        if dx == dy then pure dx else Left (sr, notHomogeneous scope (Map.keys (dx <> dy)))
    | op == OpMul -> Map.filter (/= 0) <$> (Map.unionWith (+) <$> go x <*> go y)
    | op == OpDiv -> Map.filter (/= 0) <$> (Map.unionWith (-) <$> go x <*> go y)
    | op == OpPow -> do
        dx <- go x
        zero y
        if Map.null dx then pure dx else case constantInteger y of
          Just k -> pure ((* fromIntegral k) <$> dx)
          Nothing -> Left (sr, notHomogeneous scope (Map.keys dx))
  EUn OpNeg x -> go x
  ECast x _ -> go x
  EWiden _ x -> go x
  EHomogeneous parts -> do
    ds <- traverse go parts
    case ds of
      d : rest | all (== d) rest -> pure Map.empty
      _ -> Left (sr, "The parts of a point `[F : G]` should be of the same degree in the \
                     \homogeneous coordinates they're made from, so that the point doesn't depend \
                     \on their scale (as `[a^2 + c b^2 : b^2]` doesn't).")
  ELet n v body -> do
    dv <- go v
    degree scope (Map.insert n dv lets) body
  EITE c yes no -> do
    zero c
    dy <- go yes
    dn <- go no
    if dy == dn then pure dy else Left (sr, notHomogeneous scope (Map.keys (dy <> dn)))
  _ -> mapM_ zero node $> Map.empty
  where
    go = degree scope lets
    zero e = do
      d <- go e
      unless (Map.null d) $ Left (synRange e, notHomogeneous scope (Map.keys d))

notHomogeneous :: Scope -> [String] -> String
notHomogeneous scope points =
  let coords = [ c | (c, k) <- Map.toList (scCoords scope), k `elem` points ]
      names = intercalate " and " (map (\c -> "`" ++ c ++ "`") coords)
  in names ++ " are homogeneous coordinates, which are only defined up to scale, so they can \
     \only be used in ways that don't depend on it: in a point `[F : G]` with both parts of the \
     \same degree, or in a ratio of the same degree, like `a / b`. For the complex number a \
     \point is in the plane, use `affine(1, p)`."

-- | The value of an integer expression made of integer constants alone.
constantInteger :: Syn ExprF a -> Maybe Integer
constantInteger e = case synNode e of
  _ | Just n <- integerLiteral e -> Just n
  EUn OpNeg x -> negate <$> constantInteger x
  _ -> Nothing

------------------------------------------------------
-- Finding points of the sphere
------------------------------------------------------

mentionsSphere :: Stmt -> Bool
mentionsSphere (Stmt _ node) = case node of
  SDeclHomogeneous{} -> True
  SDecl _ (Just t) e -> hasProjective t || mentionsSphereE e
  SFun _ _ e -> mentionsSphereE e
  _ -> any mentionsSphereE (shallowExprs node) || any mentionsSphere (subStmts node)

mentionsSphereE :: Expr -> Bool
mentionsSphereE (Syn _ node) = case node of
  EInfinity -> True
  EHomogeneous{} -> True
  EChart{} -> True
  EAffine{} -> True
  EChordal{} -> True
  ECast _ t | hasProjective t -> True
  _ -> any mentionsSphereE node

-- | The expressions of a statement, but not of the statements in it.
shallowExprs :: StmtF -> [Expr]
shallowExprs node = execState (mapStmtShallow (\e -> modify (e :) $> e) (Stmt NoSourceRange node)) []

subStmts :: StmtF -> [Stmt]
subStmts = \case
  SBlock body -> body
  SIf _ yes no -> [yes, no]
  SLoop _ _ _ body -> [body]
  SForEach _ _ body -> [body]
  SWithFirst _ _ _ body fb -> body : maybeToList fb
  SProc _ _ body -> [body]
  SFunction _ _ body -> [body]
  _ -> []

-- | Apply @f@ to each expression of a statement, but not to those of the
-- statements in it.
mapStmtShallow :: Monad m => (Expr -> m Expr) -> Stmt -> m Stmt
mapStmtShallow f (Stmt sr node) = Stmt sr <$> case node of
  SDecl n ty e -> SDecl n ty <$> f e
  SDeclHomogeneous ns e -> SDeclHomogeneous ns <$> f e
  SSet n e -> SSet n <$> f e
  SDeclTuple p e -> SDeclTuple p <$> f e
  SSetTuple p e -> SSetTuple p <$> f e
  SFun g ps e -> SFun g ps <$> f e
  SIf c yes no -> SIf <$> f c <*> pure yes <*> pure no
  SLoop kind c lim body -> SLoop kind <$> f c <*> traverse f lim <*> pure body
  SIterate p e w c lim -> SIterate p <$> f e <*> pure w <*> f c <*> traverse f lim
  SDraw d -> SDraw <$> case d of
    DrawPointAt p -> DrawPointAt <$> f p
    DrawCircleAt filled c r unit -> DrawCircleAt filled <$> f c <*> f r <*> pure unit
    DrawGeodesicFrom p q -> DrawGeodesicFrom <$> f p <*> f q
    DrawRectFrom filled p q -> DrawRectFrom filled <$> f p <*> f q
    DrawLineFrom p q -> DrawLineFrom <$> f p <*> f q
    UseStroke c -> UseStroke <$> f c
    UseFill c -> UseFill <$> f c
    EraseAll -> pure EraseAll
    WriteAt t p -> WriteAt <$> f t <*> f p
  SSetTable n ix v -> SSetTable n <$> f ix <*> f v
  SResize n size fill -> SResize n <$> f size <*> traverse f fill
  SAllocate n size corners fill ->
    SAllocate n <$> f size <*> traverse (\(p, q) -> (,) <$> f p <*> f q) corners <*> traverse f fill
  SWithFirst item p list body fb -> SWithFirst item <$> f p <*> pure list <*> pure body <*> pure fb
  SRun name args -> SRun name <$> traverse f args
  other -> pure other

allFunctions :: Stmt -> Funs
allFunctions s = funs
  where
    funs = Map.fromList [ (f, FunDef ps body funs) | Stmt _ (SFun f ps body) <- everyStmt s ]
    everyStmt st@(Stmt _ node) = st : concatMap everyStmt (subStmts node)
