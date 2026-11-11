{- |
Module      : Language.Legacy.Translate
Description : Turn an original-FractalStream script into a project

The original FractalStream described one dynamical system per script
("Language.Legacy.Syntax"), drawing its dynamical plane, and its
parameter plane too if the script mentions the parameter @c@. This turns
such a script into a project for this FractalStream, written to be easy
to go on editing rather than to reproduce the old pictures exactly:

* @default v to e.@ becomes a field of a @\@config "Parameters"@ tab; @c@
  becomes the field @C@ there when there's a parameter plane.
* The script itself is written once, in @\@common@, as @define
  script(limit)@: each of its loops makes at most @limit@ passes. If it
  has @par@ or @dyn@ commands, it also takes @in_parameter_plane@, and
  those commands check it.
* There's a viewer for each plane: @\@viewer "Parameter plane" var=C@
  (starting from @z := 0@, as the old program did) and
  @\@viewer "Dynamical plane"@, each declaring the variables the script
  sets for coloring and then running @script(max_iters)@ (with @true@ or
  @false@ for which plane it is, if the script needs to know). Picking @C@ in the parameter
  plane, like the old Dynamics tool, is the viewer's own "Select C"
  tool, which every viewer over a configuration variable has.
* Coloring: each flag (@[name]@, @[name | color]@) gets a color in a
  @\@config "Colors"@ tab, and a point is drawn in a dark-to-light
  gradient of its flag's color, by iterations (period 20: the color at
  0, then darker, then lighter), or black if its last loop gave up.
* Each probe becomes a definition in @\@common@, taking the parameter (@C@,
  or a real script's @(a, b)@) when there's a parameter plane, and a tool
  on each viewer running it (with @C@ in the dynamical plane and the
  clicked point in the parameter plane), showing the value it reports in
  a field of the tool's panel.
* The dynamical plane gets the old Trace tool (shortcut @t@): from the
  point clicked or dragged to, it runs @script(1)@ up to 100 times, each
  time from where the last left off (the old program ran the script with
  an iteration limit of 1), marking each point and joining them with
  lines in a color set in the tool's panel, and stopping if the point
  escapes.
* Names keep the spelling they're first written with (the old language
  ignored case), except @z@ and @c@ (@C@), and old names for built-in
  values (@pixel@ is @pixel_size@, @maxIter@ is @max_iters@, ...). A name
  that means something else now is renamed, with a warning.
* Variables are declared where first set, if that's at the top level of
  a script before any other mention, and otherwise at the top.
* Comparisons with @<@ and so on compare complex numbers by modulus, as
  the old program did (where it can't tell a value is real).
* The vanishing radius (for @vanishes@, @=@, and @stops@) starts at
  0.01, as the old program's much larger tolerance did, by declaring
  @vanishing_radius@ in the \"Iteration settings\" tab.

A real-dynamics script (@{real}@) works on a plane of real points
@(x, y)@ instead: its viewers are real viewers, @\@viewer "Dynamical
plane" xvar=x yvar=y@, and, when it mentions the parameters @a@ or @b@,
@\@viewer "Parameter plane" xvar=a yvar=b@ (starting from @(x, y) = (0,
0)@). Its values are real unless they're complex for some other reason
(an @i@), @z@ and @c@ aren't special, a pair @(e1, e2)@ stays a pair
(so @(x, y) escapes@ measures its length), and an @iterate@ without a
variable iterates @(x, y)@. In arithmetic, though, a pair is the
complex number @x + y i@, as in the old program: @(x, y) * (0, 1)@
becomes @(x + y * i) * i@.

Setting several variables at once, @set (x, y) to (e1, e2).@, becomes a
tuple assignment, @x, y <- e1, e2@.

Not translated: special tools (@load@) and data sources (@using each@),
which depended on plug-ins: these are left as TODO comments, with
warnings. So are an @end.@ that ends no block and an @until c.@ with no
@do@ before it, which the parser keeps as strays when the script
doesn't parse otherwise, rather than guess what was meant.
-}
module Language.Legacy.Translate
  ( Translation(..)
  , translateLegacyScript
  ) where

import FractalStream.Prelude

import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Legacy.Syntax
import Language.Legacy.Parser (ParseNote(..), parseLegacyScript)
import Language.Parser.Tokenizer (wordlikeTokens)
import qualified Language.Value.Builtins as Builtins

-- | A translated project, and anything about the translation worth
-- knowing: commands that couldn't be translated, renamed variables, ...
data Translation = Translation
  { translatedProject :: String
  , translationWarnings :: [String]
  } deriving Show

-- | Translate a script into a project file, with the given title.
translateLegacyScript :: String -> String -> Either String Translation
translateLegacyScript title src0 = do
  -- (old Mac OS scripts end their lines with a carriage return)
  let src = normalizeNewlines src0
  (Script isReal body, notes) <- parseLegacyScript src
  let Translation project warnings =
        translate isReal title src (if isReal then map realIterate body else body)
  pure (Translation project (map noteWarning notes ++ warnings))

-- | Line endings as @\\n@, from @\\r\\n@ or a lone @\\r@.
normalizeNewlines :: String -> String
normalizeNewlines = \case
  '\r' : '\n' : rest -> '\n' : normalizeNewlines rest
  '\r' : rest -> '\n' : normalizeNewlines rest
  ch : rest -> ch : normalizeNewlines rest
  [] -> []

noteWarning :: ParseNote -> String
noteWarning = \case
  ThenStartsBlocks ->
    "This script writes `if ... then` without a colon before a block of commands ending in `end.`, \
    \so every `then` was read as `then:`; check that each `if` does what it should."

-- | In a real-dynamics script, an @iterate@ without a variable iterates
-- the point @(x, y)@: as a loop setting both (which 'stmt' writes as a
-- tuple @iterate@ where it can, when the new point is written as a
-- pair, and otherwise by way of a complex number).
realIterate :: Cmd -> Cmd
realIterate = \case
  Iterate e Nothing c -> DoUntil [Set ["x", "y"] (asPair e)] c
  Block cs -> Block (map realIterate cs)
  If c yes no -> If c (realIterate yes) (realIterate <$> no)
  DoUntil cs c -> DoUntil (map realIterate cs) c
  Repeat n b -> Repeat n (realIterate b)
  Par b -> Par (realIterate b)
  Dyn b -> Dyn (realIterate b)
  Probe fmt n b -> Probe fmt n (realIterate b)
  c -> c
  where
    asPair = \case
      Paren e@Pair{} -> e
      Paren e@Paren{} -> asPair e
      e -> e

------------------------------------------------------------------------
-- Planes

data Plane = ParamPlane | DynPlane
  deriving (Eq, Show)

-- | The commands meant for one plane, without @par@\/@dyn@, @default@s,
-- or probes (which are translated separately).
forPlane :: Plane -> [Cmd] -> [Cmd]
forPlane plane = concatMap go
  where
    go = \case
      Par c -> if plane == ParamPlane then go c else []
      Dyn c -> if plane == DynPlane then go c else []
      Default{} -> []
      Probe{} -> []
      Block cs -> [Block (concatMap go cs)]
      If c yes no -> [If c (Block (go yes)) (Block . go <$> no)]
      DoUntil cs c -> [DoUntil (concatMap go cs) c]
      Repeat n c -> [Repeat n (Block (go c))]
      c -> [c]

-- | The commands meant for either plane, keeping @par@ and @dyn@ but
-- without @default@s or probes.
forBothPlanes :: [Cmd] -> [Cmd]
forBothPlanes = concatMap go
  where
    go = \case
      Par c -> [Par (Block (go c))]
      Dyn c -> [Dyn (Block (go c))]
      Default{} -> []
      Probe{} -> []
      Block cs -> [Block (concatMap go cs)]
      If c yes no -> [If c (Block (go yes)) (Block . go <$> no)]
      DoUntil cs c -> [DoUntil (concatMap go cs) c]
      Repeat n c -> [Repeat n (Block (go c))]
      c -> [c]

-- | A command only for one plane.
isPlaneCmd :: Cmd -> Bool
isPlaneCmd = \case
  Par _ -> True
  Dyn _ -> True
  _ -> False

-- | Every command, at any depth (not looking inside probes).
everyCmd :: [Cmd] -> [Cmd]
everyCmd = concatMap $ \c -> c : case c of
  Block cs -> everyCmd cs
  If _ yes no -> everyCmd (yes : maybeToList no)
  DoUntil cs _ -> everyCmd cs
  Repeat _ b -> everyCmd [b]
  Par b -> everyCmd [b]
  Dyn b -> everyCmd [b]
  _ -> []

-- | Every probe, with its body.
probes :: [Cmd] -> [(ProbeFormat, String, Cmd)]
probes cmds = [ (fmt, name, body) | Probe fmt name body <- everyCmd cmds ]

-- | Splice blocks into the list of commands they're in.
flatten :: [Cmd] -> [Cmd]
flatten = concatMap $ \case
  Block cs -> flatten cs
  c -> [c]

------------------------------------------------------------------------
-- Names

-- | The names mentioned by an expression or condition.
exprVars :: Expr -> [String]
exprVars = \case
  Var v -> [v]
  Neg e -> exprVars e
  Bin _ a b -> exprVars a ++ exprVars b
  Call _ e -> exprVars e
  Pair a b -> exprVars a ++ exprVars b
  Paren e -> exprVars e
  _ -> []

condVars :: Cond -> [String]
condVars = \case
  Rel _ a b -> exprVars a ++ exprVars b
  Escapes e -> exprVars e
  Vanishes e -> exprVars e
  Stops e -> exprVars e
  BoolOp _ a b -> condVars a ++ condVars b
  ParenCond c -> condVars c

-- | The variables a command (and everything in it) sets, and mentions.
cmdSets, cmdVars :: Cmd -> [String]
cmdSets = \case
  Set vs _ -> vs
  Iterate _ v _ -> [fromMaybe "z" v]
  Reduce v _ -> [v]
  Default v _ -> [v]
  c -> concatMap cmdSets (children c)
cmdVars c = cmdSets c ++ here ++ concatMap cmdVars (children c)
  where
    here = case c of
      Set _ e -> exprVars e
      Default _ e -> exprVars e
      If cnd _ _ -> condVars cnd
      DoUntil _ cnd -> condVars cnd
      Iterate e _ cnd -> exprVars e ++ condVars cnd
      Repeat n _ -> exprVars n
      Reduce _ e -> exprVars e
      Report e -> exprVars e
      _ -> []

children :: Cmd -> [Cmd]
children = \case
  Block cs -> cs
  If _ yes no -> yes : maybeToList no
  DoUntil cs _ -> cs
  Repeat _ b -> [b]
  Par b -> [b]
  Dyn b -> [b]
  Probe _ _ b -> [b]
  _ -> []

lower :: String -> String
lower = map toLower

-- | Names that the old language gave to what are now built-in values
-- (in a real-dynamics script, @z@ and @c@ are ordinary names).
builtinNamesFor :: Bool -> Map String String
builtinNamesFor isReal = Map.fromList $
  [ ("z", "z") | not isReal ] ++ [ ("c", "C") | not isReal ] ++
  [ ("pixel", "pixel_size")
  , ("maxiter", "max_iters"), ("maxiters", "max_iters")
  , ("maxradius", "escape_radius"), ("minradius", "vanishing_radius") ]

-- | Names that can't be variables in a translated script.
reserved :: Set String
reserved = Set.fromList $ concat
  [ Map.keys wordlikeTokens
  , Map.keys Builtins.colors, Map.keys Builtins.realFunctions
  , Map.keys Builtins.commonFunctions, Map.keys Builtins.complexFunctions
  , Map.keys Builtins.roundingFunctions, Map.keys Builtins.types
  , Map.keys Builtins.wheels
  , [ "rgb", "hsv", "hsl", "blend", "cycle", "dark", "light", "invert", "mod"
    , "autocolor", "nearest_autocolor", "get_autocolor", "color"
    , "max_iters", "escape_radius", "vanishing_radius", "pixel_size"
    , "to", "for", "each", "in", "while", "until", "repeat", "iterate", "up"
    , "define", "run", "with", "first", "matching", "pass", "use", "draw"
    , "erase", "write", "on", "at", "from", "as", "choice", "row", "tab"
    , "button", "coloring", "point", "line", "circle"
    , "rectangle", "filled", "stroke", "fill" ] ]

-- | A name like the given one that isn't in the set.
freshIn :: Set String -> String -> String
freshIn used base = go (base : [ base ++ "_" ++ show k | k <- [2 :: Int ..] ])
  where
    go (n : ns) | n `Set.member` used = go ns
                | otherwise = n
    go [] = base

------------------------------------------------------------------------
-- Types: all the old language needs to know is which values are real

data Ty = TReal | TComplex
  deriving (Eq, Ord, Show)

exprTy :: (String -> Ty) -> Expr -> Ty
exprTy varTy = go
  where
    go = \case
      Num _ -> TReal
      Imag _ -> TComplex
      Pi -> TReal
      Var v -> varTy v
      Neg e -> go e
      Paren e -> go e
      Bin _ a b -> max (go a) (go b)
      Pair _ _ -> TComplex
      Call f e
        | lower f `elem` ["re", "im", "arg", "abs", "arccos", "arcsin", "arctan"] -> TReal
        | otherwise -> go e

------------------------------------------------------------------------
-- Output

-- | Lines of a script, some of them an indented block.
data Doc = Line String | Nest [Doc]

render :: Int -> [Doc] -> [String]
render n = concatMap $ \case
  Line s -> [replicate n ' ' ++ s]
  Nest ds -> render (n + 2) ds

------------------------------------------------------------------------
-- Translation

-- | What the translation of one script (a viewer's, or a probe's) needs.
data Ctx = Ctx
  { cxName     :: String -> String   -- ^ a (lower-cased) name, as written in the translation
  , cxTy       :: String -> Ty       -- ^ the type of a translated name
  , cxFlags    :: Map String String  -- ^ flag name to its color's variable
  , cxFlagVar  :: String             -- ^ the variable holding the current flag's color
  , cxOutcome  :: Maybe (String, String)
    -- ^ for a script that uses @succeed@\/@fail@: the variables standing
    -- in for the old iteration count and for whether its loop gave up
  , cxIsProbe  :: Bool
  , cxUsed     :: Set String         -- ^ every name in use, for fresh names
  , cxNonNegative :: Set String
    -- ^ real parameters that start out non-negative: taken to stay that
    -- way, so comparisons needn't take their modulus
  , cxReal :: Bool
    -- ^ a real-dynamics script, where a pair @(a, b)@ is a pair, not
    -- @a + b i@
  , cxPlaneFlag :: Maybe String
    -- ^ a boolean variable saying whether the script is running in the
    -- parameter plane, if @par@ and @dyn@ commands are to check it
  , cxLoopCap :: Maybe String
    -- ^ the most passes each loop makes, if not the iteration limit
    -- (for the Trace tool, which runs a script with a limit of 1)
  }

type T = State [String]   -- warnings, newest first

warn :: String -> T ()
warn w = modify (w :)

translate :: Bool -> String -> String -> [Cmd] -> Translation
translate isReal title src body =
  let (project, warnings) = runState (translateT isReal title src body) []
  in Translation project (nub (reverse warnings))

translateT :: Bool -> String -> String -> [Cmd] -> T String
translateT isReal title src body = do
  let mentioned = nub (map lower (concatMap cmdVars body))
      builtinNames = builtinNamesFor isReal
      -- the parameter plane's coordinates: C, or a real script's (a, b)
      hasParamPlane
        | isReal = any (`elem` mentioned) ["a", "b"]
        | otherwise = "c" `elem` mentioned
      planes = [ ParamPlane | hasParamPlane ] ++ [DynPlane]

  -- names, as they'll be written
  let firstSpelling = Map.fromListWith (\_ old -> old)
        [ (lower v, v) | v <- concatMap cmdVars body ]
      userNames = [ v | v <- Map.elems firstSpelling, lower v `Map.notMember` builtinNames ]
      (spellings, renames) = foldl' pick (Map.empty, []) userNames
      pick (m, rs) v
        | v `Set.member` reserved || v `elem` Map.elems m || v `elem` Map.elems builtinNames =
            let v' = freshIn (Set.fromList (Map.elems m) <> reserved <> Set.fromList userNames) (v ++ "_")
            in (Map.insert (lower v) v' m, rs ++ [(v, v')])
        | otherwise = (Map.insert (lower v) v m, rs)
      nameOf v = fromMaybe v (Map.lookup (lower v) builtinNames <|> Map.lookup (lower v) spellings)
      used0 = Set.fromList (Map.elems spellings) <> reserved
               <> Set.fromList (Map.elems builtinNames) <> Set.fromList ["iterations", "stuck"]
  let -- the dynamical plane's coordinates
      coordinates = if isReal then [nameOf "x", nameOf "y"] else ["z", "C"]
      -- where an orbit starts, in the parameter plane
      startingPoint = if isReal then nameOf "x" ++ ", " ++ nameOf "y" else "z"
  forM_ renames $ \(v, v') -> warn ("`" ++ v ++ "` means something else now, so it's renamed `" ++ v' ++ "`.")

  -- what's assigned what, to tell which variables are real
  let defaults = nub [ (nameOf v, e) | Default v e <- everyCmd body ]
      allCmds = everyCmd (body ++ [ b | (_, _, b) <- probes body ])
      assignments = concat
        [ case c of
            Set [v] e -> [(nameOf v, e)]
            Set vs (Pair a b) | length vs == 2 -> zip (map nameOf vs) [a, b]
            Set vs _ -> [ (nameOf v, Num "0") | v <- vs ]
            Iterate e v _ -> [(nameOf (fromMaybe "z" v), e)]
            Default v e -> [(nameOf v, e)]
            _ -> []
        | c <- allCmds ]
      assigned = Set.fromList (map fst assignments)
      fixed = Map.fromList $
        (if isReal
           then [ (nameOf v, TReal) | v <- ["x", "y"] ++ [ p | hasParamPlane, p <- ["a", "b"] ] ]
           else [("z", TComplex), ("C", TComplex)]) ++
        [ ("pixel_size", TReal), ("max_iters", TReal)
        , ("escape_radius", TReal), ("vanishing_radius", TReal), ("iterations", TReal) ]
      -- (the fixed types win: `z` is complex, however it's assigned)
      start = fixed `Map.union` Map.fromList [ (v, TReal) | v <- Set.toList assigned ]
      step env = Map.unionWith max env $ Map.fromListWith max
        [ (v, exprTy (look env . nameOf) e) | (v, e) <- assignments, v `Map.notMember` fixed ]
      look env v = Map.findWithDefault (if isReal then TReal else TComplex) v env
      settle env = let env' = step env in if env' == env then env else settle env'
      tyOf = look (settle start)

  -- flags, and their colors
  let flagNames = nub ("Default Exit Condition" :
        [ n | p <- planes, Flag n _ <- everyCmd (forPlane p body) ])
      hints = Map.fromListWith (\_ old -> old)
        [ (n, h) | Flag n (Just h) <- everyCmd body ]
      (flagVars, used1) = foldl' (\(m, u) n ->
          let v = freshIn u (flagVariable n) in (m ++ [(n, v)], Set.insert v u))
        ([], used0) flagNames
      unhinted = [ n | n <- flagNames, isNothing (Map.lookup n hints >>= colorName)
                                     , isNothing (colorName n) ]
      flagColor n = fromMaybe (palette !! (fromMaybe 0 (lookup n (zip unhinted [0 ..])) `mod` length palette))
                              ((Map.lookup n hints >>= colorName) <|> colorName n)
  forM_ (Map.toList hints) $ \(n, h) -> when (isNothing (colorName h)) $
    warn ("The color `" ++ h ++ "` for flag " ++ show n ++ " isn't one FractalStream knows, so it starts as "
          ++ flagColor n ++ ".")
  let flagVar = freshIn used1 "flag_color"
      shadeFn = freshIn (Set.insert flagVar used1) "shade"
      used2 = foldr Set.insert used1 [flagVar, shadeFn]
      ctx0 = Ctx { cxName = nameOf, cxTy = tyOf, cxFlags = Map.fromList flagVars
                 , cxFlagVar = flagVar, cxOutcome = Nothing, cxIsProbe = False, cxUsed = used2
                 , cxNonNegative = Set.fromList [ v | (v, e) <- defaults, exprTy (tyOf . nameOf) e == TReal
                                                    , nonNegativeLiteral e ]
                 , cxReal = isReal, cxLoopCap = Nothing, cxPlaneFlag = Nothing }

  -- @config tabs
  let -- the parameter plane's coordinates, as configuration variables
      planeParams
        | not hasParamPlane = []
        | isReal = [ (nameOf p, "R") | p <- ["a", "b"], nameOf p `notElem` map fst defaults ]
        | otherwise = [("C", "C")]
      configNames = Set.fromList (map fst defaults) <> Set.fromList (map fst planeParams)
      paramFields =
        [ v ++ " : " ++ t ++ " as \"" ++ v ++ " =\" := 0" | (v, t) <- planeParams ] ++
        [ v ++ " : " ++ (if exprTy (tyOf . nameOf) e == TReal then "R" else "C") ++ " as \"" ++ v ++ " =\" := "
            ++ expr ctx0 0 e
        | (v, e) <- defaults ]
  forM_ defaults $ \(v, e) -> unless (null (exprVars e)) $
    warn ("The default for `" ++ v ++ "` mentions other variables, which a default can't do now.")
  let parameters = [ Line "@config \"Parameters\"" ] ++ map Line paramFields
      colorsTab = Line "@config \"Colors\"" :
        [ Line (v ++ " : Color as " ++ quote n ++ " := " ++ flagColor n) | (n, v) <- flagVars ]

  -- the script, shared in @common by the viewers and the Trace tool: it
  -- takes the most passes each loop may make (the iteration limit, or 1
  -- for the Trace tool), and, if some commands are only for one plane,
  -- which plane it's running in
  let splitPlanes = hasParamPlane && not (null [ () | c <- everyCmd body, isPlaneCmd c ])
      scriptCmds = flatten (if splitPlanes then forBothPlanes body else forPlane DynPlane body)
      usesOutcome = not (null [ () | c <- everyCmd scriptCmds, c `elem` [Succeed, Fail] ])
      itersVar = freshIn used2 "iters"
      gaveUpVar = freshIn (Set.insert itersVar used2) "gave_up"
      outcome = if usesOutcome then Just (itersVar, gaveUpVar) else Nothing
      scriptFn = freshIn (foldr Set.insert used2 [itersVar, gaveUpVar]) "script"
      limitVar = freshIn (foldr Set.insert used2 [itersVar, gaveUpVar, scriptFn]) "limit"
      planeVar = freshIn (foldr Set.insert used2 [itersVar, gaveUpVar, scriptFn, limitVar]) "in_parameter_plane"
      used3 = foldr Set.insert used2 [itersVar, gaveUpVar, scriptFn, limitVar, planeVar]
      name v | lower v == "iters" = maybe "iterations" fst outcome
             | otherwise = nameOf v
      scriptCtx = ctx0 { cxName = name, cxOutcome = outcome, cxUsed = used3
                       , cxLoopCap = Just limitVar
                       , cxPlaneFlag = if splitPlanes then Just planeVar else Nothing }
      outcomeVars = maybe [] (\(a, b) -> [a, b]) outcome
      predeclaredS = configNames <> Set.fromList (coordinates ++ [flagVar, limitVar, planeVar] ++ outcomeVars)
      scriptParams = limitVar : [ planeVar | splitPlanes ]
      runScript limit plane =
        "run " ++ scriptFn ++ "(" ++ intercalate ", "
          (limit : [ if plane == ParamPlane then "true" else "false" | splitPlanes ]) ++ ")"
      -- the variables the script sets for coloring, declared where it's run
      coloringDecls =
        [ Line (flagVar ++ " := " ++ fromMaybe "" (Map.lookup "Default Exit Condition" (Map.fromList flagVars))) ]
        ++ concat [ [Line (a ++ " := 0"), Line (b ++ " := false")] | Just (a, b) <- [outcome] ]
  scriptDocs <- translateBody scriptCtx predeclaredS scriptCmds
  let scriptDef =
        [ Line ("define " ++ scriptFn ++ "(" ++ intercalate ", " scriptParams ++ "):")
        , Nest (orPass scriptDocs) ]

  -- viewers
  let viewers = flip map planes $ \plane ->
        let header = case (plane, isReal) of
              (ParamPlane, False) -> "@viewer \"Parameter plane\" var=C"
              (DynPlane, False)   -> "@viewer \"Dynamical plane\""
              (ParamPlane, True)  -> "@viewer \"Parameter plane\" xvar=" ++ nameOf "a" ++ " yvar=" ++ nameOf "b"
              (DynPlane, True)    -> "@viewer \"Dynamical plane\" xvar=" ++ nameOf "x" ++ " yvar=" ++ nameOf "y"
            start' = [ Line (startingPoint ++ " := 0" ++ (if isReal then ", 0" else "")) | plane == ParamPlane ]
            finish = case outcome of
              Nothing -> "color <- if stuck then black else " ++ shadeFn ++ "(" ++ flagVar ++ ", iterations / 20)"
              Just (a, b) -> "color <- if " ++ b ++ " then black else " ++ shadeFn ++ "(" ++ flagVar ++ ", " ++ a ++ " / 20)"
        in Line header : start' ++ coloringDecls ++ [Line (runScript "max_iters" plane), Line finish]

  -- tools: the probes (picking C, or (a, b), is the parameter plane's
  -- own Select tool). Each probe is a definition in @common, taking the
  -- parameter (C, or a real script's (a, b)), run by a tool on each
  -- viewer: with C itself in the dynamical plane, and the clicked point
  -- in the parameter plane
  let paramNames
        | not hasParamPlane = []
        | isReal = [nameOf "a", nameOf "b"]
        | otherwise = ["C"]
  probeParts <- forM (probes body) $ \(fmt, title', pbody) -> do
    let usedP = used3
        clickedA = freshIn usedP "clicked_a"
        clickedB = freshIn (Set.insert clickedA usedP) "clicked_b"
        clickedC = freshIn usedP "clicked"
        -- the clicked point: a complex number, or a real script's pair
        -- (in the dynamical plane, the point itself)
        clickedNames plane
          | isReal && plane == DynPlane = [nameOf "x", nameOf "y"]
          | isReal = [clickedA, clickedB]
          | otherwise = [clickedC]
        tuple ns = case ns of
          [n] -> n
          _ -> "(" ++ intercalate ", " ns ++ ")"
        usedP' = foldr Set.insert usedP [clickedA, clickedB, clickedC]
        field = freshIn usedP' (variableName title')
        probeFn = freshIn (Set.insert field usedP') ("probe_" ++ variableName title')
        probeSplit = hasParamPlane && any isPlaneCmd (everyCmd [pbody])
        ctx = ctx0 { cxIsProbe = True, cxUsed = foldr Set.insert usedP' [field, probeFn]
                   , cxPlaneFlag = if probeSplit then Just planeVar else Nothing }
        cmds = flatten (if probeSplit then forBothPlanes [pbody] else forPlane DynPlane [pbody])
        reports = [ e | Report e <- everyCmd cmds ]
        reported | not (null reports) = last reports
                 | isReal = Pair (Var "x") (Var "y")
                 | otherwise = Var "z"
        -- (a reported pair shows as a complex number)
        (fieldTy, fieldValue) = probeResult fmt (exprTy (cxTy ctx . cxName ctx) reported)
                                  (expr ctx { cxReal = False } 0 reported)
        predeclared = configNames <> Set.fromList (coordinates ++ field : planeVar : paramNames)
        probeParams = paramNames ++ [ planeVar | probeSplit ]
    when (length (nub reports) > 1) $
      warn ("The probe " ++ quote title' ++ " reports more than one thing; its tool shows the last one, at the end.")
    bodyDocs <- translateBody ctx predeclared cmds
    let def = [ Line ("define " ++ withArguments probeFn probeParams ++ ":")
              , Nest (bodyDocs ++ [Line (field ++ " <- " ++ fieldValue)]) ]
        tool plane =
          let clicked = tuple (clickedNames plane)
              viewerTitle = case plane of
                ParamPlane -> "Parameter plane"
                DynPlane   -> "Dynamical plane"
              start' = case (plane, isReal) of
                (ParamPlane, _) -> [Line (startingPoint ++ " := 0" ++ (if isReal then ", 0" else ""))]
                (DynPlane, False) -> [Line ("z := " ++ clicked)]
                (DynPlane, True) -> []
              arguments =
                (case plane of
                   ParamPlane -> clickedNames plane
                   DynPlane -> paramNames)
                ++ [ if plane == ParamPlane then "true" else "false" | probeSplit ]
          in [ Line ("@tool " ++ quote title' ++ " for=" ++ quote viewerTitle)
             , Line (field ++ " : " ++ fieldTy ++ " as " ++ quote title' ++ " := " ++ fieldDefault fieldTy)
             , Line ("on click at " ++ clicked ++ ":")
             , Nest (start' ++ [Line ("run " ++ withArguments probeFn arguments)]) ]
    pure (def, map tool planes)
  let probeDefs = map fst probeParts
      probeTools = concatMap snd probeParts

  -- the Trace tool, which the old program gave every viewer: from the
  -- clicked point, run the dynamical plane's script over and over with
  -- an iteration limit of 1, each run starting where the last ended,
  -- marking each point and joining them up
  let traceTool =
        let usedT = used3
            colorVar = freshIn usedT "trace_color"
            -- the point being traced
            traced = if isReal then [nameOf "x", nameOf "y"] else ["z"]
            tuple ns = case ns of
              [n] -> n
              _ -> "(" ++ intercalate ", " ns ++ ")"
            point = tuple traced
            -- (where the point was at the start of this step)
            previous = tuple (map ("previous " ++) traced)
            toolTitle = freshIn (Set.fromList [ t | (_, t, _) <- probes body ]) "Trace"
        in [ Line ("@tool " ++ quote toolTitle ++ " for=\"Dynamical plane\" shortcut=t")
           , Line (colorVar ++ " : Color as \"Trace color\" := white")
           , Line ""
           , Line "on deactivated:"
           , Nest [Line "erase"]
           , Line ""
           , Line ("on click-or-drag at " ++ point ++ ":")
           , Nest ( [ Line "erase"
                    , Line ("use " ++ colorVar ++ " for line")
                    , Line "# (the script sets these for coloring, which the tool doesn't use)" ]
                    ++ coloringDecls ++
                    [ Line ("draw point at " ++ point)
                    , Line "# each step runs the script once more, with each loop making at most one pass"
                    , Line "repeat up to 100 times:"
                    , Nest [ Line (runScript "1" DynPlane)
                           , Line ("draw point at " ++ point)
                           , Line ("draw line from " ++ previous ++ " to " ++ point) ]
                    , Line ("until " ++ point ++ " escapes") ] ) ]

  -- a flag's gradient: its color at t = 0, then darker, back, lighter,
  -- and back again at t = 1
  let common =
        [ Line "@common"
        , Line (shadeFn ++ "(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...")
        , Line "  else blend(-sin(2 pi t), light c, c)"
        , Line "" ]
        ++ intercalate [Line ""] (scriptDef : probeDefs)
      -- the old program's tolerance for `vanishes`, `=`, and `stops`
      -- was much larger than this one's default
      settingsTab =
        [ Line "@config \"Iteration settings\""
        , Line "vanishing_radius : R as \"Vanishing radius: \" := 0.01" ]
      stanzas = [parameters | not (null paramFields)] ++ [colorsTab, settingsTab, common] ++ viewers ++ probeTools
                ++ [traceTool]
      scriptLines = intercalate [""] (map (render 0) stanzas)
  pure . unlines $
    [ "# Translated from a script for the original FractalStream."
    , "script: |" ]
    ++ map (\l -> if null l then "" else "  " ++ l) scriptLines ++
    [ "metadata:"
    , "  title: " ++ quote title
    , "  description: Translated from a script for the original FractalStream."
    , ""
    , "# The original script:" ]
    ++ map (\l -> if all isSpace l then "#" else "#   " ++ l) (lines src)

-- | Code's name, with its arguments or parameters if it has any (code
-- without them is written without parentheses).
withArguments :: String -> [String] -> String
withArguments f = \case
  [] -> f
  args -> f ++ "(" ++ intercalate ", " args ++ ")"

-- | A number that's not negative, possibly as a fraction.
nonNegativeLiteral :: Expr -> Bool
nonNegativeLiteral = \case
  Num _ -> True
  Pi -> True
  Paren e -> nonNegativeLiteral e
  Bin op a b | op `elem` [Mul, Div] -> nonNegativeLiteral a && nonNegativeLiteral b
  _ -> False

-- | Text in double quotes, as both FractalStream's language and YAML
-- write it.
quote :: String -> String
quote t = "\"" ++ concatMap escape t ++ "\""
  where
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      ch -> [ch]

-- | The type of a probe's field, and the value it shows.
probeResult :: ProbeFormat -> Ty -> String -> (String, String)
probeResult fmt ty v = case fmt of
  ProbeComplex  -> ("C", v)
  ProbeReal     -> ("R", if ty == TReal then v else "re(" ++ v ++ ")")
  ProbeInteger  -> ("Z", if ty == TReal then "round(" ++ v ++ ")" else "round(re(" ++ v ++ "))")
  ProbeRational -> ("Text", "text(round(re(" ++ v ++ ")), \"/\", round(im(" ++ v ++ ")))")

fieldDefault :: String -> String
fieldDefault = \case
  "Text" -> "\"\""
  _ -> "0"

-- | The variable for a flag's color.
flagVariable :: String -> String
flagVariable n = variableName n ++ "_color"

-- | A variable name made from some text: lower case, with underscores
-- between words.
variableName :: String -> String
variableName n =
  let ws = words (map (\ch -> if isAlphaNum ch then toLower ch else ' ') n)
      v = intercalate "_" ws
  in case v of
       "" -> "value"
       ch : _ | isDigit ch -> "v_" ++ v
       _ -> v

-- | A color, written the way FractalStream writes it, if this is one:
-- a color's name, maybe with @dark@s or @light@s before it.
colorName :: String -> Maybe String
colorName s = case words (lower s) of
  [] -> Nothing
  ws | all (`elem` ["dark", "light"]) (init ws)
     , last ws `Map.member` Builtins.colors -> Just (unwords ws)
     | otherwise -> Nothing

-- | Colors for flags that don't say what color they should be.
palette :: [String]
palette = ["blue", "red", "green", "orange", "purple", "yellow", "pink", "violet"]

------------------------------------------------------------------------
-- Statements

-- | The statements of a viewer's script or a probe's handler, preceded
-- by declarations of any variables that can't be declared where they're
-- first set.
translateBody :: Ctx -> Set String -> [Cmd] -> T [Doc]
translateBody ctx predeclared cmds = do
  let name = cxName ctx
      -- variables first set at the top level, before any other mention
      firstSets = go Set.empty Set.empty cmds
        where
          go _ acc [] = acc
          go seen acc (c : rest) =
            let acc' = case c of
                  Set [v] e | let v' = name v
                            , v' `Set.notMember` seen, v' `Set.notMember` predeclared
                            , v' `notElem` map name (exprVars e) -> Set.insert v' acc
                  Set vs@[_, _] e@Pair{}
                            | let vs' = map name vs
                            , all (`Set.notMember` seen) vs', all (`Set.notMember` predeclared) vs'
                            , all (`notElem` map name (exprVars e)) vs' -> foldr Set.insert acc vs'
                  _ -> acc
            in go (seen <> Set.fromList (map name (cmdVars c))) acc' rest
      special = Set.fromList ["pixel_size", "max_iters", "escape_radius", "vanishing_radius", "iterations"]
      mentioned = nub (map name (concatMap cmdVars cmds))
      setSomewhere = Set.fromList (map name (concatMap cmdSets cmds))
      hoisted = [ v | v <- mentioned, v `Set.notMember` predeclared, v `Set.notMember` firstSets
                    , v `Set.notMember` special ]
  forM_ hoisted $ \v -> unless (v `Set.member` setSomewhere) $
    warn ("`" ++ v ++ "` is used without ever being set; it starts at 0.")
  docs <- evalStateT (concat <$> mapM (stmt ctx True False) cmds) (Pending firstSets Set.empty)
  pure (map (\v -> Line (v ++ " := 0")) hoisted ++ docs)

-- | What 'stmt' keeps track of: the variables yet to be declared, where
-- they're first set, and the variables it has made up and declared.
data Pending = Pending
  { pendingDecls :: Set String
  , madeUp :: Set String
  }

-- | Translate a command, given whether it's at the top level of its
-- script and whether it's inside a loop.
stmt :: Ctx -> Bool -> Bool -> Cmd -> StateT Pending T [Doc]
stmt ctx top inLoop = \case
  Comment s -> pure (map (Line . ("# " ++) . trimStart) (lines s))

  Set [v] e -> do
    let v' = cxName ctx v
    pending <- gets pendingDecls
    if top && v' `Set.member` pending
      then do
        modify (\p -> p { pendingDecls = Set.delete v' pending })
        pure [Line (v' ++ " := " ++ ex e)]
      else pure [Line (v' ++ " <- " ++ ex e)]

  Set vs e -> case (vs, e) of
    -- the new values are all worked out before any is set
    ([v1, v2], Pair a b) -> do
      let (x, y) = (cxName ctx v1, cxName ctx v2)
      pending <- gets pendingDecls
      -- (declared here, if they're first set here)
      if top && x `Set.member` pending && y `Set.member` pending
        then do
          modify (\p -> p { pendingDecls = Set.delete x (Set.delete y pending) })
          pure [Line (x ++ ", " ++ y ++ " := " ++ ex a ++ ", " ++ ex b)]
        else pure [Line (x ++ ", " ++ y ++ " <- " ++ ex a ++ ", " ++ ex b)]
    ([v1, v2], _) -> do
      let (x, y) = (cxName ctx v1, cxName ctx v2)
      tmp <- freshDecl "new_point"
      pure [ Line (tmp ++ " := " ++ ex e), Line (x ++ " <- re(" ++ tmp ++ ")"), Line (y ++ " <- im(" ++ tmp ++ ")") ]
    _ -> do
      lift (warn ("Couldn't translate setting " ++ show (length vs) ++ " variables at once."))
      pure [Line ("# TODO: set (" ++ intercalate ", " vs ++ ") to " ++ ex e)]

  -- (an `if` with nothing to do here, like one that only sets a flag in a
  -- tool, which has no flags: conditions do nothing, so it can go)
  c@If{} | inert c -> pure []

  If c yes no -> do
    yes' <- block yes
    no' <- elsePart no
    pure (Line ("if " ++ cnd c ++ ":") : Nest yes' : no')

  Block cs -> concat <$> mapM (stmt ctx top inLoop) cs

  -- (an `iterate` of a pair, in a real-dynamics script)
  DoUntil [Set [v1, v2] (Pair a b)] c | checksFirstSafely c ->
    let (x, y) = (cxName ctx v1, cxName ctx v2)
    in pure (Line ("iterate (" ++ x ++ ", " ++ y ++ ") -> (" ++ ex a ++ ", " ++ ex b ++ ") until " ++ cnd c
                   ++ iterateCap)
             : afterLoop)

  DoUntil cs c -> do
    b <- block (Block cs)
    pure ([Line repeatHeader, Nest b, Line ("until " ++ cnd c)] ++ afterLoop)

  Iterate e mv c -> do
    let v = cxName ctx (fromMaybe "z" mv)
    -- `iterate` checks its condition first, `do ... until` (which the old
    -- `iterate` was short for) only after the first pass: they agree when
    -- the condition is only `escapes` and `stops`, which is false on the
    -- first check anyway, so then the shorthand reads better
    pure $ if checksFirstSafely c
      then Line ("iterate " ++ v ++ " -> " ++ ex e ++ " until " ++ cnd c ++ iterateCap) : afterLoop
      else [Line repeatHeader, Nest [Line (v ++ " <- " ++ ex e)], Line ("until " ++ cnd c)] ++ afterLoop

  Repeat n b -> do
    b' <- stmtsIn True b
    let k = fresh "k"
        count = case n of
          Num s | all isDigit s -> s
          _ -> "round(" ++ (if exprTy (cxTy ctx . cxName ctx) n == TReal then ex n else "re(" ++ ex n ++ ")") ++ ")"
    pure [Line ("for each " ++ k ++ " in range(1, " ++ count ++ "):"), Nest (orPass b')]

  Reduce v e -> do
    let v' = cxName ctx v
        m = expr ctx 8 e
    pure [ Line ("# (reduce " ++ v' ++ " mod " ++ ex e ++ ": the remainder, with " ++ v' ++ "'s sign)")
         , Line (v' ++ " <- " ++ v' ++ " - " ++ m ++ " * (if " ++ v' ++ " < 0 then ceil(" ++ v' ++ " / "
                 ++ m ++ ") else floor(" ++ v' ++ " / " ++ m ++ "))") ]

  -- what a probe reports is shown when it's done; viewers don't report
  Report _ -> pure []

  Succeed -> pure $ case cxOutcome ctx of
    Just (iters, gaveUp) -> [Line (iters ++ " <- 0"), Line (gaveUp ++ " <- false")]
    Nothing -> []
  Fail -> pure $ case cxOutcome ctx of
    Just (_, gaveUp) -> [Line (gaveUp ++ " <- true")]
    Nothing -> []

  Flag n _
    | cxIsProbe ctx -> pure []
    | otherwise -> pure [Line (cxFlagVar ctx ++ " <- " ++ Map.findWithDefault "" n (cxFlags ctx))]

  Stray what src -> do
    let why = case what of
          "until" -> "with no `do` before it to end"
          _ -> "which doesn't end any block"
    lift (warn ("The original script has `" ++ src ++ "`, " ++ why ++ "; it's left as a comment."))
    pure [ Line ("# NOTE: the original script has this here, " ++ why ++ ":")
         , Line ("#   " ++ src) ]

  Unsupported what src -> do
    lift (warn ("`" ++ what ++ "` has no counterpart in this FractalStream, so this is left as a TODO: " ++ src))
    pure [ Line ("# TODO: `" ++ what ++ "` has no counterpart in this FractalStream:")
         , Line ("#   " ++ src) ]

  -- these were dealt with before getting here
  Default{} -> pure []
  Probe{} -> pure []
  -- (in a script for both planes, which says which plane it's in)
  Par c | Just p <- cxPlaneFlag ctx -> onlyIf p c
  Dyn c | Just p <- cxPlaneFlag ctx -> onlyIf ("not " ++ p) c
  Par c -> stmt ctx top inLoop c
  Dyn c -> stmt ctx top inLoop c
  where
    ex = expr ctx 0
    cnd = cond ctx 0
    fresh = freshIn (cxUsed ctx)

    -- a loop's own limit on its passes, if it has one
    iterateCap = maybe "" (\n -> " up to " ++ n ++ " times") (cxLoopCap ctx)
    repeatHeader = maybe "repeat:" (\n -> "repeat up to " ++ n ++ " times:") (cxLoopCap ctx)

    -- a made-up variable to declare, different from any declared before
    freshDecl base = do
      p <- get
      let v = freshIn (cxUsed ctx <> madeUp p) base
      put p { madeUp = Set.insert v (madeUp p) }
      pure v

    block c = orPass <$> stmtsIn inLoop c
    stmtsIn loopy c = concat <$> mapM (stmt ctx False loopy) (flatten [c])

    elsePart = \case
      Nothing -> pure []
      Just c -> case flatten [c] of
        [] -> pure []
        [If c' yes' no'] -> do
          yes'' <- block yes'
          rest <- elsePart no'
          pure (Line ("else if " ++ cnd c' ++ ":") : Nest yes'' : rest)
        _ -> do
          b <- block c
          pure [Line "else:", Nest b]

    onlyIf test c = do
      b <- block c
      pure [Line ("if " ++ test ++ ":"), Nest b]

    -- whether a command does nothing in this script
    inert = \case
      Flag{} -> cxIsProbe ctx
      Report _ -> True
      Succeed -> isNothing (cxOutcome ctx)
      Fail -> isNothing (cxOutcome ctx)
      Block cs -> all inert cs
      If _ yes no -> all inert (yes : maybeToList no)
      _ -> False

    -- remember how the last outermost loop ended, for `succeed`/`fail`
    afterLoop = case cxOutcome ctx of
      Just (iters, gaveUp) | not inLoop ->
        [Line (iters ++ " <- iterations"), Line (gaveUp ++ " <- stuck")]
      _ -> []

-- | A block's statements, or @pass@ if it has none (comments don't count).
orPass :: [Doc] -> [Doc]
orPass ds
  | any isStatement ds = ds
  | otherwise = ds ++ [Line "pass"]
  where
    isStatement = \case
      Line ('#' : _) -> False
      _ -> True

trimStart :: String -> String
trimStart = dropWhile isSpace

-- | Whether a condition is only @escapes@ and @stops@ tests.
checksFirstSafely :: Cond -> Bool
checksFirstSafely = \case
  Escapes _ -> True
  Stops _ -> True
  BoolOp Or a b -> checksFirstSafely a && checksFirstSafely b
  ParenCond c -> checksFirstSafely c
  _ -> False

------------------------------------------------------------------------
-- Expressions

-- | An expression, written in a context of the given precedence: 6 for
-- an operand of @+@ or @-@, 7 of @*@ or @/@, 8 of negation, 9 of @^@, 10
-- for an atom. Parentheses the script wrote are kept.
expr :: Ctx -> Int -> Expr -> String
expr ctx = go
  where
    go p e = let (q, s) = shown e in if q < p then "(" ++ s ++ ")" else s

    -- a negation as the right operand of an operator gets parentheses
    rightOf _ e@(Neg _) = "(" ++ go 0 e ++ ")"
    rightOf p e = go p e

    shown = \case
      Num s -> number s
      Imag "1" -> (10, "i")
      Imag s -> (8, snd (number s) ++ "i")
      Pi -> (10, "pi")
      Var v -> (10, cxName ctx v)
      Neg e@(Neg _) -> (8, "-(" ++ go 0 e ++ ")")
      Neg e -> (8, "-" ++ go 8 e)
      -- in a real-dynamics script, a pair in arithmetic is a complex
      -- number, x + y i, as the old program took it
      e | cxReal ctx, isArithmetic e -> shown' (ctx { cxReal = False }) e
      Bin op a b -> case op of
        Add -> (6, go 6 a ++ " + " ++ rightOf 7 b)
        Sub -> (6, go 6 a ++ " - " ++ rightOf 7 b)
        Mul -> (7, go 7 a ++ " * " ++ rightOf 8 b)
        Div -> (7, go 7 a ++ " / " ++ rightOf 8 b)
        Pow -> (9, go 10 a ++ "^" ++ go 10 b)
      Call f e -> (10, call (lower f) e)
      Pair a b
        | cxReal ctx -> (10, "(" ++ go 0 a ++ ", " ++ go 0 b ++ ")")
        | otherwise -> complexPair a b
      Paren e -> (10, "(" ++ go 0 e ++ ")")

    call f e
      | f == "abs" = "|" ++ go 0 e ++ "|"
      | otherwise = f ++ "(" ++ go 0 e ++ ")"

    shown' ctx' e = let s = expr ctx' 0 e in (precedence e, s)
    precedence = \case
      Bin op _ _ -> case op of
        Add -> 6; Sub -> 6; Mul -> 7; Div -> 7; Pow -> 9
      Neg _ -> 8
      _ -> 10

    -- (a, b) as a complex number, a + b i, leaving out a zero part
    complexPair a b = case (zero a, b) of
      (True, _) | zero b -> (10, "0")
      (True, Num "1") -> (10, "i")
      (True, _) -> (7, rightOf 7 b ++ " * i")
      (False, _) | zero b -> shown a
      _ -> (6, go 6 a ++ " + " ++ rightOf 7 b ++ " * i")
    zero = \case
      Num s -> not (null s) && all (`elem` "0.") s
      Paren e -> zero e
      _ -> False

-- | Arithmetic, in which a pair stands for a complex number.
isArithmetic :: Expr -> Bool
isArithmetic = \case
  Bin{} -> True
  Neg _ -> True
  Call _ _ -> True
  _ -> False

-- | A number as this FractalStream writes it: with a digit before any
-- decimal point, and without scientific notation (where @e@ would be
-- Euler's number).
number :: String -> (Int, String)
number s =
  let (mantissa, ex) = break (`elem` "eE") s
      m = case mantissa of
            '.' : _ -> '0' : mantissa
            _ -> mantissa
  in case ex of
       [] -> (10, m)
       _ : rest ->
         let power = case rest of
               '+' : ds -> ds
               ds -> ds
             p = case power of
               '-' : _ -> "(" ++ power ++ ")"
               _ -> power
         in (7, m ++ " * 10^" ++ p)

------------------------------------------------------------------------
-- Conditions

-- | A condition, in a context of the given precedence: 1 for an operand
-- of @or@, 2 of @and@, 3 for a comparison.
cond :: Ctx -> Int -> Cond -> String
cond ctx = go
  where
    go p c = let (q, s) = shown c in if q < p then "(" ++ s ++ ")" else s
    ex = expr ctx 0
    shown = \case
      BoolOp And a b -> (2, go 2 a ++ " and " ++ go 3 b)
      BoolOp Or a b -> (1, go 1 a ++ " or " ++ go 2 b)
      BoolOp Xor a b -> (3, "(" ++ go 0 a ++ ") != (" ++ go 0 b ++ ")")
      ParenCond c -> (3, "(" ++ go 0 c ++ ")")
      Escapes e -> (3, ex e ++ " escapes")
      Vanishes e -> (3, ex e ++ " vanishes")
      Stops e -> (3, ex e ++ " stops")
      Rel Equal a b -> (3, ex a ++ " = " ++ ex b)
      Rel op a b ->
        let real = all ((== TReal) . exprTy (cxTy ctx . cxName ctx)) [a, b]
            -- (a modulus is of a number, so a pair in it is x + y i)
            modulus = expr ctx { cxReal = False } 0
            side e | real || nonNegative e = ex e
                   | Paren e' <- e = "|" ++ modulus e' ++ "|"
                   | otherwise = "|" ++ modulus e ++ "|"
            o = case op of
              Less -> "<"; LessEq -> "<="; Greater -> ">"; GreaterEq -> ">="
        in (3, side a ++ " " ++ o ++ " " ++ side b)

    -- real values that are never negative, so their modulus is themselves
    nonNegative = \case
      Num _ -> True
      Pi -> True
      Var v -> cxName ctx v `elem` ["pixel_size", "escape_radius", "vanishing_radius", "max_iters"]
                 || cxName ctx v `Set.member` cxNonNegative ctx
      Paren e -> nonNegative e
      Bin op a b | op `elem` [Mul, Div] -> nonNegative a && nonNegative b
      _ -> False
