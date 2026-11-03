{-# language OverloadedStrings #-}
-- | Colorizers: small, named pieces of viewer code that set a viewer's
-- @color@, so that a viewer script only has to say what to compute
-- (@iterate z -> z^2 + C until z escapes@) and not also how to show it.
--
-- A colorizer has a block of code, run at the end of the viewer's own
-- code, and may have configuration fields (shown as a tab of the
-- project's configuration window) and parameters (given when it's chosen,
-- e.g. @Smooth(z)@, like the arguments of @run name(a, b)@; a parameter
-- without an argument gets its default value, @t = iterations / period@,
-- if it has one, or else the viewer's variable of the same name). Each
-- viewer
-- gets its own copy of the fields, and every name the colorizer itself
-- declares -- its fields, parameters, and local variables -- is renamed to
-- one no script can write, so that a colorizer can never clash with a
-- script's own names.
--
-- Some colorizers are built in ('builtinColorizers'); a script can define
-- more with @\@colorizer@ stanzas:
--
-- > @colorizer "Smooth"
-- > period : R as "Color period" <- 20
-- >
-- > define colorize(v):
-- >   ...
--
-- A script's colorizer may have the same name as a built-in one, and is
-- used instead of it (with a warning): so that a new built-in colorizer
-- never breaks a script that already had one by that name.
--
-- Which colorizer a viewer uses is, in order of preference:
--
-- 1. the one chosen interactively, saved under the project file's
--    @colorizers:@ key (by viewer title);
-- 2. the one named by its @\@viewer@ header's @colorizer=@ attribute;
-- 3. otherwise, @Manual@ (which does nothing) if the viewer's code sets
--    @color@ itself, and @Rainbow@ if it doesn't.
--
-- The last two are the viewer's /default/ colorizer, which "reset to
-- defaults" goes back to.
module Actor.Colorizer
  ( Colorizer(..)
  , builtinColorizers
  , manualColorizerName
  , rainbowColorizerName
  , ViewerColorizer(..)
  , Colorized(..)
  , applyColorizers
  , setsColor
  , splitArguments
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isSpace, toLower)
import Data.List (unzip4, find, dropWhileEnd, nub, (\\))

import Actor.Script
import Language.Parser (ParseError, parse)
import Language.Parser.Tokenizer (tokenize)
import Language.Parser.SourceRange
import Language.Typecheck (TC(..))
import Language.Code.Syntax
import Language.Code.Procedures (expandRuns, renameStmt)
import Language.Code.Typecheck.Functions (rename)
import Language.Value.Syntax (Expr, Syn(..), ExprF(EVar, ELit), textLiteral)
import Language.Type (SomeHaskellType(..), TypeProxy(IntegerType))
import Language.Value.Parser (argumentGrammarWithFunctions)

-- | A colorizer, ready to be added to a viewer.
data Colorizer = Colorizer
  { colorizerName        :: String
  , colorizerDescription :: Maybe String
  , colorizerParams      :: [String]
    -- ^ The names of its parameters, in order.
  , colorizerDefaults    :: Map String (Expr, String)
    -- ^ The parameters that have default values: each one's value, and
    -- its text as written.
  , colorizerConfig      :: [ConfigStmt]
    -- ^ Its configuration fields' declarations.
  , colorizerCommon      :: [Stmt]
    -- ^ The @\@common@ statements its code uses.
  , colorizerCode        :: Maybe Stmt
    -- ^ The code to run after the viewer's own; 'Nothing' for one that
    -- does nothing at all (@Manual@).
  }

manualColorizerName, rainbowColorizerName :: String
manualColorizerName = "Manual"
rainbowColorizerName = "Rainbow"

-- | The colorizers built into FractalStream, in the order a menu should
-- list them.
builtinColorizers :: [Colorizer]
builtinColorizers =
  [ Colorizer
      { colorizerName = manualColorizerName
      , colorizerDescription = Just "The viewer's own code sets the color."
      , colorizerParams = []
      , colorizerDefaults = Map.empty
      , colorizerConfig = []
      , colorizerCommon = []
      , colorizerCode = Nothing
      }
  , builtin $ unlines
      [ "@colorizer \"Rainbow\" description=\"Points that escape cycle through \
        \the rainbow by iteration count; the rest are black.\""
      , "color_speed : R as \"Iterations per color cycle:\" <- 20"
      , "use_arenberg : Boolean as \"Use the perceptually even Arenberg wheel\" <- false"
      , "define colorize(t = iterations / color_speed):"
      , "  if stuck:"
      , "    color <- black"
      , "  else:"
      , "    color <- if use_arenberg then arenberg(t) else rainbow(t)"
      ]
  , builtin $ unlines $
      [ "@colorizer \"Escape time\" description=\"Bands of color by iteration count, \
        \from a choice of palettes.\""
      , paletteDecl
      , "period : R as \"Iterations per color cycle:\" <- 20"
      , "shade : Boolean as \"Shade by iteration count\" <- false"
      ] ++ twoColorsDecl ++
      [ givesUpDecl
      , "define colorize(t = iterations / period):"
      , "  if stuck:"
      , "    color <- gives_up"
      , "  else:"
      , "    color <- " ++ paletteExpr
      , "    if shade:"
      , "      color <- cycle(iterations / period, dark color, light color)"
      ]
  , builtin $ unlines $
      [ "@colorizer \"Smooth\" description=\"Continuous color by escape time, \
        \without bands, from a choice of palettes. Its argument is the variable \
        \that escapes.\""
      , paletteDecl
      , "period : R as \"Iterations per color cycle:\" <- 20"
      , degreeDecl
      ] ++ twoColorsDecl ++
      [ givesUpDecl
      , "define colorize(z):"
      , "  if stuck:"
      , "    color <- gives_up"
      , "  else:"
      , "    t := iterations / period"
      , "    if z escaped:"
      , "      t <- (" ++ smoothIterations ++ ") / period"
      , "    color <- " ++ paletteExpr
      ]
  , builtin $ unlines
      [ "@colorizer \"Binary decomposition\" description=\"Smooth color by escape \
        \time, in different colors where the escaping variable ends up in the upper \
        \and lower half-planes. Its argument is the variable that escapes.\""
      , "period : R as \"Iterations per color cycle:\" <- 10"
      , degreeDecl
      , "row:"
      , "  upper1 : Color as \"Upper half-plane:\" <- red"
      , "  upper2 : Color as \"and\" <- yellow"
      , "row:"
      , "  lower1 : Color as \"Lower half-plane:\" <- green"
      , "  lower2 : Color as \"and\" <- blue"
      , "smooth : Boolean as \"Use smooth coloring?\" <- true"
      , givesUpDecl
      , "define colorize(z):"
      , "  if stuck:"
      , "    color <- gives_up"
      , "  else:"
      , "    s := iterations"
      , "    if smooth and z escaped:"
      , "      s <- " ++ smoothIterations
      , "    color <- if im z > 0 then cycle(s / period, upper1, upper2) else cycle(s / period, lower1, lower2)"
      ]
  , builtin $ unlines
      [ "@colorizer \"Autocolor\" description=\"Gives each point its argument ends up \
        \at a color of its own, the first time it's seen: each root a Newton's \
        \method converges to, say. Viewers using it share its colors (it uses \
        \autocolor group 0). Its argument is the variable that converges.\""
      , "allow_new : Boolean as \"Allow new autocolor entries\" <- true"
      , "shade : Boolean as \"Shade by iteration count\" <- true"
      , "period : R as \"Iterations per shading cycle:\" <- 20"
      , givesUpDecl
      , "define colorize(z):"
      , "  if stuck:"
      , "    color <- gives_up"
      , "  else:"
      , "    color <- if allow_new then autocolor(0, z) else get_autocolor(0, z)"
      , "    if shade:"
      , "      color <- cycle(iterations / period, dark color, light color)"
      ]
  , builtin $ unlines $
      [ "@colorizer \"Glow\" description=\"Blends between two colors by how close \
        \the iteration count came to the iteration limit.\""
      , "brightness : R as \"Brightness exponent:\" <- 0.5"
      , "row:"
      , "  near : Color as \"Near the limit:\" <- dark blue"
      , "  far : Color as \"Far from it:\" <- white"
      , givesUpDecl
      , "define colorize:"
      , "  if stuck:"
      , "    color <- gives_up"
      , "  else:"
      , "    color <- blend ((iterations / max_iters)^brightness, near, far)"
      ]
  ]

-- | The palettes a colorizer's @palette@ choice can pick from, by name,
-- and how each colors @t@ (all of them repeat with period 1).
palettes :: [(String, String)]
palettes =
  [ (name, map toLower name ++ "(t)")
  | name <- ["Rainbow", "Arenberg", "Romao", "Bamo", "Broco", "Corko", "Viko"
            , "Fire", "Ice", "Ocean", "Forest", "Rose", "Wheat"] ]
  ++ [ ("Two colors", "cycle(t, color1, color2)") ]

paletteDecl, givesUpDecl, degreeDecl, smoothIterations, paletteExpr :: String
paletteDecl = "palette : Z as \"Palette:\" choice { "
  ++ intercalate ", " (map (show . fst) palettes) ++ " } <- 0"
givesUpDecl = "gives_up : Color as \"Color when the loop gives up:\" <- black"
degreeDecl = "degree : R as \"Degree of the map:\" <- 2"
-- | The continuous ("smoothed") iteration count of an escaping @z@: see
-- https://linas.org/art-gallery/escape/ray.html. (With a different
-- escape radius than @escape_radius@, it's only off by a constant.)
smoothIterations = "iterations + 1 - log (log |z| / log escape_radius) / log degree"
paletteExpr = go (zip [0 :: Int ..] (map snd palettes))
  where
    go [(_, e)] = e
    go ((i, e) : rest) = "if palette = " ++ show i ++ " then " ++ e ++ " else " ++ go rest
    go [] = "black"

twoColorsDecl :: [String]
twoColorsDecl =
  [ "row:"
  , "  color1 : Color as \"Two colors:\" <- dark blue"
  , "  color2 : Color as \"and\" <- white"
  ]

-- | A built-in colorizer, written as a script's @\@colorizer@ stanza
-- would be. One that doesn't parse is a bug, caught by the test suite
-- (which checks every one of them). Its source ranges are forgotten,
-- since they're in this source, not the script it's used in.
builtin :: String -> Colorizer
builtin src = case parseScript src of
  Right [stanza] | Just c <- fromStanza stanza -> c
    { colorizerConfig = map (renameConfig id True) (colorizerConfig c)
    , colorizerCode = stripRanges <$> colorizerCode c
    , colorizerDefaults = Map.map (first (fmap (const NoSourceRange))) (colorizerDefaults c)
    }
  Right _ -> broken "it isn't exactly one @colorizer stanza"
  Left (_, err) -> broken err
  where
    broken why = error ("A built-in colorizer is broken: " ++ why ++ "\n" ++ src)

-- | The colorizer a @\@colorizer@ stanza defines.
fromStanza :: ParsedStanza -> Maybe Colorizer
fromStanza = \case
  ColorizerStanza{..} -> Just Colorizer
    { colorizerName = czName
    , colorizerDescription = czDescription
    , colorizerParams = map cpName czParams
    , colorizerDefaults = Map.fromList
        [ (cpName p, (v, cpDefaultSrc p)) | p <- czParams, Just v <- [cpDefault p] ]
    , colorizerConfig = czConfig
    , colorizerCommon = czCommon
    , colorizerCode = Just czCode
    }
  _ -> Nothing

-- | How a viewer is colorized: by which colorizer, and by which one it
-- would be without an interactive choice. Both are written as a
-- colorizer is chosen: its name, followed by any arguments in
-- parentheses.
data ViewerColorizer = ViewerColorizer
  { colorizerInUse   :: String
  , colorizerDefault :: String
  , colorizerScope   :: Set String
    -- ^ The names in scope at the end of the viewer's code, which is
    -- where a colorizer chosen without arguments looks for variables
    -- named after its parameters. (Approximately: the viewer's
    -- configuration, coordinate, and top-level declarations.)
  }
  deriving (Eq, Show)

-- | A script, with its colorizers added.
data Colorized = Colorized
  { colorizedStanzas   :: [ParsedStanza]
    -- ^ The script's stanzas, with each viewer's colorizer added, and
    -- without the @\@colorizer@ stanzas.
  , colorizedViewers   :: Map String ViewerColorizer
    -- ^ How each viewer is colorized, by title.
  , colorizedAvailable :: [Colorizer]
    -- ^ Every colorizer the script can use: its own, then the built-in
    -- ones it doesn't replace.
  , colorizedWarnings  :: [String]
  }

-- | Add each viewer's colorizer to a parsed script: its code to the end
-- of the viewer's code, and its fields as a configuration stanza of their
-- own. Takes the interactive choices, by viewer title; one that can't be
-- used (an unknown colorizer, say) gives a warning, and the viewer's
-- default instead.
applyColorizers :: Map String String
                -> [ParsedStanza]
                -> Either (SourceRange, String) Colorized
applyColorizers choices stanzas = do
  let own = mapMaybe fromStanza stanzas
      ownNames = map colorizerName own
  forM_ (ownNames \\ nub ownNames) $ \dup ->
    Left (NoSourceRange, "There are two @colorizer stanzas called " ++ show dup ++ ".")
  let available = own ++ [ c | c <- builtinColorizers, colorizerName c `notElem` ownNames ]
      shadowWarnings =
        [ "the script's colorizer " ++ show (colorizerName c)
          ++ " replaces the built-in one of the same name"
        | c <- builtinColorizers, colorizerName c `elem` ownNames ]

  results <- forM stanzas $ \case
    ViewerStanza{..} -> do
      let fallback = fromMaybe (automaticColorizer vsCode) vsColorizer
          scope = Set.unions
            [ configScope
            , Set.fromList (vsVar : maybeToList vsPixelVar ++ maybe [] (map fst . fst) vsPrepare)
            , topLevelNames vsCode ]
          resolve = resolveChoice available vsFunctions scope
      defaultChoice <- left (\err -> (NoSourceRange, "The `colorizer=` attribute of the viewer "
                                       ++ show vsTitle ++ " can't be used: " ++ err))
                            (resolve fallback)
      let (choice, warnings) = case Map.lookup vsTitle choices of
            Nothing -> (defaultChoice, [])
            Just txt -> case resolve txt of
              Right c -> (c, [])
              Left err -> (defaultChoice, [ "ignoring the saved colorizer " ++ show txt
                                            ++ " for the viewer " ++ show vsTitle ++ ": " ++ err ])
      -- The `coloring:` block is for the viewer's default colorizer: it
      -- has to fit that one, and changes its settings when it's in use.
      configured <- configure (rcColorizer defaultChoice) vsColoring
      let inUse | colorizerName (rcColorizer choice) == colorizerName (rcColorizer defaultChoice)
                  = choice { rcColorizer = (rcColorizer choice) { colorizerConfig = configured } }
                | otherwise = choice
          (code', config) = colorize vsTitle inUse vsCode
          info = ViewerColorizer (rcText choice) (rcText defaultChoice) scope
      pure ([ViewerStanza{vsCode = code', ..}], config, [(vsTitle, info)], warnings)
    ColorizerStanza{} -> pure ([], [], [], [])
    other -> pure ([other], [], [], [])
  let (stanzas', configs, infos, warnings) = unzip4 results
  pure Colorized
    { -- The colorizers' fields come after the script's own (and so
      -- before the implicit "Iteration settings").
      colorizedStanzas = concat stanzas' ++ concat configs
    , colorizedViewers = Map.fromList (concat infos)
    , colorizedAvailable = available
    , colorizedWarnings = shadowWarnings ++ concat warnings
    }
  where
    configScope = Set.fromList (concatMap configNames
      (concat [ body | ConfigStanza _ body <- stanzas ++ [implicitBookkeepingStanza] ]))
    single = length [ () | ViewerStanza{} <- stanzas ] == 1
    tabTitle title = if single then "Coloring" else title ++ " coloring"

    colorize title ResolvedChoice{rcColorizer = colorizer, rcArgs = args} viewerCode =
      case colorizerCode colorizer of
        Nothing -> (viewerCode, [])
        Just body ->
          let hide n = "[colorizer] " ++ title ++ ": " ++ n
              params = colorizerParams colorizer
              fields = concatMap configNames (colorizerConfig colorizer)
              own = Set.toList (Set.fromList (params ++ fields ++ declaredNames body))
              renaming = Map.fromList [ (n, hide n) | n <- own ]
              -- the @common statements the colorizer needs, that the
              -- viewer's code doesn't already have
              common = [ s | s <- colorizerCommon colorizer
                           , maybe True (`Set.notMember` topLevelNames viewerCode) (definedName s) ]
              -- an argument is read where the viewer's code ends, and a
              -- default where the colorizer's own names are
              value = \case
                Given e -> e
                Defaulted e -> rename renaming e
              code = Stmt NoSourceRange $ SBlock $
                common
                ++ [ Stmt NoSourceRange (SDecl (hide p) Nothing (value a)) | (p, a) <- zip params args ]
                ++ [ renameStmt renaming body ]
              config = [ ConfigStanza (tabTitle title)
                           (map (renameConfig hide False) (colorizerConfig colorizer))
                       | not (null (colorizerConfig colorizer)) ]
          in (appendStmt viewerCode code, config)

-- | A colorizer's settings, with the defaults replaced by a viewer's
-- @coloring:@ block's values. A choice's value can be given by its label,
-- as text.
configure :: Colorizer -> [ColorizerSetting] -> Either (SourceRange, String) [ConfigStmt]
configure colorizer settings = do
  let cname = colorizerName colorizer
      known = concatMap configNames (colorizerConfig colorizer)
  forM_ settings $ \ColorizerSetting{..} -> unless (czsName `elem` known) $
    Left (czsRange, "The " ++ cname ++ " colorizer has no setting called `" ++ czsName ++ "`"
                    ++ (if null known then "; it has no settings at all."
                        else "; its settings are " ++ intercalate ", " [ "`" ++ n ++ "`" | n <- known ] ++ "."))
  let byName = Map.fromList [ (czsName c, c) | c <- settings ]
      go = \case
        d@ConfigDecl{cdName}
          | Just ColorizerSetting{..} <- Map.lookup cdName byName -> do
              (value, src) <- case (cdChoices d, textLiteral czsValue) of
                (Just labels, Just label) -> case lookup label (zip labels [0 :: Int ..]) of
                  Nothing -> Left (czsValueRange, "`" ++ cdName ++ "` has no choice called " ++ show label
                                                  ++ "; its choices are "
                                                  ++ intercalate ", " (map show labels) ++ ".")
                  Just i -> pure (Syn czsValueRange (ELit (show i) (SomeHaskellType IntegerType (fromIntegral i))), show i)
                _ -> pure (czsValue, czsValueSrc)
              pure d { cdDefault = value, cdDefaultSrc = src
                     , cdRange = czsRange, cdDefaultRange = czsValueRange }
        ConfigRow stmts -> ConfigRow <$> mapM go stmts
        ConfigTabs tabs -> ConfigTabs <$> mapM (\(t, ss) -> (t,) <$> mapM go ss) tabs
        other -> pure other
  mapM go (colorizerConfig colorizer)

-- | The colorizer a viewer gets when nothing says which: @Manual@ if its
-- code sets @color@, @Rainbow@ if not.
automaticColorizer :: Stmt -> String
automaticColorizer code
  | setsColor code = manualColorizerName
  | otherwise = rainbowColorizerName

-- | Whether code assigns to @color@ anywhere (including in the code
-- blocks it runs).
setsColor :: Stmt -> Bool
setsColor code = case expandRuns code of
  TC (Right expanded) -> go expanded
  TC (Left _) -> go code -- an error that the build will report anyway
  where
    go (Stmt _ node) = case node of
      SBlock body -> any go body
      SSet "color" _ -> True
      SIf _ yes no -> go yes || go no
      SLoop _ _ _ body -> go body
      SForEach _ _ body -> go body
      SWithFirst _ _ _ body fallback -> go body || maybe False go fallback
      _ -> False

-- | Where a parameter's value comes from.
data Argument
  = Given Expr
    -- ^ an argument (or the viewer's variable named after the parameter)
  | Defaulted Expr
    -- ^ the parameter's default value

-- | A colorizer choice, read.
data ResolvedChoice = ResolvedChoice
  { rcColorizer :: Colorizer
  , rcArgs      :: [Argument]
  , rcText      :: String
    -- ^ How it's written, tidied up: @Smooth(z)@, not @ Smooth(z) @.
  }

-- | Read a colorizer choice, as written: a colorizer's name, followed by
-- arguments for its parameters, in parentheses. The arguments are read
-- knowing the user-defined functions in scope where they're used. Any
-- parameters after the arguments given (all of them, without
-- parentheses) get their default values, or else the viewer's variables
-- of the same names, which have to be in scope.
resolveChoice :: [Colorizer] -> Set String -> Set String -> String -> Either String ResolvedChoice
resolveChoice available funs scope txt = do
  let (name, rest) = break (== '(') (trim txt)
  colorizer <- maybe (Left ("there's no colorizer called " ++ show (trim name)
                            ++ " (the colorizers are "
                            ++ intercalate ", " (map colorizerName available) ++ ")"))
                     Right (find ((== trim name) . colorizerName) available)
  let params = colorizerParams colorizer
      defaults = colorizerDefaults colorizer
      cname = colorizerName colorizer
  argTexts <- case trim rest of
    "" -> Right []
    '(' : inner | not (null inner), last inner == ')' ->
      Right (filter (not . null) (splitArguments (init inner)))
    _ -> Left ("the arguments of " ++ cname ++ " should be in parentheses after its name")
  when (length argTexts > length params) $ Left $ case params of
    [] -> cname ++ " doesn't take any arguments"
    [_] -> cname ++ " takes 1 argument"
    _ -> cname ++ " takes " ++ show (length params) ++ " arguments"
  given <- forM argTexts $ \argText ->
    left (\(e :: ParseError) -> unlines (pp e))
         (fmap (const NoSourceRange) <$> parse (argumentGrammarWithFunctions funs) (tokenize argText))
  let rest' = drop (length given) params
      missing = [ p | p <- rest', Map.notMember p defaults, p `Set.notMember` scope ]
  unless (null missing) $
    Left (cname ++ " needs a value for " ++ listNames missing ++ ", and the viewer has no "
          ++ listNames missing ++ " to use: give "
          ++ (if length params == 1 then "it as an argument" else "the arguments")
          ++ " in parentheses after its name")
  let fill p = case Map.lookup p defaults of
        Just (v, _) -> Defaulted v
        Nothing -> Given (Syn NoSourceRange (EVar p))
  pure ResolvedChoice
    { rcColorizer = colorizer
    , rcArgs = map Given given ++ map fill rest'
    , rcText = cname ++ (if null argTexts then "" else "(" ++ intercalate ", " argTexts ++ ")")
    }
  where
    listNames ns = intercalate " or " [ "`" ++ n ++ "`" | n <- ns ]

-- | Split a colorizer's arguments, as written between its parentheses,
-- at the commas between them (not those inside parentheses, brackets, or
-- braces).
splitArguments :: String -> [String]
splitArguments = map trim . go (0 :: Int) ""
  where
    go _ acc [] = [reverse acc]
    go 0 acc (',' : rest) = reverse acc : go 0 "" rest
    go depth acc (c : rest)
      | c `elem` ("([{" :: String) = go (depth + 1) (c : acc) rest
      | c `elem` (")]}" :: String) = go (max 0 (depth - 1)) (c : acc) rest
      | otherwise = go depth (c : acc) rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

------------------------------------------------------------
-- Syntax helpers
------------------------------------------------------------

appendStmt :: Stmt -> Stmt -> Stmt
appendStmt (Stmt sr (SBlock stmts)) s = Stmt sr (SBlock (stmts ++ [s]))
appendStmt body s = Stmt (stmtRange body) (SBlock [body, s])

-- | The names defined at the top level of some code: those in scope at
-- its end.
topLevelNames :: Stmt -> Set String
topLevelNames = \case
  Stmt _ (SBlock stmts) -> Set.fromList (mapMaybe definedName stmts)
  s -> Set.fromList (maybeToList (definedName s))

-- | The variables some code declares, anywhere in it.
declaredNames :: Stmt -> [String]
declaredNames (Stmt _ node) = case node of
  SBlock body -> concatMap declaredNames body
  SDecl n _ _ -> [n]
  SIf _ yes no -> declaredNames yes ++ declaredNames no
  SLoop _ _ _ body -> declaredNames body
  SForEach item _ body -> item : declaredNames body
  SWithFirst item _ _ body fallback -> item : declaredNames body ++ foldMap declaredNames fallback
  SProc _ _ body -> declaredNames body
  _ -> []

configNames :: ConfigStmt -> [String]
configNames = \case
  ConfigDecl{cdName} -> [cdName]
  ConfigRow stmts -> concatMap configNames stmts
  ConfigTabs tabs -> concatMap (concatMap configNames . snd) tabs
  _ -> []

-- | Rename a colorizer's fields (keeping their names as their labels, if
-- they don't have labels already), optionally forgetting where they came
-- from.
renameConfig :: (String -> String) -> Bool -> ConfigStmt -> ConfigStmt
renameConfig f strip = \case
  d@ConfigDecl{cdName} -> d { cdName = f cdName
                            , cdRange = if strip then NoSourceRange else cdRange d
                            , cdDefaultRange = if strip then NoSourceRange else cdDefaultRange d
                            , cdLabel = Just (fromMaybe cdName (cdLabel d)) }
  ConfigRow stmts -> ConfigRow (map (renameConfig f strip) stmts)
  ConfigTabs tabs -> ConfigTabs [ (t, map (renameConfig f strip) ss) | (t, ss) <- tabs ]
  other -> other

-- | Forget where code came from.
stripRanges :: Stmt -> Stmt
stripRanges (Stmt _ node) = Stmt NoSourceRange $ case node of
  SBlock body -> SBlock (map stripRanges body)
  SDecl n t e -> SDecl n t (ex e)
  SSet n e -> SSet n (ex e)
  SFun f ps e -> SFun f ps (ex e)
  SIf c yes no -> SIf (ex c) (stripRanges yes) (stripRanges no)
  SLoop k c lim body -> SLoop k (ex c) (ex <$> lim) (stripRanges body)
  SIterate n e w c lim -> SIterate n (ex e) w (ex c) (ex <$> lim)
  SPass -> SPass
  SDraw d -> SDraw $ case d of
    DrawPointAt p -> DrawPointAt (ex p)
    DrawCircleAt f c r -> DrawCircleAt f (ex c) (ex r)
    DrawRectFrom f a b -> DrawRectFrom f (ex a) (ex b)
    DrawLineFrom a b -> DrawLineFrom (ex a) (ex b)
    UseStroke c -> UseStroke (ex c)
    UseFill c -> UseFill (ex c)
    EraseAll -> EraseAll
    WriteAt t p -> WriteAt (ex t) (ex p)
  SForEach item list body -> SForEach item list (stripRanges body)
  SWithFirst item p list body fallback ->
    SWithFirst item (ex p) list (stripRanges body) (stripRanges <$> fallback)
  SProc name ps body -> SProc name ps (stripRanges body)
  SRun name arg -> SRun name (ex <$> arg)
  where
    ex = fmap (const NoSourceRange)
