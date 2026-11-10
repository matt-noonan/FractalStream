{- |
Module      : Language.Editor.Highlight
Description : What each part of a script is, for coloring it in an editor

A script is split into spans, each with a 'Highlight' class: keywords,
numbers, comments, and so on, and identifiers by what they name -- a
parameter (a @\@config@ or tool field), a variable, a function (@let@), a
code block (@define@) -- or by their not naming anything defined, which
is probably a mistake.

This works from the tokenizer's tokens, not the parsed script, so that it
keeps working while a script is half-written and doesn't parse; so what
names refer to is worked out from the definitions it can see, stanza by
stanza (plus @\@config@ and @\@common@, which every stanza sees), without
tracking scopes exactly. Statement words (@draw@, @while@, @at@, ...)
aren't keywords to the tokenizer -- they can name variables -- so they're
keywords here only when they don't name something the script defines.
-}
module Language.Editor.Highlight
  ( Highlight(..)
  , highlightName
  , highlights
  , highlightsWith
  ) where

import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (listToMaybe, maybeToList)

import Language.Parser.Tokenizer (Token(..), functionDefinitionBody)
import Language.Editor.Lines
import qualified Language.Value.Builtins as Builtins

-- | What a span of a script is.
data Highlight
  = Keyword          -- ^ @if@, @while@, @draw@, @on click@, @escapes@, ...
  | BuiltinFunction  -- ^ @sin@, @re@, @rainbow@, @blend@, @length@, ...
  | BuiltinValue     -- ^ @pi@, @i@, @e@, @true@, @iterations@, @stuck@, @color@
  | TypeName         -- ^ @R@, @C@, @List of Z@, in a type
  | ColorName        -- ^ @red@, @blue@, ...
  | NumberLiteral
  | StringLiteral
  | Comment
  | Operator         -- ^ @+@, @<-@, @->@, @=@, @:=@, ...
  | StanzaHeader     -- ^ a whole @\@viewer ...@ line
  | Parameter        -- ^ a @\@config@ or tool field, or a built-in setting (@max_iters@)
  | Variable
  | Function         -- ^ defined with @let@
  | CodeBlock        -- ^ defined with @define@
  | Undefined        -- ^ an identifier that doesn't name anything the script defines
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The name of a class, as a setting would name it: @"builtin-function"@.
highlightName :: Highlight -> String
highlightName = \case
  Keyword -> "keyword"
  BuiltinFunction -> "builtin-function"
  BuiltinValue -> "builtin-value"
  TypeName -> "type"
  ColorName -> "color"
  NumberLiteral -> "number"
  StringLiteral -> "string"
  Comment -> "comment"
  Operator -> "operator"
  StanzaHeader -> "stanza-header"
  Parameter -> "parameter"
  Variable -> "variable"
  Function -> "function"
  CodeBlock -> "code-block"
  Undefined -> "undefined"

-- | The classified spans of a script, as character offsets (from, and up
-- to but not including, to), in order and not overlapping. What's in
-- none of them (spaces, brackets, commas) has no class.
highlights :: String -> [(Int, Int, Highlight)]
highlights = highlightsWith Map.empty

-- | 'highlights', for a script that can also refer to names defined
-- elsewhere (a movie's script, say, which sets the project's
-- parameters): what they are.
highlightsWith :: Map String Highlight -> String -> [(Int, Int, Highlight)]
highlightsWith known src = concatMap spansOf stanzas
  where
    ls = scriptLines src
    stanzas = splitStanzas ls
    common = commonDefinitions stanzas <> known
    viewerVars = Map.fromList
      [ (title, viewerNames s) | s <- stanzas, stKind s == "viewer"
                               , Just title <- [stTitle s] ]
    -- @common's code, and a colorizer's, is used in other stanzas, as if
    -- written there: so it can refer to what any of them defines
    everyStanza = Map.unions (map (stanzaDefinitions viewerVars) stanzas)
    spansOf s =
      let defs = stanzaDefinitions viewerVars s <> common
                 <> (if stKind s `elem` ["common", "colorizer"] then everyStanza else Map.empty)
      in concatMap (lineSpans s defs) (stLines s)

------------------------------------------------------------------------
-- Lines and stanzas

-- | A stanza: its kind (@"viewer"@, @"tool"@, ...; @""@ for a script with
-- no stanzas, like a single viewer's code), its header line, its title,
-- and its attributes.
data Stanza = Stanza
  { stKind   :: String
  , stTitle  :: Maybe String
  , stAttrs  :: Map.Map String String
  , stHeader :: Maybe Line
  , stLines  :: [Line]    -- ^ including the header
  }

splitStanzas :: [Line] -> [Stanza]
splitStanzas ls = case break isHeader ls of
  (before, rest) -> [ Stanza "" Nothing Map.empty Nothing before | not (null before) ] ++ go rest
  where
    isHeader l = "@" `isPrefixOf` lnText l
    go [] = []
    go (h : rest) =
      let (body, more) = break isHeader rest
          (kind, title, attrs) = parseHeader h
      in Stanza kind title attrs (Just h) (h : body) : go more

-- | A header's kind, title, and @name=value@ attributes.
parseHeader :: Line -> (String, Maybe String, Map.Map String String)
parseHeader l =
  let kind = takeWhile isAlphaNum (drop 1 (lnText l))
      toks = [ t | (_, _, t) <- lnTokens l ]
      title = listToMaybe [ s | Quoted s <- toks ]
      attrs = Map.fromList (go toks)
      go = \case
        Identifier k : Equal : v : rest | Just v' <- value v -> (k, v') : go rest
        _ : rest -> go rest
        [] -> []
      value = \case
        Identifier v -> Just v
        Quoted v -> Just v
        NumberI n -> Just (show n)
        _ -> Nothing
  in (kind, title, attrs)

-- | The names a viewer defines for its code (and its tools'): its
-- coordinate variable (@var=@, by default @z@), or both of a real
-- viewer's (@xvar=@ and @yvar=@), along with a real viewer's pixel width
-- and height, and the viewer's geometry (its size and center).
viewerNames :: Stanza -> [(String, Highlight)]
viewerNames s = case (Map.lookup "xvar" attrs, Map.lookup "yvar" attrs) of
  (Just x, Just y) -> [(x, Variable), (y, Variable), ("pixel_width", Parameter), ("pixel_height", Parameter)]
                      ++ geometry ["viewer_center_x", "viewer_center_y"]
  _ -> (Map.findWithDefault "z" "var" attrs, Variable) : geometry ["viewer_center"]
  where attrs = stAttrs s
        geometry centers = [ (n, Parameter) | n <- "viewer_width" : "viewer_height" : centers ]

------------------------------------------------------------------------
-- Definitions

-- | What names a stanza can see refer to (the first definition seen wins).
type Defs = Map String Highlight

-- | What every stanza sees: @\@config@ fields, @\@common@'s definitions,
-- and FractalStream's own settings.
commonDefinitions :: [Stanza] -> Defs
commonDefinitions stanzas =
  Map.unions [ definitions s | s <- stanzas, stKind s `elem` ["config", "common"] ]
  <> Map.fromList [ (n, Parameter) | n <- ["max_iters", "escape_radius", "vanishing_radius"] ]

-- | A stanza's own definitions, and those it sees because of what it is:
-- a viewer's coordinate and pixel size, a tool's viewer's coordinate.
stanzaDefinitions :: Map.Map String [(String, Highlight)] -> Stanza -> Defs
stanzaDefinitions viewerVars s = definitions s <> implicit
  where
    implicit = Map.fromList $ case stKind s of
      "viewer" -> viewerNames s
                  ++ [(Map.findWithDefault "pixel_size" "pixelVar" (stAttrs s), Parameter)]
      "tool" -> concat (maybeToList toolViewerNames) ++ [("pixel_size", Parameter)]
      "colorizer" -> [("pixel_size", Parameter)]
      "" -> [("z", Variable), ("pixel_size", Parameter)]
      _ -> []
    toolViewerNames = case Map.lookup "for" (stAttrs s) of
      Just title -> Map.lookup title viewerVars
      Nothing -> case Map.elems viewerVars of
        [v] -> Just v
        _ -> Nothing

-- | The names a stanza defines, from the shapes of its lines.
definitions :: Stanza -> Defs
definitions s = Map.fromListWith (\_ old -> old) (concatMap lineDefs (zip [0 :: Int ..] body))
  where
    body = filter (not . null . lnTokens) (drop (maybe 0 (const 1) (stHeader s)) (stLines s))
    -- a tool's fields come before its first handler
    firstHandler = listToMaybe [ k | (k, l) <- zip [0 ..] body, isHandler l ]
    isField k = case stKind s of
      k' | k' `elem` ["config", "colorizer"] -> True
      "tool" -> maybe True (k <) firstHandler
      _ -> False
    lineDefs (k, l) =
      let toks = [ t | (_, _, t) <- lnTokens l ]
      in fieldDef k toks ++ codeDefs toks ++ coloringDefs l
    -- `name : Type ...` (not `row:` or `coloring:`, which have no type)
    fieldDef k = \case
      Identifier n : Colon : _ : _ | isField k -> [(n, Parameter)]
      Identifier n : Colon : _ : _ -> [(n, Variable)]
      _ -> []
    -- settings changed in a viewer's `coloring:` block
    coloringDefs l
      | Just parent <- enclosing l, isWord "coloring" parent
      , (_, _, Identifier n) : _ <- lnTokens l = [(n, Parameter)]
      | otherwise = []
    enclosing l = listToMaybe
      [ p | p <- reverse (takeWhile ((/= lnStart l) . lnStart) body)
          , lnIndent p < lnIndent l ]
    isWord w l = case lnTokens l of
      (_, _, Identifier w') : _ -> w == w'
      _ -> False

-- | Definitions that can appear in any code.
codeDefs :: [Token] -> [(String, Highlight)]
codeDefs toks = case toks of
  -- `f(x, y) := ...` (before tuple declarations, which it also looks like)
  Identifier f : rest@(OpenParen : ps) | Just _ <- functionDefinitionBody rest -> (f, Function) : params (takeWhile (/= CloseParen) ps)
  Identifier "define" : Identifier f : OpenParen : rest -> (f, CodeBlock) : params rest
  Identifier "define" : Identifier f : _ -> [(f, CodeBlock)]
  Identifier "on" : rest -> handlerPoints rest
  Identifier "prepare" : rest -> [ (n, Variable) | (Identifier n, Colon) <- zip rest (drop 1 rest) ]
  -- a tuple declaration, `x, y := ...` or `(x, y) := ...`
  _ | (lhs@(_ : _), ColonEquals : _) <- break (== ColonEquals) toks
    , Comma `elem` lhs, all tupleToken lhs ->
        [ (n, Variable) | Identifier n <- lhs ] ++ everywhere toks
  _ -> everywhere toks
  where
    tupleToken = \case
      Identifier _ -> True
      t -> t `elem` [Comma, OpenParen, CloseParen]
    -- `x := ...`, `x -> ...`, `for each x in`, `with first x matching`
    everywhere = \case
      Identifier n : ColonEquals : rest -> (n, Variable) : everywhere rest
      Identifier n : RightArrow : rest -> (n, Variable) : everywhere rest
      Identifier "each" : Identifier n : rest -> (n, Variable) : everywhere rest
      Identifier "first" : Identifier n : rest -> (n, Variable) : everywhere rest
      _ : rest -> everywhere rest
      [] -> []
    params ts = [ (n, Variable) | (before, Identifier n) <- zip (OpenParen : ts) ts
                                , before `elem` [OpenParen, Comma] ]
    -- `on click at w:`, `at (x, y)`, `on drag from s to w:`
    handlerPoints ts = [ (n, Variable) | (before, Identifier n) <- zip ts (drop 1 ts)
                                       , before `elem` [Identifier "at", Identifier "from", Identifier "to", OpenParen, Comma] ]

-- | A tool handler's first line, @on ...:@.
isHandler :: Line -> Bool
isHandler l = lnIndent l == 0 && case lnTokens l of
  (_, _, Identifier "on") : _ -> True
  _ -> False

------------------------------------------------------------------------
-- Classifying

lineSpans :: Stanza -> Defs -> Line -> [(Int, Int, Highlight)]
lineSpans s defs l
  | Just h <- stHeader s, lnStart h == lnStart l =
      [ (lnStart l, codeEnd, StanzaHeader) | codeEnd > lnStart l ] ++ comment
  | otherwise = tokenSpans ++ comment
  where
    lineEnd = lnStart l + length (lnText l)
    codeEnd = maybe lineEnd id (lnComment l)
    comment = [ (c, lineEnd, Comment) | Just c <- [lnComment l] ]
    toks = lnTokens l
    -- the event words of a handler's first line: up to its point, or ':'
    eventWords
      | isHandler l = Set.fromList
          [ from | (from, _, t) <- takeWhile (\(_, _, t) -> t `notElem` [Identifier "at", Identifier "from", Colon]) toks
                 , t /= Identifier "on" ]
      | otherwise = Set.empty
    tokenSpans =
      [ (from, to, h)
      | ((from, to, t), before) <- zip toks (Nothing : map (\(_, _, t) -> Just t) toks)
      , Just h <- [classify from t before] ]
    classify from t before
      | from `Set.member` eventWords = Just Keyword
      | otherwise = case t of
          Identifier n -> Just (identifier n before)
          _ -> tokenClass t

    identifier n before
      | n `Set.member` typeNames || n == "List", maybe False inTypePosition before = TypeName
      | n == "of", before == Just (Identifier "List") = TypeName
      | n == "x", Just (Identifier b) <- before, b `Set.member` typeNames = TypeName
      -- (a built-in can't be redefined: the parser takes it for the
      -- built-in, and reports the definition)
      | n `Set.member` builtinFunctions = BuiltinFunction
      | n `Map.member` Builtins.colors = ColorName
      | Just h <- Map.lookup n defs = h
      | n `Set.member` keywords = Keyword
      | n == "color" = BuiltinValue
      | n `Set.member` unicodeTypes = TypeName
      | otherwise = Undefined

    -- what a type comes after: `x : R`, `List of C`, `R x R`
    inTypePosition = \case
      Colon -> True
      Identifier "of" -> True
      Identifier "x" -> True
      _ -> False

-- | The class of a token other than an identifier, if it has one.
tokenClass :: Token -> Maybe Highlight
tokenClass = \case
  NumberI _ -> Just NumberLiteral
  NumberF _ -> Just NumberLiteral
  ScientificNotation _ -> Just NumberLiteral
  Quoted _ -> Just StringLiteral
  t | t `elem` [If, Then, Else, Or_, And_, Not_, Escapes, Vanishes, Stops, TimesKeyword] -> Just Keyword
    | t `elem` [Euler, Pi, I, True_, False_, Iterations, Stuck] -> Just BuiltinValue
    | t `elem` [TextKeyword, AppendKeyword, PrependKeyword, JoinKeyword, RemoveKeyword, FindKeyword
               , TransformKeyword, RangeKeyword, LengthKeyword, DiffKeyword] -> Just BuiltinFunction
    | t `elem` [ Plus, Minus, Times, Divide, IntegerDivide, Caret, Bar, Equal, ExactlyEqual
               , NotEqual, GreaterThan, LessThan, GreaterThanOrEqual, LessThanOrEqual
               , ColonEquals, Prime, LeftArrow, RightArrow, At, AtAt ] -> Just Operator
    | otherwise -> Nothing

-- | Words with a meaning of their own in statements, handlers, and
-- @\@config@ fields (unless the script defines them as something else).
keywords :: Set String
keywords = Set.fromList
  [ "while", "until", "repeat", "iterate", "up", "to", "for", "each", "in", "with", "first"
  , "matching", "define", "run", "pass", "draw", "use", "erase", "write", "point", "at"
  , "circle", "filled", "radius", "rectangle", "from", "line", "stroke", "fill", "on"
  , "as", "choice", "button", "row", "tab", "prepare", "coloring" ]

builtinFunctions :: Set String
builtinFunctions = Set.fromList $ concat
  [ Map.keys Builtins.realFunctions, Map.keys Builtins.commonFunctions
  , Map.keys Builtins.complexFunctions, Map.keys Builtins.roundingFunctions
  , Map.keys Builtins.wheels
  , [ "mod", "rgb", "hsv", "hsl", "blend", "cycle", "dark", "light", "invert"
    , "autocolor", "nearest_autocolor", "get_autocolor"
    , "table_size", "table_cell", "table_point" ] ]

typeNames :: Set String
typeNames = Set.fromList (Map.keys Builtins.types)

-- | Type names that can't be anything else.
unicodeTypes :: Set String
unicodeTypes = Set.fromList [ n | n <- Map.keys Builtins.types, any (not . isAlphaNumAscii) n ]
  where isAlphaNumAscii c = isAlphaNum c && c < '\x80'
