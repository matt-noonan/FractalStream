{-# language RecursiveDo, OverloadedStrings #-}
-- | Layer 2 of the unified project-script format: a grammar for a whole
-- project file written as a single script with @\@setup@\/@\@config@\/
-- @\@viewer@\/@\@tool@ stanzas, e.g.
--
-- > @viewer "Mandelbrot Set" var=C size=500x500
-- > z : C <- 0
-- > iterate z -> z^2 + C until z escapes
-- >
-- > @config "Configuration"
-- > maxIters : Z <- 100  as "Iteration limit:"
--
-- This module only turns script text into a list of 'ParsedStanza's -- it
-- does not yet resolve cross-stanza references (a tool's @for=@ attribute,
-- a viewer's @limit=@\/@escape=@\/@vanish=@ attributes naming a config
-- variable) or build any 'Actor.Ensemble.Ensemble'\/'Actor.Viewer.Viewer'\/
-- 'Actor.Tool.Tool' values; that cross-stanza resolution is a later layer.
--
-- Follows the same "one tokenization, one Earley grammar, reuse the
-- existing statement-level grammars" discipline as 'Actor.Event's tool-
-- script grammar: the whole file is tokenized once via
-- 'tokenizeWithIndentation', and each stanza's body reuses
-- 'codeStatementsGrammar' (for @\@viewer@) or 'toolBlocksGrammar' (for
-- @\@tool@) directly, rather than re-tokenizing an extracted substring.
module Actor.Script
  ( ConfigStmt(..)
  , ParsedStanza(..)
  , parseScript
  , scriptGrammar
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Parser
import Language.Parser.Tokenizer
import Language.Parser.SourceRange
import Language.Value.Parser
import Language.Code.Parser (Splices(..), noSplices, codeStatementsGrammar, ParsedCode)
import Actor.Event (toolBlocksGrammar, ParsedToolBlock)
import Actor.Layout (Dimensions(..), parseDimensions)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Read (readMaybe)

------------------------------------------------------------
-- Parsed stanza bodies
------------------------------------------------------------

-- | One statement inside an @\@setup@\/@\@config@ stanza's body: either a
-- variable declaration (which becomes a config\/setup field in the UI), a
-- line of explanatory text, or a plain button.
data ConfigStmt
  = ConfigDecl
      { cdRange   :: SourceRange
      , cdName    :: String
      , cdType    :: SomeType
      , cdDefault :: ParsedValue
      , cdLabel   :: Maybe String
        -- ^ Overrides the default label (the variable name) when present.
      , cdChoices :: Maybe [String]
        -- ^ When present, this declaration is shown as a dropdown with
        -- these choices; only valid when 'cdType' is @ℤ@, in which case
        -- the variable's value is the chosen index.
      , cdUsing   :: Maybe [(String, FSType)]
        -- ^ When present (@using z:C, C:C@), this declaration is a
        -- *formula*: its default-value expression is typechecked against
        -- this environment instead of the empty one, and the formula's
        -- free variables are left unresolved so it can be spliced (via
        -- @«name»@) into a @\@viewer@\/@\@tool@ body that binds them.
      }
  | ConfigText String
  | ConfigButton String
  | ConfigRow [ConfigStmt]
    -- ^ An indented @row:@ block; its statements are laid out side by
    -- side instead of stacked vertically.

-- | One stanza of a unified project script, with its body already parsed
-- but not yet resolved against any other stanza.
data ParsedStanza
  = SetupStanza
      { ssTitle :: String
      , ssBody  :: [ConfigStmt]
      }
  | ConfigStanza
      { csTitle :: String
      , csBody  :: [ConfigStmt]
      }
  | ViewerStanza
      { vsTitle       :: String
      , vsVar         :: String
        -- ^ The name of the viewer's complex coordinate variable.
      , vsSize        :: Dimensions
      , vsCenter      :: Maybe String
      , vsPixelSize   :: Maybe String
      , vsIterLimit   :: Maybe String
      , vsEscape      :: Maybe String
      , vsVanish      :: Maybe String
        -- ^ Each of these, when present, is a raw expression source
        -- string (not yet parsed\/resolved), exactly mirroring how
        -- 'Actor.Viewer.Complex.ComplexViewer's YAML codec defers parsing
        -- its own @escape-radius@\/@vanishing-radius@\/@iteration-limit@
        -- fields.
      , vsPixelVar    :: Maybe String
        -- ^ The name of a variable to bind to the per-pixel step size, if
        -- given (mirrors the classic YAML's @pixel-size:@ key).
      , vsResizable   :: Bool
      , vsPosition    :: Dimensions
      , vsCode        :: ParsedCode
      }
  | ToolStanza
      { tsName     :: String
      , tsFor      :: Maybe String
        -- ^ The title of the viewer this tool attaches to, if given.
      , tsShortcut :: String
      , tsIcon     :: Maybe String
      , tsDrawLayer          :: Int
      , tsRefreshOnActivate  :: Bool
      , tsRefreshCanUpdate   :: Bool
      , tsConfig   :: [ConfigStmt]
        -- ^ Declarations at the top of the stanza, before any @on ...:@
        -- block, become this tool's own configuration panel -- the same
        -- way a @\@config@ stanza's declarations do.
      , tsBlocks   :: [ParsedToolBlock]
      }

------------------------------------------------------------
-- Entry point
------------------------------------------------------------

-- | Parse a whole unified project script into its stanzas. Does not
-- resolve cross-stanza references (a tool's @for=@, a viewer's
-- @limit=@\/@escape=@\/@vanish=@); see the module header.
--
-- Internally, this is a two-pass parse over one tokenization: @\@setup@
-- stanzas are parsed first (against no splices -- a setup field can't
-- reference another setup field), to build the real 'Splices' map that
-- their own declarations make available; then every other stanza is
-- parsed against that map, so a @\@viewer@\/@\@tool@ body can reference a
-- setup-declared name via @«name»@. This has to be two passes over two
-- separate 'parse' calls, not one 'mdo'-tied-together grammar: deciding
-- whether a `«name»` token is even a valid splice reference is a
-- *recognition*-time question for Earley (it needs a yes\/no answer to
-- build its chart), not a lazily-deferred *semantic-value* one, so it
-- can't be answered by a splices map that depends on the very parse
-- result it's deciding inside of.
parseScript :: String -> Either (SourceRange, String) [ParsedStanza]
parseScript input = do
  let allToks = tokenizeWithIndentation input
      indexed = zip [(0 :: Int) ..] (splitStanzas allToks)
      setupChunks = [ ic | ic@(_, c) <- indexed, isSetupChunk c ]
      otherChunks = [ ic | ic@(_, c) <- indexed, not (isSetupChunk c) ]

  setupResults <- traverse (\(i, c) -> (i,) <$> parseChunk noSplices c) setupChunks

  let setupBodies = [ b | (_, SetupStanza _ b) <- setupResults ]
      splices     = buildSplicesFromSetups setupBodies

  otherResults <- traverse (\(i, c) -> (i,) <$> parseChunk splices c) otherChunks

  pure (map snd (sortOn fst (setupResults ++ otherResults)))

-- | Parse one stanza's own token span (as produced by 'splitStanzas',
-- without the enclosing file-level Indent\/Dedent) into its 'ParsedStanza'.
parseChunk :: Splices -> [SRToken] -> Either (SourceRange, String) ParsedStanza
parseChunk splices chunk =
  case parse (scriptGrammar splices) wrapped of
    Left err        -> Left (errorLocation &&& unlines . pp $ err)
    Right [estanza] -> estanza
    Right _         -> Left (NoSourceRange,
      "INTERNAL ERROR: Actor.Script.parseChunk was given a token span containing other than exactly one stanza.")
  where
    wrapped = SRToken Indent NoSourceRange : chunk ++ [SRToken Dedent NoSourceRange]

-- | Does this stanza's own token span (as produced by 'splitStanzas')
-- start with @\@setup@?
isSetupChunk :: [SRToken] -> Bool
isSetupChunk (_ : kw : _) = baseToken kw == Identifier "setup"
isSetupChunk _            = False

-- | Split a unified script's token stream (as produced by
-- 'tokenizeWithIndentation', which wraps the whole file in one outer
-- Indent\/Dedent pair) into one token span per stanza, dropping that
-- outer pair. Each span starts right at a top-level, line-initial \@
-- token and runs up to (but not including) the next one. This slices
-- the already-tokenized stream; it never re-tokenizes text.
--
-- Both conditions on where a stanza boundary can occur matter: a mid-
-- expression \@ (list indexing, e.g. @result \<- myList \@ 0@) sits at
-- the same *depth* as a stanza header, so depth alone isn't enough to
-- tell them apart -- it also has to be the first token on its line.
splitStanzas :: [SRToken] -> [[SRToken]]
splitStanzas allToks = case allToks of
  (i : rest@(_ : _))
    | baseToken i == Indent, baseToken (last rest) == Dedent ->
        reverse (go 0 True [] [] (init rest))
  _ -> [allToks]
  where
    go :: Int -> Bool -> [SRToken] -> [[SRToken]] -> [SRToken] -> [[SRToken]]
    go _ _ cur acc [] = if null cur then acc else reverse cur : acc
    go depth atLineStart cur acc (t : ts)
      | depth == 0, atLineStart, baseToken t == At, not (null cur) =
          go 0 False [t] (reverse cur : acc) ts
      | otherwise =
          go depth' atLineStart' (t : cur) acc ts
      where
        -- Indent/Dedent are zero-width, virtual tokens (indentation
        -- changes, not real line content), so they must *preserve*
        -- whatever line-start status came before them rather than reset
        -- it -- otherwise the first real token after a nested block
        -- closes (e.g. the next stanza's own `@`, right after an `if`/
        -- `on`/... block's closing Dedent) is wrongly seen as non-line-
        -- initial, and a real stanza boundary gets missed.
        atLineStart' = case baseToken t of
          Newline -> True
          Indent  -> atLineStart
          Dedent  -> atLineStart
          _       -> False
        depth' = case baseToken t of
          Indent -> depth + 1
          Dedent -> depth - 1
          _      -> depth

-- | Build the splices map that every @\@setup@ stanza's own declarations
-- make available to the rest of the script, recursing into @row:@
-- blocks. A declaration's raw, not-yet-resolved 'ParsedValue' is what's
-- stored -- 'ParsedValue' is already deferred over both the environment
-- and the type it's checked at, so the same splice works whether it's
-- referenced somewhere expecting its "natural" type or needing a cast,
-- and whether the reference site's environment is empty or not (that's
-- what makes a `using`-clause formula field spliceable at all).
buildSplicesFromSetups :: [[ConfigStmt]] -> Splices
buildSplicesFromSetups stmtLists = Splices
  { codeSplices  = Map.empty
  , valueSplices = Map.fromList
      [ (cdName, cdDefault) | stmts <- stmtLists, ConfigDecl{..} <- concatMap flatten stmts ]
  }
  where
    flatten = \case
      ConfigRow rs -> concatMap flatten rs
      s            -> [s]

------------------------------------------------------------
-- Top-level grammar
------------------------------------------------------------

scriptGrammar :: forall r. Splices -> Grammar r (Prod r [Either (SourceRange, String) ParsedStanza])
scriptGrammar splices@Splices{valueSplices} = mdo

  -- Reuse the existing statement-level grammars for stanza bodies, rather
  -- than re-deriving assignment\/loop\/draw-command syntax here.
  viewerBody <- codeStatementsGrammar splices
  toolBody   <- toolBlocksGrammar splices

  value <- valueGrammar valueSplices
  ty    <- typeGrammar

  colon <- rule (token Colon <?> ":")
  nl    <- rule (token Newline <?> "end of line")
  quotedP <- rule (tokenMatch (\case { Quoted s -> Just s; _ -> Nothing }))

  ------------------------------------------------------------
  -- Headers
  ------------------------------------------------------------

  attrValue <- rule (quotedP <|> ident)

  attr <- rule ((,) <$> ident <*> (token Equal *> attrValue))

  attrs <- rule (many attr)

  setupHeader <- rule $
    withSourceRange $
      (\title as sr -> validateNoAttrs sr as >> pure title)
      <$> (token At *> tok "setup" *> quotedP) <*> (attrs <* nl)

  configHeader <- rule $
    withSourceRange $
      (\title as sr -> validateNoAttrs sr as >> pure title)
      <$> (token At *> tok "config" *> quotedP) <*> (attrs <* nl)

  viewerHeader <- rule $
    withSourceRange $
      (\title as sr -> validateViewerHeader sr title as)
      <$> (token At *> tok "viewer" *> quotedP) <*> (attrs <* nl)

  toolHeader <- rule $
    withSourceRange $
      (\name as sr -> validateToolHeader sr name as)
      <$> (token At *> tok "tool" *> quotedP) <*> (attrs <* nl)

  ------------------------------------------------------------
  -- @setup@\/@config@ stanza bodies: variable declarations, text, buttons
  ------------------------------------------------------------

  choiceList <- rule $
    token OpenBrace *>
    ((:) <$> quotedP <*> many (token Comma *> quotedP))
    <* token CloseBrace

  -- NOTE: `as "label"`, `choice { ... }`, and `using <env>` all come
  -- *before* the `<-`, so the default-value expression is always the
  -- last thing on the line (immediately followed by `nl`), exactly like
  -- an ordinary `Let` statement's value. Putting them after the value
  -- instead would be genuinely ambiguous: since plain identifiers like
  -- `as`/`choice`/`using` are not reserved words, `value`'s own grammar
  -- is happy to treat e.g. `0 as "L"` as the implicit-multiplication/
  -- concatenation of `0`, the bare variable `as`, and the text literal
  -- `"L"` -- a second, equally valid parse of the same tokens.
  usingEnv <- envGrammar

  configDecl <- rule $
    withSourceRange $
      (\name fsTy mlabel mchoices museEnv val sr ->
          validateConfigDecl sr name fsTy val mlabel mchoices museEnv)
      <$> ident
      <*> (colon *> ty)
      <*> optional (tok "as" *> quotedP)
      <*> optional (tok "choice" *> choiceList)
      <*> optional (tok "using" *> usingEnv)
      <*> (token LeftArrow *> value <* nl)

  configText <- rule $
    (\s -> Right (ConfigText s)) <$> (token TextKeyword *> quotedP <* nl)

  configButton <- rule $
    (\s -> Right (ConfigButton s)) <$> (tok "button" *> quotedP <* nl)

  -- An indented `row:` block, the same shape as `on <event>:`/`if ...:`
  -- introduce an indented block elsewhere in this language -- its
  -- statements lay out side by side instead of stacked vertically.
  configRow <- rule $
    (\estmts -> ConfigRow <$> sequence estmts) <$>
    (tok "row" *> colon *> nl *> token Indent *> many configStmt <* token Dedent)

  configStmt <- ruleChoice [configDecl, configText, configButton, configRow]

  ------------------------------------------------------------
  -- Stanzas
  ------------------------------------------------------------

  setupStanza <- rule $
    (\etitle estmts -> SetupStanza <$> etitle <*> sequence estmts)
    <$> setupHeader <*> many configStmt

  configStanza <- rule $
    (\etitle estmts -> ConfigStanza <$> etitle <*> sequence estmts)
    <$> configHeader <*> many configStmt

  viewerStanza <- rule $
    (\ehdr code -> (\ViewerHeader{..} ->
        ViewerStanza vhTitle vhVar vhSize vhCenter vhPixelSize vhIterLimit vhEscape vhVanish
                     vhPixelVar vhResizable vhPosition code) <$> ehdr)
    <$> viewerHeader <*> viewerBody

  toolStanza <- rule $
    (\ehdr edecls blocks -> (\ToolHeader{..} decls ->
        ToolStanza thName thFor thShortcut thIcon
                   thDrawLayer thRefreshOnActivate thRefreshCanUpdate decls blocks)
        <$> ehdr <*> sequence edecls)
    <$> toolHeader <*> many configStmt <*> toolBody

  stanza <- ruleChoice [setupStanza, configStanza, viewerStanza, toolStanza]

  rule ((token Indent *> many stanza <* token Dedent) <?> "a unified project script")

------------------------------------------------------------
-- Header attribute validation
------------------------------------------------------------

validateNoAttrs :: SourceRange -> [(String, String)] -> Either (SourceRange, String) ()
validateNoAttrs sr = checkKnownAttrs sr []

-- | Check that every attribute name is one of the given known names (with
-- no duplicates), and return the attribute map on success.
checkKnownAttrs :: SourceRange -> [String] -> [(String, String)] -> Either (SourceRange, String) ()
checkKnownAttrs sr known attrs = do
  forM_ attrs $ \(k, _) ->
    unless (k `elem` known) $
      Left (sr, "Unknown attribute `" ++ k ++ "`" ++
            (if null known then "; this header takes no attributes."
             else "; expected one of " ++ intercalate ", " known ++ "."))
  let seen = map fst attrs
  go Set.empty seen
  where
    go _ [] = Right ()
    go seen (k:ks)
      | k `Set.member` seen = Left (sr, "Duplicate attribute `" ++ k ++ "`.")
      | otherwise            = go (Set.insert k seen) ks

lookupAttr :: String -> [(String, String)] -> Maybe String
lookupAttr = lookup

requireAttr :: SourceRange -> String -> [(String, String)] -> Either (SourceRange, String) String
requireAttr sr k attrs = case lookupAttr k attrs of
  Just v  -> Right v
  Nothing -> Left (sr, "Missing required attribute `" ++ k ++ "`.")

data ViewerHeader = ViewerHeader
  { vhTitle     :: String
  , vhVar       :: String
  , vhSize      :: Dimensions
  , vhCenter    :: Maybe String
  , vhPixelSize :: Maybe String
  , vhIterLimit :: Maybe String
  , vhEscape    :: Maybe String
  , vhVanish    :: Maybe String
  , vhPixelVar  :: Maybe String
  , vhResizable :: Bool
  , vhPosition  :: Dimensions
  }

data ToolHeader = ToolHeader
  { thName    :: String
  , thFor     :: Maybe String
  , thShortcut :: String
  , thIcon    :: Maybe String
  , thDrawLayer         :: Int
  , thRefreshOnActivate :: Bool
  , thRefreshCanUpdate  :: Bool
  }

-- | Parse a `true`\/`false` attribute value, the same spelling the
-- language itself uses for boolean literals.
parseBoolAttr :: SourceRange -> String -> String -> Either (SourceRange, String) Bool
parseBoolAttr sr k = \case
  "true"  -> Right True
  "false" -> Right False
  v -> Left (sr, "The `" ++ k ++ "` attribute must be `true` or `false`, not `" ++ v ++ "`.")

-- | Parse an integer-valued attribute (always quoted, since a bare
-- numeral doesn't tokenize as a single identifier -- see @size=@'s
-- treatment of @WxH@ for the same reason).
parseIntAttr :: SourceRange -> String -> String -> Either (SourceRange, String) Int
parseIntAttr sr k v = case readMaybe v of
  Just n  -> Right n
  Nothing -> Left (sr, "The `" ++ k ++ "` attribute must be a whole number, not `" ++ v ++ "`.")

optionalBoolAttr :: SourceRange -> String -> Bool -> [(String, String)] -> Either (SourceRange, String) Bool
optionalBoolAttr sr k def attrs = maybe (Right def) (parseBoolAttr sr k) (lookupAttr k attrs)

optionalDimensionsAttr :: SourceRange -> String -> Dimensions -> [(String, String)]
                        -> Either (SourceRange, String) Dimensions
optionalDimensionsAttr sr k def attrs =
  maybe (Right def) (left (sr,) . parseDimensions) (lookupAttr k attrs)

validateViewerHeader :: SourceRange
                      -> String
                      -> [(String, String)]
                      -> Either (SourceRange, String) ViewerHeader
validateViewerHeader sr vhTitle attrs = do
  checkKnownAttrs sr
    ["var", "size", "center", "pixelSize", "limit", "escape", "vanish", "pixelVar", "resizable", "position"]
    attrs
  vhVar      <- requireAttr sr "var" attrs
  vhSize     <- requireAttr sr "size" attrs >>= left (sr,) . parseDimensions
  vhResizable <- optionalBoolAttr sr "resizable" True attrs
  vhPosition  <- optionalDimensionsAttr sr "position" (Dimensions (100, 100)) attrs
  pure ViewerHeader
    { vhTitle, vhVar, vhSize, vhResizable, vhPosition
    , vhCenter    = lookupAttr "center" attrs
    , vhPixelSize = lookupAttr "pixelSize" attrs
    , vhIterLimit = lookupAttr "limit" attrs
    , vhEscape    = lookupAttr "escape" attrs
    , vhVanish    = lookupAttr "vanish" attrs
    , vhPixelVar  = lookupAttr "pixelVar" attrs
    }

validateToolHeader :: SourceRange
                    -> String
                    -> [(String, String)]
                    -> Either (SourceRange, String) ToolHeader
validateToolHeader sr thName attrs = do
  checkKnownAttrs sr ["for", "shortcut", "icon", "drawLayer", "refreshOnActivate", "refreshCanUpdate"] attrs
  thDrawLayer <- maybe (Right 100) (parseIntAttr sr "drawLayer") (lookupAttr "drawLayer" attrs)
  thRefreshOnActivate <- optionalBoolAttr sr "refreshOnActivate" True attrs
  thRefreshCanUpdate  <- optionalBoolAttr sr "refreshCanUpdate" False attrs
  pure ToolHeader
    { thName, thDrawLayer, thRefreshOnActivate, thRefreshCanUpdate
    , thFor      = lookupAttr "for" attrs
    , thShortcut = fromMaybe "" (lookupAttr "shortcut" attrs)
    , thIcon     = lookupAttr "icon" attrs
    }

------------------------------------------------------------
-- Config\/setup declaration validation
------------------------------------------------------------

validateConfigDecl :: SourceRange
                    -> String
                    -> FSType
                    -> ParsedValue
                    -> Maybe String
                    -> Maybe [String]
                    -> Maybe [(String, FSType)]
                    -> Either (SourceRange, String) ConfigStmt
validateConfigDecl sr name fsTy val mlabel mchoices museEnv = do
  case mchoices of
    Just _ | fsTy /= IntegerT ->
      Left (sr, "`choice { ... }` can only be used with a ℤ-typed declaration (`" ++
                name ++ "` has type " ++ withType fsTy ppType ++ ").")
    _ -> Right ()
  case (mchoices, museEnv) of
    (Just _, Just _) ->
      Left (sr, "`" ++ name ++ "` cannot use both `choice { ... }` and `using`.")
    _ -> Right ()
  case museEnv of
    Just env | Just dup <- firstDuplicate (map fst env) ->
      Left (sr, "`" ++ name ++ "`'s `using` clause declares `" ++ dup ++ "` more than once.")
    _ -> Right ()
  pure (ConfigDecl sr name (withType fsTy SomeType) val mlabel mchoices museEnv)

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise           = go (Set.insert x seen) xs
