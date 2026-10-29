{-# language RecursiveDo, OverloadedStrings #-}
-- | Layer 2 of the unified project-script format: a grammar for a whole
-- project file written as a single script with @\@config@\/@\@common@\/
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
  , implicitBookkeepingStanza
  , scriptGrammar
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Parser
import Language.Parser.Tokenizer
import Language.Parser.SourceRange
import Language.Value.Parser
import Language.Code.Parser (CommonDefs(..), noCommonDefs, codeStatementsGrammar, ParsedCode, withDefinedFunctions, withCommon)
import Language.Code.Syntax (Stmt(..), StmtF(..))
import Actor.Event (toolBlocksGrammar, ParsedToolBlock)
import Actor.Layout (Dimensions(..), parseDimensions)

import qualified Data.Set as Set
import Text.Read (readMaybe)
import Data.Char (isDigit)
import Numeric (showFFloat)

------------------------------------------------------------
-- Parsed stanza bodies
------------------------------------------------------------

-- | One statement inside an @\@config@ stanza's body (or at the top of a
-- @\@tool@ stanza): either a variable declaration (which becomes a config
-- field in the UI), a line of explanatory text, a plain button, or a row
-- of these.
data ConfigStmt
  = ConfigDecl
      { cdRange   :: SourceRange
      , cdName    :: String
      , cdType    :: SomeType
      , cdDefault :: ParsedValue
      , cdDefaultRange :: SourceRange
        -- ^ The span of just the @<- value@ expression, not the whole
        -- declaration (which 'cdRange' already covers). Only used
        -- transiently, to fill in 'cdDefaultSrc' below -- see
        -- 'fillDefaultSrcs'.
      , cdDefaultSrc :: String
        -- ^ The raw text of the @<- value@ expression, exactly as the
        -- user wrote it -- empty until 'parseScript's own
        -- 'fillDefaultSrcs' post-processing pass fills it in (the
        -- grammar itself has no access to the original string, only
        -- to already-tokenized input). Lets a caller display\/re-edit
        -- the field's own text, unlike 'cdDefault', which is
        -- already-parsed and no longer textual.
      , cdLabel   :: Maybe String
        -- ^ Overrides the default label (the variable name) when present.
      , cdChoices :: Maybe [String]
        -- ^ When present, this declaration is shown as a dropdown with
        -- these choices; only valid when 'cdType' is @ℤ@, in which case
        -- the variable's value is the chosen index.
      }
  | ConfigText String
  | ConfigButton String
  | ConfigRow [ConfigStmt]
    -- ^ An indented @row:@ block; its statements are laid out side by
    -- side instead of stacked vertically.

-- | One stanza of a unified project script, with its body already parsed
-- but not yet resolved against any other stanza.
data ParsedStanza
  = ConfigStanza
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
      , vsPositionGiven :: Bool
        -- ^ 'False' when @position=@ was omitted, so 'vsPosition' is
        -- only a placeholder ('parseScript's
        -- own 'assignViewerPositions' pass replaces it with a value
        -- staggered against this script's other un-positioned
        -- viewers, same timing as 'fillDefaultSrcs').
      , vsPrepare     :: Maybe ([(String, SomeType)], ParsedCode)
        -- ^ An optional @prepare name1 : Type1, name2 : Type2, ...:@
        -- block, naming this viewer's preparation-script output
        -- variables and their types (mirroring the classic YAML
        -- @preparation: outputs:@ list), followed by the block's own
        -- code to compute them. 'Nothing' when the viewer has no
        -- preparation script at all.
      , vsCode        :: ParsedCode
      }
  | CommonStanza
      { cmBody :: ParsedCode
        -- ^ Declarations and function definitions to prepend to every
        -- viewer and tool script (see 'Language.Code.Parser.withCommon').
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
-- @\@common@ stanzas are parsed first, since their statements (and the
-- names of the functions they define) have to be known to parse the other
-- stanzas' scripts. This has to be two separate 'parse' calls, not one
-- 'mdo'-tied-together grammar: whether an identifier is a function name
-- is a *recognition*-time question for Earley (it needs a yes\/no answer
-- to build its chart), so it can't depend on the very parse result it's
-- deciding inside of.
parseScript :: String -> Either (SourceRange, String) [ParsedStanza]
parseScript input = do
  let chunks = zip [(0 :: Int) ..] (splitStanzas (tokenizeWithIndentation input))
      commonChunks = [ ic | ic@(_, c) <- chunks, isChunk "common" c ]
      restChunks   = [ ic | ic@(_, c) <- chunks, not (isChunk "common" c) ]
      -- every stanza can call the functions defined in any @common stanza
      defs0 = withDefinedFunctions (concatMap snd commonChunks) noCommonDefs
  commonResults <- traverse (\(i, c) -> (i,) <$> parseChunk defs0 c) commonChunks
  let common = concat [ stmts | (_, CommonStanza (Stmt _ (SBlock stmts))) <- commonResults ]
      defs = defs0 { commonCode = common }
  restResults <- traverse (\(i, c) -> (i,) <$> parseChunk defs c) restChunks
  pure (assignViewerPositions (fillDefaultSrcs input (map snd (sortOn fst (commonResults ++ restResults)))))

-- | A synthesized @\@config@ stanza providing `max_iters`\/`escape_radius`\/
-- `vanishing_radius`, meant to be appended to a parsed script's own stanzas
-- by 'Actor.Ensemble.buildEnsembleFromScript'\/'Actor.Ensemble.checkScript'
-- (not by 'parseScript' itself, which stays a pure grammar-level parse
-- with no semantic additions -- 'Actor.Script.ScriptSpec's own tests
-- check its *exact* stanza shape), so a project gets a real,
-- editable, auto-generated panel for these without the script author
-- declaring or wiring anything. A viewer's own `limit=`\/`escape=`\/
-- `vanish=` header attributes default to referencing these names now
-- (see 'validateViewerHeader'), rather than falling back to a
-- hard-coded constant, unless explicitly overridden to point at a
-- different variable. A script that declares its own `max_iters` (or
-- the other two) collides with this exactly like any other duplicate
-- config name -- no special-casing needed.
implicitBookkeepingStanza :: ParsedStanza
implicitBookkeepingStanza = ConfigStanza "Iteration settings"
  [ implicitDecl "max_iters"        IntegerT "Iteration limit: "  "100"
  , implicitDecl "escape_radius"    RealT    "Escape radius: "    "10"
  , implicitDecl "vanishing_radius" RealT    "Vanishing radius: " "0.0001"
  ]
  where
    implicitDecl :: String -> FSType -> String -> String -> ConfigStmt
    implicitDecl cdName fsTy label defaultSrc = ConfigDecl
      { cdRange = NoSourceRange
      , cdName
      , cdType = withType fsTy SomeType
      , cdDefault = either (error . ("INTERNAL ERROR: implicitBookkeepingStanza: " ++) . show)
                           id (parseParsedValue defaultSrc)
      , cdDefaultRange = NoSourceRange
      , cdDefaultSrc = defaultSrc
      , cdLabel = Just label
      , cdChoices = Nothing
      }

-- | Fill in every 'ConfigDecl's 'cdDefaultSrc' (the raw text of its own
-- @<- value@ expression), sliced out of the original script via
-- 'cdDefaultRange'. A post-processing
-- pass over the whole parse result, rather than something the grammar
-- does directly, because the grammar operates purely over
-- already-tokenized input -- it never has the original string in hand
-- to slice from.
fillDefaultSrcs :: String -> [ParsedStanza] -> [ParsedStanza]
fillDefaultSrcs input = map onStanza
  where
    onStanza = \case
      ConfigStanza t b -> ConfigStanza t (map onStmt b)
      t@ToolStanza{..} -> t { tsConfig = map onStmt tsConfig }
      v@ViewerStanza{} -> v
      c@CommonStanza{} -> c

    onStmt = \case
      d@ConfigDecl{..} -> d { cdDefaultSrc = sliceLine input cdDefaultRange }
      ConfigRow stmts  -> ConfigRow (map onStmt stmts)
      other            -> other

    sliceLine :: String -> SourceRange -> String
    sliceLine src sr = case spanOfSourceRange sr of
      Just (InLine i s e) -> case drop i (lines src) of
        (line:_) -> take (e + 1 - s) (drop s line)
        []       -> ""
      _ -> ""

-- | Stagger the placeholder @100x100@ position 'validateViewerHeader'
-- gave every @\@viewer@ that omitted its own @position=@, so that
-- several such viewers don't all open stacked exactly on top of one
-- another: the first keeps @100x100@, the second becomes @130x130@,
-- the third @160x160@, and so on, 30 pixels further each time. A
-- viewer with an explicit @position=@ ('vsPositionGiven') is left
-- alone and doesn't consume a step in that sequence, so an explicit
-- viewer sitting between two un-positioned ones doesn't shift their
-- stagger.
assignViewerPositions :: [ParsedStanza] -> [ParsedStanza]
assignViewerPositions = go (0 :: Int)
  where
    go _ [] = []
    go n (v@ViewerStanza{vsPositionGiven = False} : rest) =
      v { vsPosition = Dimensions (100 + 30 * n, 100 + 30 * n) } : go (n + 1) rest
    go n (s : rest) = s : go n rest

-- | Parse one stanza's own token span (as produced by 'splitStanzas',
-- without the enclosing file-level Indent\/Dedent) into its 'ParsedStanza'.
parseChunk :: CommonDefs -> [SRToken] -> Either (SourceRange, String) ParsedStanza
parseChunk defs chunk =
  case parse (scriptGrammar (withDefinedFunctions chunk defs)) wrapped of
    Left err        -> Left (errorLocation &&& unlines . pp $ err)
    Right [estanza] -> estanza
    Right _         -> Left (NoSourceRange,
      "INTERNAL ERROR: Actor.Script.parseChunk was given a token span containing other than exactly one stanza.")
  where
    wrapped = SRToken Indent NoSourceRange : chunk ++ [SRToken Dedent NoSourceRange]

-- | Does this stanza's own token span start with @\@@ and the given keyword?
isChunk :: String -> [SRToken] -> Bool
isChunk name (_ : kw : _) = baseToken kw == Identifier name
isChunk _ _               = False

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

------------------------------------------------------------
-- Top-level grammar
------------------------------------------------------------

scriptGrammar :: forall r. CommonDefs -> Grammar r (Prod r [Either (SourceRange, String) ParsedStanza])
scriptGrammar defs = mdo

  -- Reuse the existing statement-level grammars for stanza bodies, rather
  -- than re-deriving assignment\/loop\/draw-command syntax here.
  viewerBody <- codeStatementsGrammar defs
  toolBody   <- toolBlocksGrammar defs

  value <- valueGrammar
  ty    <- typeGrammar

  colon <- rule (token Colon <?> ":")
  nl    <- rule (token Newline <?> "end of line")
  quotedP <- rule (tokenMatch (\case { Quoted s -> Just s; _ -> Nothing }))

  ------------------------------------------------------------
  -- Headers
  ------------------------------------------------------------

  -- An attribute's value is usually a quoted string or a bare
  -- identifier (`pixelVar=px`), but several common shapes can also be
  -- written unquoted, since none of them need any help from quotes to
  -- know where the value ends -- each is a small, fixed-length run of
  -- tokens that can never be confused with the start of the next
  -- `name=value` pair (Earley explores every alternative in parallel,
  -- but only the interpretation that leaves no stray tokens before the
  -- next attribute or the header's closing newline survives to a full
  -- parse): a dimension (`size=600x600`), a signed whole number or
  -- decimal (`center=-0.5`), a fraction of either
  -- (`pixelSize=1/128`), and a boolean (`resizable=false`).
  --
  -- A decimal literal is reconstructed via 'showFFloat' (fixed-point
  -- *by construction*), never via plain `show` on a `Double`, which
  -- can render in scientific notation (e.g. `show (0.0001 :: Double)
  -- == "1.0e-4"`) -- the exact landmine `toUserString` already exists
  -- to dodge for `@config` defaults, and just as fatal here: `e` is
  -- the Euler's-number keyword, so a round-tripped "1.0e-4" would
  -- silently re-parse as `1.0 * e - 4`, not the original value, if it
  -- parsed at all. A whole number reconstructs via plain `show` on
  -- the `Integer` instead, which never uses scientific notation.
  numberLiteral <- rule (tokenMatch (\case
    NumberI n -> Just (Right n)
    NumberF f -> Just (Left f)
    _ -> Nothing))

  signedNumber <- rule
    ((showNumberLiteral . negateNumberLiteral <$> (token Minus *> numberLiteral))
     <|> (showNumberLiteral <$> numberLiteral))

  fractionValue <- rule
    ((\n d -> n ++ "/" ++ d) <$> signedNumber <*> (token Divide *> signedNumber))

  -- Dimensions (`size=`/`position=`) are always whole, non-negative
  -- pixel counts -- parsed downstream by `parseDimensions`, which
  -- never accepts a decimal -- so this deliberately doesn't go
  -- through `numberLiteral`/`signedNumber` above.
  dimWidth <- rule (tokenMatch (\case { NumberI n -> Just n; _ -> Nothing }))

  dimensionValue <- rule
    ((\w h -> show w ++ "x" ++ h) <$> dimWidth <*> tokenMatch (\case
        Identifier ('x':ds@(_:_)) | all isDigit ds -> Just ds
        _ -> Nothing))

  boolValue <- rule (("true" <$ token True_) <|> ("false" <$ token False_))

  attrValue <- rule
    (quotedP <|> dimensionValue <|> fractionValue <|> boolValue <|> signedNumber <|> ident)

  attr <- rule ((,) <$> ident <*> (token Equal *> attrValue))

  attrs <- rule (many attr)

  configHeader <- rule $
    withSourceRange $
      (\title as sr -> validateNoAttrs sr as >> pure title)
      <$> (token At *> tok "config" *> quotedP) <*> (attrs <* nl)

  viewerHeader <- rule $
    withSourceRange $
      (\title as sr -> validateViewerHeader sr title as)
      <$> (token At *> tok "viewer" *> quotedP) <*> (attrs <* nl)

  commonHeader <- rule $
    withSourceRange $
      (\as sr -> validateNoAttrs sr as)
      <$> (token At *> tok "common" *> attrs <* nl)

  toolHeader <- rule $
    withSourceRange $
      (\name as sr -> validateToolHeader sr name as)
      <$> (token At *> tok "tool" *> quotedP) <*> (attrs <* nl)

  ------------------------------------------------------------
  -- @config@ stanza bodies: variable declarations, text, buttons
  ------------------------------------------------------------

  choiceList <- rule $
    token OpenBrace *>
    ((:) <$> quotedP <*> many (token Comma *> quotedP))
    <* token CloseBrace

  -- NOTE: `as "label"` and `choice { ... }` both come
  -- *before* the `<-`, so the default-value expression is always the
  -- last thing on the line (immediately followed by `nl`), exactly like
  -- an ordinary `Let` statement's value. Putting them after the value
  -- instead would be genuinely ambiguous: since plain identifiers like
  -- `as`/`choice` are not reserved words, `value`'s own grammar
  -- is happy to treat e.g. `0 as "L"` as the implicit-multiplication/
  -- concatenation of `0`, the bare variable `as`, and the text literal
  -- `"L"` -- a second, equally valid parse of the same tokens.
  -- Captures the value expression's own span, separately from the
  -- whole declaration's ('configDecl's own `withSourceRange`) -- so a
  -- later caller with the original script text in hand can recover
  -- exactly the `<- ...` text the user wrote, not just the
  -- already-parsed 'ParsedValue'. See 'cdDefaultRange's doc comment.
  valueWithRange <- rule $
    withSourceRange ((\v sr -> (sr, v)) <$> value)

  configDecl <- rule $
    withSourceRange $
      (\name fsTy mlabel mchoices (valRange, val) sr ->
          validateConfigDecl sr name fsTy val valRange mlabel mchoices)
      <$> ident
      <*> (colon *> ty)
      <*> optional (tok "as" *> quotedP)
      <*> optional (tok "choice" *> choiceList)
      <*> (token LeftArrow *> valueWithRange <* nl)

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
  -- @\@viewer@ preparation-script header: `prepare name1 : Ty1, ...:`
  ------------------------------------------------------------

  -- Each output's type is given right here in the header (rather than
  -- inferred from a `Let` inside the block, as a first design pass
  -- tried): the block's own body type-checks against an environment
  -- that already has every declared output bound, so its statements
  -- are ordinary assignments (`computed <- ...`), not declarations --
  -- exactly like the main viewer code's own `color <- ...` already
  -- works. That also sidesteps a real type-system wall a Let-based
  -- design hits: this block's own code could never be typed against
  -- the *combined* (config + prep outputs) environment the main code
  -- needs to see, since a `Let` requires the name to be absent first,
  -- and there's no "widen this Code into a bigger environment"
  -- operation in this language to reconcile the two afterward.
  prepareOutputDecl <- rule $
    (\name fsTy -> (name, withType fsTy SomeType)) <$> ident <*> (colon *> ty)

  prepareNames <- rule $
    (:) <$> prepareOutputDecl <*> many (token Comma *> prepareOutputDecl)

  prepareHeader <- rule $
    withSourceRange $
      (\names sr -> validatePrepareNames sr names)
      <$> (tok "prepare" *> prepareNames <* colon <* nl)

  prepareBlock <- rule $
    (\enames code -> (\names -> (names, code)) <$> enames)
    <$> prepareHeader <*> (token Indent *> (withCommon defs <$> viewerBody) <* token Dedent)

  ------------------------------------------------------------
  -- Stanzas
  ------------------------------------------------------------

  configStanza <- rule $
    (\etitle estmts -> ConfigStanza <$> etitle <*> sequence estmts)
    <$> configHeader <*> many configStmt

  viewerStanza <- rule $
    (\ehdr mprepE code ->
        (\ViewerHeader{..} mprep ->
            ViewerStanza vhTitle vhVar vhSize vhCenter vhPixelSize vhIterLimit vhEscape vhVanish
                         vhPixelVar vhResizable (fromMaybe (Dimensions (100, 100)) vhPosition)
                         (isJust vhPosition) mprep code)
        <$> ehdr <*> sequence mprepE)
    <$> viewerHeader <*> optional prepareBlock <*> (withCommon defs <$> viewerBody)

  toolStanza <- rule $
    (\ehdr edecls blocks -> (\ToolHeader{..} decls ->
        ToolStanza thName thFor thShortcut thIcon
                   thDrawLayer thRefreshOnActivate thRefreshCanUpdate decls blocks)
        <$> ehdr <*> sequence edecls)
    <$> toolHeader <*> many configStmt <*> toolBody

  commonStanza <- rule $
    (\ehdr body -> ehdr >> CommonStanza <$> validateCommonBody body)
    <$> commonHeader <*> viewerBody

  stanza <- ruleChoice [configStanza, viewerStanza, toolStanza, commonStanza]

  rule ((token Indent *> many stanza <* token Dedent) <?> "a unified project script")

------------------------------------------------------------
-- Header attribute validation
------------------------------------------------------------

-- | A @\@common@ stanza may only define things: its statements are
-- prepended to every viewer and tool script, so anything else (drawing,
-- assignments, loops) would run surprisingly often.
validateCommonBody :: ParsedCode -> Either (SourceRange, String) ParsedCode
validateCommonBody body = case body of
  Stmt _ (SBlock stmts) -> body <$ mapM_ check stmts
  _ -> body <$ check body
  where
    check (Stmt sr node) = case node of
      SDecl{} -> pure ()
      SFun{}  -> pure ()
      SProc{} -> pure ()
      SPass   -> pure ()
      _ -> Left (sr, "A @common stanza can only contain variable declarations "
                     ++ "(x := ... or x : T <- ...), function definitions (let f(x) = ...), "
                     ++ "and code definitions (define name: ...).")

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

-- | Render a number literal parsed from an unquoted attribute value
-- back to text that re-parses to the exact same value, without ever
-- using scientific notation. See the `numberLiteral`\/`signedNumber`
-- rules' own doc comment in 'scriptGrammar' for why that matters.
showNumberLiteral :: Either Double Integer -> String
showNumberLiteral = either (\d -> showFFloat Nothing d "") show

negateNumberLiteral :: Either Double Integer -> Either Double Integer
negateNumberLiteral = either (Left . negate) (Right . negate)

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
  , vhPosition  :: Maybe Dimensions
    -- ^ 'Nothing' when @position=@ is omitted -- a concrete default is
    -- assigned later, by 'assignViewerPositions', which staggers it
    -- against any other viewer in the same script that also omitted
    -- it. An explicit @position=@ always wins and is never staggered.
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

-- | Parse an integer-valued attribute. May be quoted or not -- the
-- grammar's own @attrValue@ rule accepts a bare (optionally negative)
-- whole number directly now, not just via @ident@.
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
  -- A beginner's first viewer doesn't need to think about a
  -- coordinate-variable name or a window size at all -- `z` and
  -- 600x600 are reasonable defaults, and both stay available as
  -- explicit overrides for anyone who wants something else.
  let vhVar = fromMaybe "z" (lookupAttr "var" attrs)
  vhSize     <- optionalDimensionsAttr sr "size" (Dimensions (600, 600)) attrs
  vhResizable <- optionalBoolAttr sr "resizable" True attrs
  vhPosition  <- traverse (left (sr,) . parseDimensions) (lookupAttr "position" attrs)
  pure ViewerHeader
    { vhTitle, vhVar, vhSize, vhResizable, vhPosition
    , vhCenter    = lookupAttr "center" attrs
    , vhPixelSize = lookupAttr "pixelSize" attrs
    -- An omitted `limit=`/`escape=`/`vanish=`/`pixelVar=` falls back to
    -- a reference to the automatically-provided `max_iters`/
    -- `escape_radius`/`vanishing_radius`/`pixel_size` name (see
    -- parseScript's own synthesized `@config` group for the first
    -- three; `pixel_size` is always bound as a plain `Let`, needing no
    -- such group) -- these attributes remain available as an explicit
    -- override, for a script that wants a differently-named variable
    -- instead.
    , vhIterLimit = lookupAttr "limit"    attrs <|> Just "max_iters"
    , vhEscape    = lookupAttr "escape"   attrs <|> Just "escape_radius"
    , vhVanish    = lookupAttr "vanish"   attrs <|> Just "vanishing_radius"
    , vhPixelVar  = lookupAttr "pixelVar" attrs <|> Just "pixel_size"
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
-- Config declaration validation
------------------------------------------------------------

validateConfigDecl :: SourceRange
                    -> String
                    -> FSType
                    -> ParsedValue
                    -> SourceRange
                    -> Maybe String
                    -> Maybe [String]
                    -> Either (SourceRange, String) ConfigStmt
validateConfigDecl sr name fsTy val valRange mlabel mchoices = do
  case mchoices of
    Just _ | fsTy /= IntegerT ->
      Left (sr, "`choice { ... }` can only be used with a ℤ-typed declaration (`" ++
                name ++ "` has type " ++ withType fsTy ppType ++ ").")
    _ -> Right ()
  pure ConfigDecl
    { cdRange = sr, cdName = name, cdType = withType fsTy SomeType
    , cdDefault = val, cdDefaultRange = valRange, cdDefaultSrc = ""
    , cdLabel = mlabel, cdChoices = mchoices
    }

------------------------------------------------------------
-- Preparation-script header validation
------------------------------------------------------------

validatePrepareNames :: SourceRange -> [(String, SomeType)] -> Either (SourceRange, String) [(String, SomeType)]
validatePrepareNames sr names = case firstDuplicate (map fst names) of
  Just dup -> Left (sr, "`prepare` declares `" ++ dup ++ "` more than once.")
  Nothing  -> Right names

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x:xs)
      | x `Set.member` seen = Just x
      | otherwise           = go (Set.insert x seen) xs
