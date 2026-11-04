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
  , ColorizerParam(..)
  , ColorizerSetting(..)
  , ParsedStanza(..)
  , parseScript
  , scriptCommonDefs
  , implicitBookkeepingStanza
  , scriptGrammar
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Parser
import Language.Parser.Tokenizer
import Language.Parser.SourceRange
import Language.Value.Parser
import Language.Code.Parser (CommonDefs(..), noCommonDefs, codeStatementsGrammar, codeBlockGrammar, ParsedCode, withDefinedFunctions, withCommon)
import Language.Code.Syntax (Stmt(..), StmtF(..))
import Actor.Event (toolBlocksGrammar, ParsedToolBlock, Coordinate(..), ViewKind(..))
import Actor.Layout (AppAction(..))

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
-- or tabs of these.
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
  | ConfigButton SourceRange String
    -- ^ A button; only meaningful in a @\@tool@, whose @on button@
    -- handlers it triggers (a @\@config@ stanza rejects it).
  | ConfigAction AppAction String
    -- ^ A button that does something in the app (no script can write
    -- one: a built-in colorizer adds it to its settings)
  | ConfigRow [ConfigStmt]
    -- ^ An indented @row:@ block; its statements are laid out side by
    -- side instead of stacked vertically.
  | ConfigTabs [(String, [ConfigStmt])]
    -- ^ A run of consecutive @tab "Label":@ blocks, shown as one group
    -- of tabs with one page per block.

-- | Collect a block's parsed statements, merging each run of consecutive
-- @tab@ blocks into a single tab group.
configStmts :: [Either e ConfigStmt] -> Either e [ConfigStmt]
configStmts = fmap mergeTabs . sequence
  where
    mergeTabs = \case
      ConfigTabs xs : ConfigTabs ys : rest -> mergeTabs (ConfigTabs (xs ++ ys) : rest)
      stmt : rest -> stmt : mergeTabs rest
      [] -> []

-- | Reject buttons in a @\@config@ stanza: only a tool's @on button@
-- handlers respond to buttons, and only to the tool's own.
noButtons :: [ConfigStmt] -> Either (SourceRange, String) [ConfigStmt]
noButtons stmts = stmts <$ mapM_ check stmts
  where
    check = \case
      ConfigButton sr _ -> Left (sr, "A @config stanza can't have a button, since nothing "
                                  ++ "would respond to it. Put the button in a @tool's "
                                  ++ "declarations, and handle it there with `on button \"...\":`.")
      ConfigRow xs  -> mapM_ check xs
      ConfigTabs ts -> mapM_ (mapM_ check . snd) ts
      _ -> pure ()

-- | One stanza of a unified project script, with its body already parsed
-- but not yet resolved against any other stanza.
data ParsedStanza
  = ConfigStanza
      { csTitle :: String
      , csBody  :: [ConfigStmt]
      }
  | ViewerStanza
      { vsTitle       :: String
      , vsCoord       :: Coordinate
        -- ^ The viewer's coordinate: one complex variable (@var=@), or a
        -- pair of real ones (@xvar=@ and @yvar=@).
      , vsCenter      :: Maybe String
      , vsPixelSize   :: Maybe String
      , vsAspect      :: Maybe String
      , vsView        :: ViewKind
        -- ^ The @view=@ attribute: how the viewer starts out showing its
        -- points, on the plane (the default) or on the Riemann sphere.
        -- ^ The @aspect=@ attribute (a real viewer's pixel height over
        -- its pixel width), as written.
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
      , vsColorizer   :: Maybe String
        -- ^ The @colorizer=@ attribute, if given: the colorizer this
        -- viewer uses by default (see "Actor.Colorizer"), as written,
        -- e.g. @Rainbow@ or @Smooth(z)@.
      , vsColoring    :: [ColorizerSetting]
        -- ^ The settings of an optional @coloring:@ block: values for its
        -- default colorizer's settings, in place of the colorizer's own
        -- defaults.
      , vsPrepare     :: Maybe ([(String, SomeType)], ParsedCode)
        -- ^ An optional @prepare name1 : Type1, name2 : Type2, ...:@
        -- block, naming this viewer's preparation-script output
        -- variables and their types (mirroring the classic YAML
        -- @preparation: outputs:@ list), followed by the block's own
        -- code to compute them. 'Nothing' when the viewer has no
        -- preparation script at all.
      , vsCode        :: ParsedCode
      , vsFunctions   :: Set String
        -- ^ The user-defined functions in scope at the end of the
        -- viewer's code, which is where its colorizer's arguments are
        -- read.
      }
  | CommonStanza
      { cmBody :: ParsedCode
        -- ^ Declarations and function definitions to prepend to every
        -- viewer and tool script (see 'Language.Code.Parser.withCommon').
      }
  | ColorizerStanza
      { czName        :: String
      , czDescription :: Maybe String
      , czConfig      :: [ConfigStmt]
        -- ^ Declarations at the top of the stanza, before its code: each
        -- viewer using this colorizer gets a copy of these fields.
      , czParams      :: [ColorizerParam]
      , czCommon      :: [Stmt]
        -- ^ The @\@common@ statements its code uses (see
        -- 'Language.Code.Parser.withCommon'), to be added along with it
        -- to a viewer that doesn't already have them.
      , czCode        :: ParsedCode
        -- ^ The body of its @define colorize(...):@ block.
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

-- | One line of a viewer's @coloring:@ block: @name <- value@.
data ColorizerSetting = ColorizerSetting
  { czsRange      :: SourceRange
  , czsName       :: String
  , czsValue      :: ParsedValue
  , czsValueRange :: SourceRange
  , czsValueSrc   :: String
    -- ^ The value's text, as written (filled in like 'cdDefaultSrc').
  }

-- | One of a colorizer's parameters: @t@, or @t = expression@.
data ColorizerParam = ColorizerParam
  { cpName         :: String
  , cpDefault      :: Maybe ParsedValue
    -- ^ Its default value, used when it isn't given an argument;
    -- without one, it gets the viewer's variable of the same name.
  , cpDefaultRange :: SourceRange
  , cpDefaultSrc   :: String
    -- ^ The default value's text, as written (filled in like
    -- 'cdDefaultSrc').
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
      restChunks = [ ic | ic@(_, c) <- chunks, not (isChunk "common" c) ]
  (commonResults, defs) <- parseCommonChunks chunks
  restResults <- traverse (\(i, c) -> (i,) <$> parseChunk defs c) restChunks
  pure (fillDefaultSrcs input (map snd (sortOn fst (commonResults ++ restResults))))

-- | What a project script's @\@common@ stanzas give every other piece of
-- its code: their statements, and the names of the functions they define.
-- For code that isn't part of the script itself but should see the same
-- definitions, such as a movie's script ("Actor.Movie").
scriptCommonDefs :: String -> Either (SourceRange, String) CommonDefs
scriptCommonDefs input =
  snd <$> parseCommonChunks (zip [0 ..] (splitStanzas (tokenizeWithIndentation input)))

-- | Parse the @\@common@ stanzas among a script's (numbered) stanzas,
-- giving them and the definitions they make.
parseCommonChunks :: [(Int, [SRToken])]
                  -> Either (SourceRange, String) ([(Int, ParsedStanza)], CommonDefs)
parseCommonChunks chunks = do
  let commonChunks = [ ic | ic@(_, c) <- chunks, isChunk "common" c ]
      -- every stanza can call the functions defined in any @common stanza
      defs0 = withDefinedFunctions (concatMap snd commonChunks) noCommonDefs
  commonResults <- traverse (\(i, c) -> (i,) <$> parseChunk defs0 c) commonChunks
  let common = concat [ stmts | (_, CommonStanza (Stmt _ (SBlock stmts))) <- commonResults ]
  pure (commonResults, defs0 { commonCode = common })

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
      c@ColorizerStanza{..} -> c { czConfig = map onStmt czConfig
                                 , czParams = [ p { cpDefaultSrc = sliceLine input (cpDefaultRange p) }
                                              | p <- czParams ] }
      v@ViewerStanza{..} -> v { vsColoring = [ c { czsValueSrc = sliceLine input (czsValueRange c) }
                                             | c <- vsColoring ] }
      c@CommonStanza{} -> c

    onStmt = \case
      d@ConfigDecl{..} -> d { cdDefaultSrc = sliceLine input cdDefaultRange }
      ConfigRow stmts  -> ConfigRow (map onStmt stmts)
      ConfigTabs tabs  -> ConfigTabs [ (l, map onStmt stmts) | (l, stmts) <- tabs ]
      other            -> other

    sliceLine :: String -> SourceRange -> String
    sliceLine src sr = case spanOfSourceRange sr of
      Just (InLine i s e) -> case drop i (lines src) of
        (line:_) -> take (e + 1 - s) (drop s line)
        []       -> ""
      _ -> ""

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

  -- Dimensions (`size=600x600`) are always whole, non-negative pixel
  -- counts, so this deliberately doesn't go through
  -- `numberLiteral`/`signedNumber` above. No attribute takes one any
  -- more, but still parsing them lets an old `@viewer size=...`
  -- header reach 'validateViewerHeader's explanation of where window
  -- geometry went, rather than failing as a bare parse error.
  dimWidth <- rule (tokenMatch (\case { NumberI n -> Just n; _ -> Nothing }))

  dimensionValue <- rule
    ((\w h -> show w ++ "x" ++ h) <$> dimWidth <*> tokenMatch (\case
        Identifier ('x':ds@(_:_)) | all isDigit ds -> Just ds
        _ -> Nothing))

  boolValue <- rule (("true" <$ token True_) <|> ("false" <$ token False_))

  -- A point, `center=(1, -1/2)`, for a real viewer.
  pairValue <- rule
    ((\a b -> "(" ++ a ++ ", " ++ b ++ ")")
      <$> (token OpenParen *> (fractionValue <|> signedNumber))
      <*> (token Comma *> (fractionValue <|> signedNumber) <* token CloseParen))

  attrValue <- rule
    (quotedP <|> dimensionValue <|> fractionValue <|> boolValue <|> signedNumber <|> pairValue <|> ident)

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
    withSourceRange ((\s sr -> Right (ConfigButton sr s)) <$> (tok "button" *> quotedP <* nl))

  -- An indented `row:` block, the same shape as `on <event>:`/`if ...:`
  -- introduce an indented block elsewhere in this language -- its
  -- statements lay out side by side instead of stacked vertically.
  configRow <- rule $
    (\estmts -> ConfigRow <$> configStmts estmts) <$>
    (tok "row" *> colon *> nl *> token Indent *> many configStmt <* token Dedent)

  -- A `tab "Label":` block, holding one page of a tab group. Each block
  -- parses as a group of its own; 'configStmts' then merges consecutive
  -- groups, which keeps the grammar itself unambiguous.
  configTab <- rule $
    (\label estmts -> (\stmts -> ConfigTabs [(label, stmts)]) <$> configStmts estmts) <$>
    (tok "tab" *> quotedP) <*>
    (colon *> nl *> token Indent *> many configStmt <* token Dedent)

  configStmt <- ruleChoice [configDecl, configText, configButton, configRow, configTab]

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
    (\etitle estmts -> ConfigStanza <$> etitle <*> (configStmts estmts >>= noButtons))
    <$> configHeader <*> many configStmt

  -- A `coloring:` block, at the start of a viewer's code: one
  -- `name <- value` line for each setting of its colorizer to change.
  coloringSetting <- rule $
    withSourceRange $
      (\name (valRange, val) sr -> ColorizerSetting sr name val valRange "")
      <$> ident <*> (token LeftArrow *> valueWithRange <* nl)

  coloringBlock <- rule $
    withSourceRange $
      (\settings sr -> case firstDuplicate (map czsName settings) of
          Just dup -> Left (sr, "The `coloring:` block sets `" ++ dup ++ "` more than once.")
          Nothing -> Right settings)
      <$> (tok "coloring" *> colon *> nl *> token Indent *> many coloringSetting <* token Dedent)

  viewerStanza <- rule $
    (\ehdr mcoloringE mprepE code ->
        (\ViewerHeader{..} mcoloring mprep ->
            ViewerStanza vhTitle vhCoord vhCenter vhPixelSize vhAspect vhView vhIterLimit vhEscape vhVanish
                         vhPixelVar vhColorizer (fromMaybe [] mcoloring) mprep code (funNames defs))
        <$> ehdr <*> sequence mcoloringE <*> sequence mprepE)
    <$> viewerHeader <*> optional coloringBlock <*> optional prepareBlock
    <*> (withCommon defs <$> viewerBody)

  toolStanza <- rule $
    (\ehdr edecls blocks -> (\ToolHeader{..} decls ->
        ToolStanza thName thFor thShortcut thIcon
                   thDrawLayer thRefreshOnActivate thRefreshCanUpdate decls blocks)
        <$> ehdr <*> configStmts edecls)
    <$> toolHeader <*> many configStmt <*> toolBody

  commonStanza <- rule $
    (\ehdr body -> ehdr >> CommonStanza <$> validateCommonBody body)
    <$> commonHeader <*> viewerBody

  colorizerHeader <- rule $
    withSourceRange $
      (\name as sr -> validateColorizerHeader sr name as)
      <$> (token At *> tok "colorizer" *> quotedP) <*> (attrs <* nl)

  -- (Not a top-level pair, `a, b`: the comma is between parameters.)
  paramDefault <- argumentGrammarWithFunctions (funNames defs)

  colorizerParam <- rule $
    (\name mdef -> case mdef of
        Nothing -> ColorizerParam name Nothing NoSourceRange ""
        Just (sr, v) -> ColorizerParam name (Just v) sr "")
    <$> ident
    <*> optional (token Equal *> withSourceRange ((\v sr -> (sr, v)) <$> paramDefault))

  colorizerParams <- rule $
    (:) <$> (token OpenParen *> colorizerParam)
        <*> (many (token Comma *> colorizerParam) <* token CloseParen)

  colorizerBlock <- codeBlockGrammar defs

  -- `define colorize(v, w):` and its block. Only one name is allowed, so
  -- that the stanza says plainly which code is the colorizer's.
  colorizerDefine <- rule $
    withSourceRange $
      (\params code sr -> case firstDuplicate (map cpName params) of
          Just dup -> Left (sr, "The colorizer's code has two parameters called `" ++ dup ++ "`.")
          Nothing -> Right (params, code))
      <$> (tok "define" *> tok "colorize" *> (fromMaybe [] <$> optional colorizerParams))
      <*> (colon *> nl *> colorizerBlock)

  colorizerStanza <- rule $
    (\ehdr estmts edef -> do
        (name, desc) <- ehdr
        stmts <- configStmts estmts >>= noButtons
        (params, code) <- edef
        -- (The @common statements its parameters' defaults use, too.)
        let withDefaults = Stmt NoSourceRange $ SBlock $
              [ Stmt NoSourceRange (SDecl (cpName p) Nothing v) | p <- params, Just v <- [cpDefault p] ]
              ++ [code]
        pure (ColorizerStanza name desc stmts params (commonUsedBy defs withDefaults) code))
    <$> colorizerHeader <*> many configStmt <*> (colorizerDefine <?> "the colorizer's code, `define colorize:`")

  stanza <- ruleChoice [configStanza, viewerStanza, toolStanza, commonStanza, colorizerStanza]

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
  , vhCoord     :: Coordinate
  , vhCenter    :: Maybe String
  , vhPixelSize :: Maybe String
  , vhAspect    :: Maybe String
  , vhView      :: ViewKind
  , vhIterLimit :: Maybe String
  , vhEscape    :: Maybe String
  , vhVanish    :: Maybe String
  , vhPixelVar  :: Maybe String
  , vhColorizer :: Maybe String
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


validateViewerHeader :: SourceRange
                      -> String
                      -> [(String, String)]
                      -> Either (SourceRange, String) ViewerHeader
validateViewerHeader sr vhTitle attrs = do
  -- Window geometry used to be set here too, but it belongs to the
  -- frontend showing the viewer rather than to the script, so it now
  -- lives in the project file's frontend-specific keys instead (e.g.
  -- `wx-properties:`). Explain that, rather than just calling the
  -- attribute unknown.
  forM_ attrs $ \(k, _) ->
    when (k `elem` ["size", "position", "resizable"]) $
      Left (sr, "The `" ++ k ++ "` attribute is no longer part of a `@viewer` \
                \header: window size, position, and resizability are saved \
                \by the user interface itself (for the wx UI, under the \
                \project file's `wx-properties:` key).")
  checkKnownAttrs sr
    [ "var", "xvar", "yvar", "center", "pixelSize", "aspect", "view"
    , "limit", "escape", "vanish", "pixelVar", "colorizer" ]
    attrs
  -- A beginner's first viewer doesn't need to think about a
  -- coordinate-variable name at all -- `z` is a reasonable default,
  -- and stays available as an explicit override for anyone who wants
  -- something else. A real viewer names both of its coordinates.
  vhCoord <- case (lookupAttr "var" attrs, lookupAttr "xvar" attrs, lookupAttr "yvar" attrs) of
    (Just _, Just _, _) -> Left (sr, varAndXY)
    (Just _, _, Just _) -> Left (sr, varAndXY)
    (mv, Nothing, Nothing) -> Right (ComplexCoordinate (fromMaybe "z" mv))
    (Nothing, Just x, Just y)
      | x == y -> Left (sr, "A viewer's `xvar` and `yvar` must be different variables, \
                            \but both are `" ++ x ++ "`.")
      | otherwise -> Right (RealCoordinates x y)
    (Nothing, Just _, Nothing) -> Left (sr, "A viewer with `xvar=` also needs `yvar=`.")
    (Nothing, Nothing, Just _) -> Left (sr, "A viewer with `yvar=` also needs `xvar=`.")
  let vhAspect = lookupAttr "aspect" attrs
  case (vhCoord, vhAspect) of
    (ComplexCoordinate _, Just _) ->
      Left (sr, "Only a real viewer (one with `xvar=` and `yvar=`) can have an \
                \`aspect` attribute: a complex viewer's pixels are always square.")
    _ -> Right ()
  vhView <- case lookupAttr "view" attrs of
    Nothing -> Right PlaneView
    Just "plane" -> Right PlaneView
    Just "sphere" -> Right SphereView
    Just v -> Left (sr, "A viewer's `view` is `plane` or `sphere`, not `" ++ v ++ "`.")
  case (vhCoord, vhView) of
    (RealCoordinates _ _, SphereView) ->
      Left (sr, "Only a complex viewer (one with `var=`) can be shown on the sphere \
                \(`view=sphere`): a real viewer's points are pairs of real numbers.")
    _ -> Right ()
  pure ViewerHeader
    { vhTitle, vhCoord, vhAspect, vhView
    , vhCenter
    = lookupAttr "center" attrs
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
    , vhColorizer = lookupAttr "colorizer" attrs
    }

  where
    varAndXY = "A viewer has either one complex coordinate (`var=`) or two \
               \real ones (`xvar=` and `yvar=`), not both."

validateColorizerHeader :: SourceRange -> String -> [(String, String)]
                        -> Either (SourceRange, String) (String, Maybe String)
validateColorizerHeader sr name attrs = do
  checkKnownAttrs sr ["description"] attrs
  when (null name || '(' `elem` name) $
    Left (sr, "A colorizer's name can't be empty, or contain `(`.")
  pure (name, lookupAttr "description" attrs)

-- | The @\@common@ statements some code uses.
commonUsedBy :: CommonDefs -> ParsedCode -> [Stmt]
commonUsedBy defs code = case withCommon defs (Stmt NoSourceRange (SBlock [code])) of
  Stmt _ (SBlock stmts) -> take (length stmts - 1) stmts
  _ -> []

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
