{-# language RecursiveDo, DataKinds, ImpredicativeTypes #-}
module Language.Code.Parser
  ( parseCode
  , parseParsedCode
  , codeBlockGrammar
  , codeStatementsGrammar
  , CommonDefs(..)
  , noCommonDefs
  , builtinFunctionNames
  , viewKindName
  , viewPixelSizeName
  , viewMobiusNames
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  , ParsedCode
  , withDefinedFunctions
  , withCommon
  , runParsedCode
  ) where

import FractalStream.Prelude

import Language.Parser hiding (many)
import Language.Typecheck
import Language.Value.Parser
import Language.Parser.SourceRange (SourceRange(..), Pos(..))
import Language.Code
import Language.Parser.Tokenizer
import Language.Code.Syntax
import Language.Draw (RadiusUnit(..))
import Language.Value.Syntax (Syn(..), ExprF(..), BinOp(..), UnOp(..))
import Language.Code.Typecheck

import qualified Data.Set as Set

------------------------------------------------------
-- Main function for parsing Code
------------------------------------------------------

-- | What a script's grammar needs to know about the project's shared
-- definitions.
data CommonDefs = CommonDefs
  { funNames     :: Set String
    -- ^ The names of the user-defined functions, which the grammar
    -- parses as function calls rather than as variables.
  , commonCode   :: [Stmt]
    -- ^ Statements (from a project's @\@common@ stanzas) to prepend to
    -- every viewer and tool script; see 'withCommon'.
  }

-- | No project definitions: only the built-in functions (see
-- 'builtinFunctions').
noCommonDefs :: CommonDefs
noCommonDefs = CommonDefs (Set.fromList builtinFunctionNames) builtinFunctions

-- | Functions every tool can call, written as if in a project's @\@common@
-- (so that, like those, they're only added to the scripts that use them).
builtinFunctionNames :: [String]
builtinFunctionNames = ["pixel_size_at"]

-- | The names under which a tool is told how its viewer shows its points
-- (see "Actor.Ensemble"): the kind of view (0 the plane, 1 the sphere, 2
-- the hyperbolic plane), the width of a pixel on the screen plane, and in
-- the hyperbolic plane, part of the map from the screen to the model (see
-- 'viewMobiusNames').
viewKindName, viewPixelSizeName :: String
viewKindName = "[internal] view kind"
viewPixelSizeName = "[internal] view pixel size"

-- (the real and imaginary parts of @a@ and @c@ of the Möbius map, of
-- determinant 1, from the screen to the model: see
-- 'Actor.Viewer.Disk.viewMobius')
viewMobiusNames :: [String]
viewMobiusNames = [ "[internal] view " ++ n | n <- ["a.re", "a.im", "c.re", "c.im"] ]

-- | The built-in functions' definitions.
--
-- @pixel_size_at(p)@ is the width, in the model, of a screen pixel at the
-- model point @p@: on the plane, the pixel size; in the disk, the pixel
-- size stretched by the view at @p@ (|V'(s)| at the screen point s showing
-- p: for the screen-to-model map (a s + b)/(c s + d), of determinant 1,
-- that's |a - c p|²); on the sphere, by the stereographic projection
-- at @p@, (1 + |p|²) / 2 (a pixel facing the viewer: near the globe's rim,
-- pixels are longer one way). A tool sizes a marker at @p@ with it, so the
-- marker looks the same anywhere in the view.
builtinFunctions :: [Stmt]
builtinFunctions = [ Stmt NoSourceRange (SFun "pixel_size_at" ["p"] pixelSizeAt) ]
  where
    e = Syn NoSourceRange
    var = e . EVar
    real d = e (ELit (show d) (SomeHaskellType RealType d))
    int k = e (ELit (show k) (SomeHaskellType IntegerType k))
    i = e (ELit "i" (SomeHaskellType ComplexType (0 :+ 1)))
    bin op x y = e (EBin op x y)
    absSq x = bin OpMul (e (EUn OpAbs x)) (e (EUn OpAbs x))
    p = var "p"
    (ar, ai, cr, ci) = case map var viewMobiusNames of
      [w, x, y, z] -> (w, x, y, z)
      _ -> error "internal error: a, c: four parts"
    av = bin OpAdd ar (bin OpMul ai i)
    cv = bin OpAdd cr (bin OpMul ci i)
    kind = var viewKindName
    disk = absSq (bin OpSub av (bin OpMul cv p))
    sphere = bin OpDiv (bin OpAdd (real 1) (absSq p)) (real 2)
    pixelSizeAt = bin OpMul (var viewPixelSizeName)
      (e (EITE (bin OpExactEq kind (int 2)) disk
                (e (EITE (bin OpExactEq kind (int 1)) sphere (real 1)))))

-- | Prepend the common statements that a script body uses (directly, or
-- through other common definitions) to it, so that their declarations
-- and function definitions scope over the whole body. Unused definitions
-- are left out, so a definition that mentions a variable some scripts
-- don't have (like a viewer's coordinate, which tool refresh handlers
-- don't see) only matters where it is used.
withCommon :: CommonDefs -> ParsedCode -> ParsedCode
withCommon CommonDefs{commonCode} body = case (used, body) of
  ([], _) -> body
  (_, Stmt sr (SBlock stmts)) -> Stmt sr (SBlock (used ++ stmts))
  _ -> Stmt (stmtRange body) (SBlock (used ++ [body]))
  where
    -- Walk the common statements backwards, keeping each one that
    -- defines something needed later (by the body, or by a kept
    -- definition after it).
    used = fst (foldr keep ([], stmtNames body) commonCode)
    keep stmt (kept, needed)
      | any (`Set.member` needed) (definedNames stmt) = (stmt : kept, needed <> stmtNames stmt)
      | otherwise = (kept, needed)

-- | Record the names of the functions defined (by @f(...) := ...@)
-- anywhere in the given tokens, so that the grammar can parse calls to
-- them.
withDefinedFunctions :: [SRToken] -> CommonDefs -> CommonDefs
withDefinedFunctions toks defs =
  defs { funNames = funNames defs `Set.union` Set.fromList (definedFunctionNames (map baseToken toks)) }

-- | The names of the functions defined in the given tokens: a definition
-- is a name, its parameters in parentheses, then @:=@, starting a line.
definedFunctionNames :: [Token] -> [String]
definedFunctionNames = go True
  where
    go atStart = \case
      Identifier f : rest
        | atStart, Just _ <- functionDefinitionBody rest -> f : go False rest
      t : rest -> go (t `elem` [Newline, Indent, Dedent]) rest
      [] -> []

parseCode :: forall env
           . EnvironmentProxy env
          -> CommonDefs
          -> String
          -> Either (Either ParseError TCError) (Code env)
parseCode env defs input = do
  c <- parseParsedCode defs input
  first Right (runParsedCode env c)

parseParsedCode :: CommonDefs -> String -> Either (Either ParseError TCError) ParsedCode
parseParsedCode defs input =
  let toks = tokenizeWithIndentation input
  in first Left (parse (codeBlockGrammar (withDefinedFunctions toks defs)) toks)

------------------------------------------------------
-- Code grammar
------------------------------------------------------

-- | The grammar for a single indented block of statements (what follows a
-- header line like @if ...:@, @until ...:@, or, for whole-program/handler-
-- body parsing, the entire input). Exported so that other grammars with
-- their own header syntax (e.g. 'Actor.Event's tool-script grammar) can
-- recurse into statement-block parsing from their own 'Grammar' computation,
-- the same way this grammar itself calls into 'valueGrammar'.
codeBlockGrammar :: forall r
                  . CommonDefs
                 -> Grammar r (Prod r ParsedCode)
codeBlockGrammar defs = do
  statements <- codeStatementsGrammar defs
  rule ((token Indent *> statements <* token Dedent) <?> "indented statements")

-- | The grammar for a bare sequence of statements with no surrounding
-- @Indent@\/@Dedent@ tokens -- i.e. the part of 'codeBlockGrammar' that
-- comes after its own @Indent@ is consumed and before its @Dedent@.
-- Exported so that a grammar whose statement sequences aren't indentation-
-- delimited (e.g. a future unified project-script grammar, where a
-- @\@viewer@\/@\@tool@ stanza's body sits flush with its own header rather
-- than nested beneath it) can still reuse the full statement-level
-- language -- declarations, assignments, function definitions, loops,
-- conditionals, and draw commands -- without re-deriving any of it.
codeStatementsGrammar :: forall r
                        . CommonDefs
                       -> Grammar r (Prod r ParsedCode)
codeStatementsGrammar CommonDefs{..} = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  statements <- rule (node (SBlock <$> many toplevel) <?> "statements")

  block <- rule ((token Indent *> statements <* token Dedent) <?> "indented statements")

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* nl
    , node
      (SDecl <$> ident
             <*> (Just <$> (colon *> typ))
             <*> (token ColonEquals *> value <* nl)) <?> "variable initialization"
    , node
      (SDecl <$> ident
             <*> pure Nothing
             <*> (token ColonEquals *> value <* nl)) <?> "variable initialization"
    , node
      (SDeclTuple <$> tupleTargets
                  <*> (token ColonEquals *> value <* nl)) <?> "variable initialization"
    , node
      (SProc <$> (lit "define" *> ident)
             <*> (fromMaybe [] <$> optional argList)
             <*> (colon *> nl *> block)) <?> "code definition"
    , node
      (SRun <$> (lit "run" *> ident)
            <*> (optional value <* nl)) <?> "run"
    , node
      (SFun <$> ident
            <*> argList
            <*> (token ColonEquals *> value <* nl)) <?> "function definition"
    , node
      (SIf <$> (token If *> value <* (colon <* nl))
           <*> block
           <*> elseIf) <?> "if statement"
    , node
      ((\c l b -> SLoop LoopWhile c l b)
        <$> (lit "while" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "while loop"
    , node
      ((\l b c -> SLoop LoopDoWhile c l b)
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "while" *> value <* nl)) <?> "repeat...while loop"
    , node
      ((\c l b -> SLoop LoopUntil c l b)
        <$> (lit "until" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "until loop"
    , node
      ((\l b c -> SLoop LoopDoUntil c l b)
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "until" *> value <* nl)) <?> "repeat...until loop"
    , effect <?> "extended operation"
    ]

  argList <- rule $
    ((:) <$> (token OpenParen *> ident)
         <*> ((many (token Comma *> ident)) <* token CloseParen))

  upTo <- rule (lit "up" *> lit "to" *> value <* token TimesKeyword)

  -- What a tuple assignment assigns to: a variable, or two or more
  -- patterns in parentheses, nesting to the right as pairs do.
  patternAtom <- rule $
        withSourceRange ((\n sr -> PVar sr n) <$> ident)
    <|> (token OpenParen *> patternTuple <* token CloseParen)
  patternTuple <- rule $
    withSourceRange ((\p ps sr -> nestPattern sr p ps)
                       <$> patternAtom <*> some (token Comma *> patternAtom))
  -- a tuple of targets, with or without its parentheses:
  -- `x, y <- ...` or `(x, y) <- ...`
  tupleTargets <- rule $
    patternTuple <|> (token OpenParen *> patternTuple <* token CloseParen)
  -- `iterate z -> ...` or `iterate (x, y) -> ...`
  iterateTarget <- rule $
        withSourceRange ((\n sr -> PVar sr n) <$> ident)
    <|> (token OpenParen *> patternTuple <* token CloseParen)

  elseIf <- ruleChoice
    [ ((token Else *> colon *> nl) *> block) <?> "else clause"
    , node
      (SIf <$> ((token Else *> token If) *> value <* (colon <* nl))
           <*> block
           <*> elseIf) <?> "else if clause"
    , pure (Stmt NoSourceRange SPass) <?> "end of block"
    ]

  let lit = token . Identifier

  simpleStatement <- ruleChoice
    [ node
      (SSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , node
      (SSetTuple <$> tupleTargets <*> (token LeftArrow *> value)) <?> "variable assignment"
    , node
      (SIterate
        <$> (lit "iterate" *> iterateTarget <* token RightArrow)
        <*> value
        <*> ((lit "while" $> True) <|> (lit "until" $> False))
        <*> value
        <*> optional upTo)
    , node (SPass <$ lit "pass") <?> "pass"
    -- writing a table's cell: `T @ (j, k) <- v`, or `T @ point z <- v`,
    -- or a slice of it: `T @ (j, *) <- xs`, `T @ * <- xs`
    , node
      (SSetTable <$> ident <*> (token At *> value) <*> (token LeftArrow *> value))
      <?> "table assignment"
    -- `resize T to (w, h)`, maybe `filled with v`
    , node
      (SResize <$> (lit "resize" *> ident)
               <*> (lit "to" *> tableSize)
               <*> optional (lit "filled" *> lit "with" *> value))
      <?> "table resize"
    -- `reset T to (w, h)`, maybe `from p to q`, maybe `filled with v`
    , node
      (SAllocate <$> (lit "reset" *> ident)
                 <*> (lit "to" *> tableSize)
                 <*> optional ((,) <$> (lit "from" *> value) <*> (lit "to" *> value))
                 <*> optional (lit "filled" *> lit "with" *> value))
      <?> "table reset"
    ]

  value <- valueGrammarWithFunctions funNames

  -- A table's size, in `allocate`: in parentheses, or a single name or
  -- number, so that what follows (`from p to q`) can't be read as part
  -- of it (`(w, h) from p` would be an implicit product).
  tableSize <- ruleChoice
    [ token OpenParen *> value <* token CloseParen
    , withSourceRange ((\n sr -> Syn sr (EVar n)) <$> ident)
    , withSourceRange ((\n sr -> Syn sr (ELit (show n) (SomeHaskellType IntegerType (fromInteger n))))
                       <$> tokenMatch (\case { NumberI n -> Just n; _ -> Nothing }))
    ]

  effect <- ruleChoice
    [ toplevelDrawCommand
    , listCommand
    ]

  toplevelDrawCommand <- ruleChoice
    [ (tok "draw" *> drawCommand <* nl)
    , (tok "use" *> penCommand <* nl)
    , (tok "erase" *> eraseCommand <* nl)
    , (tok "write" *> writeCommand <* nl)
    ]

  eraseCommand <- ruleChoice [draw (pure EraseAll)]

  drawCommand <- ruleChoice
    [ draw
      (DrawPointAt <$> ((tok "point" *> tok "at") *> value))
    , draw
      (DrawCircleAt <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "circle" *> tok "at") *> value)
                    <*> ((tok "with" *> tok "radius") *> value)
                    <*> (maybe ModelUnits (const ScreenPixels) <$> optional (tok "pixels")))
    , draw
      (DrawRectFrom <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "rectangle" *> tok "from") *> value)
                    <*> (tok "to" *> value))
    , draw
      (DrawLineFrom <$> ((tok "line" *> tok "from") *> value)
                    <*> (tok "to" *> value))
    , draw
      (DrawGeodesicFrom <$> ((tok "geodesic" *> tok "from") *> value)
                        <*> (tok "to" *> value))
    ]

  strokeOrLine <- ruleChoice
    [ tok "stroke", tok "line" ]

  penCommand <- ruleChoice
    [ draw (UseFill   <$> (value <* (tok "for" *> tok "fill")))
    , draw (UseStroke <$> (value <* (tok "for" *> strokeOrLine)))
    ]

  writeCommand <- rule $
    draw (WriteAt <$> value <*> (tok "at" *> value))

  listCommand <- ruleChoice
    [ withSourceRange
      ((\item list body sr -> withListVar sr list (\l -> SForEach item l body))
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> value)
      <*> (colon *> nl *> block))
    , withSourceRange
      ((\item p list body fallback sr ->
          withListVar sr list (\l -> SWithFirst item p l body fallback))
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> value)
      <*> (colon *> nl *> block)
      <*> optional (token Else *> colon *> nl *> block))
    ]

  colon <- rule (token Colon <?> ":")
  nl <- rule (token Newline <?> "end of line")

  pure statements

-- | Build a statement that loops over a list, which the typed AST needs to
-- be a variable. Any other list expression is first stored in a hidden
-- variable, named after the statement's position so that nested loops get
-- different names, and scoped to a block around the statement.
withListVar :: SourceRange -> ParsedValue -> (String -> StmtF) -> ParsedCode
withListVar sr list mk = case list of
  Syn _ (EVar v) -> Stmt sr (mk v)
  _ -> Stmt sr (SBlock [ Stmt sr (SDecl hidden Nothing list), Stmt sr (mk hidden) ])
  where
    hidden = case sr of
      SourceRange (Pos r c) _ -> "[list at " ++ show r ++ ":" ++ show c ++ "]"
      NoSourceRange -> "[list]"

node :: Prod r StmtF -> Prod r ParsedCode
node = withSourceRange . fmap (flip Stmt)

draw :: Prod r DrawStmt -> Prod r ParsedCode
draw = node . fmap SDraw

-- | A tuple of patterns, nesting to the right: @(a, b, c)@ is
-- @(a, (b, c))@.
nestPattern :: SourceRange -> Pattern -> [Pattern] -> Pattern
nestPattern sr p = \case
  [] -> p
  q : qs -> PTuple sr p (nestPattern sr q qs)
