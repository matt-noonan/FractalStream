{-# language RecursiveDo, DataKinds, ImpredicativeTypes #-}
module Language.Code.Parser
  ( parseCode
  , parseParsedCode
  , codeBlockGrammar
  , codeStatementsGrammar
  , Splices(..)
  , noSplices
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  , ParsedCode
  , withDefinedFunctions
  , runParsedCode
  ) where

import FractalStream.Prelude

import Language.Parser hiding (many)
import Language.Typecheck
import Language.Value.Parser
import Language.Parser.SourceRange (SourceRange(..))
import Language.Code
import Language.Parser.Tokenizer
import Language.Code.Syntax
import Language.Code.Typecheck

import qualified Data.Map as Map
import qualified Data.Set as Set

------------------------------------------------------
-- Main function for parsing Code
------------------------------------------------------

data Splices = Splices
  { codeSplices  :: Map String ParsedCode
  , valueSplices :: Map String ParsedValue
  , funNames     :: Set String
    -- ^ The names of the user-defined functions, which the grammar
    -- parses as function calls rather than as variables.
  }

noSplices :: Splices
noSplices = Splices Map.empty Map.empty Set.empty

-- | Record the names of the functions defined (by @let f(...) = ...@)
-- anywhere in the given tokens, so that the grammar can parse calls to
-- them.
withDefinedFunctions :: [SRToken] -> Splices -> Splices
withDefinedFunctions toks splices =
  splices { funNames = funNames splices `Set.union` Set.fromList (go (map baseToken toks)) }
  where
    go = \case
      Let_ : Identifier f : OpenParen : rest -> f : go rest
      _ : rest -> go rest
      [] -> []

parseCode :: forall env
           . EnvironmentProxy env
          -> Splices
          -> String
          -> Either (Either ParseError TCError) (Code env)
parseCode env splices input = do
  c <- parseParsedCode splices input
  first Right (runParsedCode env c)

parseParsedCode :: Splices -> String -> Either (Either ParseError TCError) ParsedCode
parseParsedCode splices input =
  let toks = tokenizeWithIndentation input
  in first Left (parse (codeBlockGrammar (withDefinedFunctions toks splices)) toks)

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
                  . Splices
                 -> Grammar r (Prod r ParsedCode)
codeBlockGrammar splices = do
  statements <- codeStatementsGrammar splices
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
                        . Splices
                       -> Grammar r (Prod r ParsedCode)
codeStatementsGrammar Splices{..} = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  statements <- rule (node (SBlock <$> many toplevel) <?> "statements")

  block <- rule ((token Indent *> statements <* token Dedent) <?> "indented statements")

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* nl
    , spliced <* nl
    , node
      (SDecl <$> ident
             <*> (Just <$> (colon *> typ))
             <*> (token LeftArrow *> value <* nl)) <?> "variable initialization"
    , node
      (SDecl <$> ident
             <*> pure Nothing
             <*> (token ColonEquals *> value <* nl)) <?> "variable initialization"
    , node
      (SFun <$> (token Let_ *> ident)
            <*> argList
            <*> (token Equal *> value <* nl)) <?> "function definition"
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
      (SIterate
        <$> (lit "iterate" *> ident <* token RightArrow)
        <*> value
        <*> ((lit "while" $> True) <|> (lit "until" $> False))
        <*> value
        <*> optional upTo)
    , node (SPass <$ lit "pass") <?> "pass"
    ]

  value <- valueGrammarWithFunctions funNames valueSplices

  spliced <- rule $
    token OpenSplice *>
    (tokenMatch $ \case { Identifier n -> Map.lookup n codeSplices; _ -> Nothing })
    <* token CloseSplice

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
                    <*> ((tok "with" *> tok "radius") *> value))
    , draw
      (DrawRectFrom <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "rectangle" *> tok "from") *> value)
                    <*> (tok "to" *> value))
    , draw
      (DrawLineFrom <$> ((tok "line" *> tok "from") *> value)
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
    [ node (SForEach
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> ident)
      <*> (colon *> nl *> block))
    , node (SWithFirst
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> ident)
      <*> (colon *> nl *> block)
      <*> optional (tok "else" *> colon *> nl *> block))
    ]

  colon <- rule (token Colon <?> ":")
  nl <- rule (token Newline <?> "end of line")

  pure statements

node :: Prod r StmtF -> Prod r ParsedCode
node = withSourceRange . fmap (flip Stmt)

draw :: Prod r DrawStmt -> Prod r ParsedCode
draw = node . fmap SDraw
