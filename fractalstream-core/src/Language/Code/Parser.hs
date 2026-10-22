{-# language RecursiveDo, DataKinds, ImpredicativeTypes #-}
module Language.Code.Parser
  ( parseCode
  , parseParsedCode
  , codeBlockGrammar
  , Splices(..)
  , noSplices
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  , ParsedCode(..)
  ) where

import FractalStream.Prelude

import Language.Parser hiding (many)
import Language.Typecheck
import Language.Value
import Language.Value.Parser
import Language.Code
import Language.Parser.Tokenizer
import Language.Code.Typecheck

import qualified Data.Map as Map

------------------------------------------------------
-- Main function for parsing Code
------------------------------------------------------

data Splices = Splices
  { codeSplices  :: Map String ParsedCode
  , valueSplices :: Map String ParsedValue
  }

noSplices :: Splices
noSplices = Splices Map.empty Map.empty

parseCode :: forall env
           . EnvironmentProxy env
          -> Splices
          -> String
          -> Either (Either ParseError TCError) (Code env)
parseCode env splices input = do
  ParsedCode c <- parseParsedCode splices input
  case c env of TC x -> first Right x

parseParsedCode :: Splices -> String -> Either (Either ParseError TCError) ParsedCode
parseParsedCode splices input =
  first Left (parse (codeBlockGrammar splices) (tokenizeWithIndentation input))

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
codeBlockGrammar Splices{..} = mdo

  toplevel <- ruleChoice
    [ block
    , lineStatement
    ]

  block <- ruleChoice
    [ check (tcBlock <$> (token Indent *> many toplevel <* token Dedent)) <?> "indented statements"
    -- Grammar hack so that Let statements slurp up the remainder
    -- of the block into their scope.
    , check (
        ((\xs x -> tcBlock (xs ++ [x]))
         <$> (token Indent *> many toplevel)
         <*> (letStatement <* token Dedent)) <?> "indented statements")
    ]

  letStatement <- rule $
    check (
     tcLet <$> ident
           <*> (colon *> typ)
           <*> (token LeftArrow *> value <* nl)
           <*> (check (tcBlock <$> blockTail)) <?> "variable initialization")

  blockTail <- ruleChoice
    [ (\xs x -> xs ++ [x])
      <$> many lineStatement
      <*> letStatement
    , many lineStatement
    ]

  typ <- typeGrammar

  lineStatement <- ruleChoice
    [ simpleStatement <* nl
    , spliced <* nl
    , check
      (tcIfThenElse <$> (token If *> value <* (colon <* nl))
                    <*> block
                    <*> elseIf) <?> "if statement"
    , check
      (tcWhile
        <$> (lit "while" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "while loop"
    , check
      (tcDoWhile
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "while" *> value <* nl)) <?> "repeat...while loop"
    , check
      (tcUntil
        <$> (lit "until" *> value)
        <*> (optional upTo <* colon <* nl)
        <*> block) <?> "until loop"
    , check
      (tcDoUntil
        <$> (lit "repeat" *> optional upTo <* colon <* nl)
        <*> block
        <*> (lit "until" *> value <* nl)) <?> "repeat...until loop"
    , effect <?> "extended operation"
    ]

  upTo <- rule (lit "up" *> lit "to" *> value <* token TimesKeyword)

  elseIf <- ruleChoice
    [ ((token Else *> colon *> nl) *> block) <?> "else clause"
    , check
      (tcIfThenElse <$> ((token Else *> token If) *> value <* (colon <* nl))
                    <*> block
                    <*> elseIf) <?> "else if clause"
    , pure (ParsedCode $ \env -> withEnvironment env $ pure NoOp) <?> "end of block"
    ]

  let lit = token . Identifier

  simpleStatement <- ruleChoice
    [ check
      (tcSet <$> ident <*> (token LeftArrow *> value)) <?> "variable assignment"
    , check
      (tcIterate
        <$> (lit "iterate" *> ident <* token RightArrow)
        <*> value
        <*> ((lit "while" $> True) <|> (lit "until" $> False))
        <*> value
        <*> optional upTo)
    , check ((\_ _ -> pure NoOp) <$ lit "pass") <?> "pass"
    ]

  value <- valueGrammar valueSplices

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

  eraseCommand <- ruleChoice [check (pure tcClear)]

  drawCommand <- ruleChoice
    [ check
      (tcDrawPoint <$> ((tok "point" *> tok "at") *> value))
    , check
      (tcDrawCircle <$> (isJust <$> optional (tok "filled"))
                    <*> ((tok "circle" *> tok "at") *> value)
                    <*> ((tok "with" *> tok "radius") *> value))
    , check
      (tcDrawRect <$> (isJust <$> optional (tok "filled"))
                  <*> ((tok "rectangle" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    , check
      (tcDrawLine <$> ((tok "line" *> tok "from") *> value)
                  <*> (tok "to" *> value))
    ]

  strokeOrLine <- ruleChoice
    [ tok "stroke", tok "line" ]

  penCommand <- ruleChoice
    [ check (tcSetFill   <$> (value <* (tok "for" *> tok "fill")))
    , check (tcSetStroke <$> (value <* (tok "for" *> strokeOrLine)))
    ]

  writeCommand <- rule $
    check (tcWrite <$> value <*> (tok "at" *> value))

  listCommand <- ruleChoice
    [ check (tcListFor
      <$> (tok "for" *> tok "each" *> ident)
      <*> (tok "in" *> ident)
      <*> (colon *> nl *> block))
    , check (tcListWith
      <$> (tok "with" *> tok "first" *> ident)
      <*> (tok "matching" *> value)
      <*> (tok "in" *> ident)
      <*> (colon *> nl *> block)
      <*> optional (tok "else" *> colon *> nl *> block))
    ]

  colon <- rule (token Colon <?> ":")
  nl <- rule (token Newline <?> "end of line")

  pure block

check :: Prod r CheckedCode -> Prod r ParsedCode
check = withSourceRange
      . fmap @_ @CheckedCode (\c sr -> ParsedCode (\env -> withEnvironment env $ c sr env))
