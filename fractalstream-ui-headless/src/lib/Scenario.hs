{-# language RecursiveDo #-}
{- |
Module       : Scenario
Description  : The scenario language's AST and parser (see SCENARIO_LANGUAGE.md)

A scenario is a flat sequence of commands, one per line, driving a
headless FractalStream session: select a viewer/tool, inject synthetic
input, change configuration/viewport, and capture or assert on what got
rendered. This module only parses scenario text into a 'Command' list;
executing commands against a loaded project is a separate concern (the
interpreter, built on top of 'Actor.Ensemble'/'Actor.Viewer').

Following the same approach as the tool-script grammar
("Actor.Event"'s @toolScriptGrammar@): one Earley pass over the whole
input, reusing the existing tokenizer and the real value-expression
grammar ('valueGrammar') for every numeric\/arithmetic
position, rather than a separate hand-rolled syntax or re-tokenizing
substrings.
-}
module Scenario
  ( Command(..)
  , Point(..)
  , RGB
  , MovieAnimation(..)
  , MovieOptions(..)
  , parseScenario
  ) where

import FractalStream.Prelude

import Language.Value.Typecheck (ParsedValue)
import Actor.Movie (InsetCorner(..))
import Actor.Event (Coordinate(..), ViewKind(..))
import Language.Value.Parser (valueGrammar, arithGrammar)
import Language.Value.Syntax (textLiteral)
import Data.Char (isDigit)
import Language.Parser hiding (many)
import Language.Parser.Tokenizer (Token(..), tokenizeWithIndentation)
import Language.Parser.SourceRange

-- | A model-space point, either as two (not-yet-typechecked) real-valued
-- expressions in Cartesian form, @(x, y)@, or as a single complex-valued
-- expression, @1 + 2i@ -- the natural way to write a point for a
-- complex-plane viewer, reusing the value grammar's own complex literal
-- syntax rather than a separate ad hoc notation.
data Point
  = CartesianPoint ParsedValue ParsedValue
  | ComplexPoint ParsedValue

-- | An 8-bit RGB color literal.
type RGB = (Int, Int, Int)

data Command
  = SelectViewer String
  | SelectTool String
  | DeselectTool

  | SetParameter String ParsedValue
  | SetToolParameter String ParsedValue
  | SetColorizerParameter String (Either String ParsedValue)
    -- ^ A setting of the current viewer's colorizer, to a quoted choice
    -- (as its field shows it, e.g. @"Two colors"@: 'Left') or a value.

  | ResizeViewer Int Int
  | SetViewerCenter Point
  | SetViewerPixelSize ParsedValue
  | SetViewerAspect ParsedValue
  | SetViewerView ViewKind

  | ClickAt Point
  | DoubleClickAt Point
  | DragFromTo Point Point
  | PressButton String
  | TriggerRefresh
  | TriggerTimer String

  | WaitForViewer String

  | CaptureViewer String FilePath
  | CaptureViewerBase String FilePath
  | CaptureViewerAsPreview String Bool
    -- ^ The viewer's title, and whether to leave out tools' drawings
    -- (@capture viewer "..." base as preview@). Only allowed in a
    -- project's @metadata: preview-commands:@.

  | AssertPixel String (Int, Int) RGB (Maybe Int)

  | RecordMovie String FilePath MovieOptions MovieAnimation
    -- ^ Record a movie of the named viewer into a file (see "Actor.Movie").

-- | How a scenario's movie is recorded.
data MovieOptions = MovieOptions
  { moFrames :: Int
  , moFps    :: Int
  , moLoop   :: Bool  -- ^ leave out the frame at @t = 1@
  , moBase   :: Bool  -- ^ leave out tools' drawings, as @capture ... base@ does
  , moInset  :: Maybe (String, InsetCorner)
    -- ^ a viewer to show in a corner of each frame (the bottom right,
    -- unless it says otherwise), marking where along the path the frame
    -- is (only for a path)
  }

-- | What changes from one frame of a movie to the next.
data MovieAnimation
  = MovieScript String
    -- ^ a script run each frame, with @t@ going from 0 to 1
  | MoviePath Coordinate [Point]
    -- ^ a complex variable, or a pair of real ones, moved along a path
    -- through these points

-- | Parse a whole scenario file into its list of commands.
parseScenario :: String -> Either (SourceRange, String) [Command]
parseScenario input =
  left (errorLocation &&& unlines . pp) (parse scenarioGrammar (tokenizeWithIndentation input))

scenarioGrammar :: forall r. Grammar r (Prod r [Command])
scenarioGrammar = mdo

  value <- valueGrammar
  arith <- arithGrammar

  str    <- rule (tokenMatch (\case { Quoted s -> Just s; _ -> Nothing }) <?> "a quoted string")
  intLit <- rule (tokenMatch (\case { NumberI n -> Just (fromIntegral n); _ -> Nothing }) <?> "a whole number")
  ident' <- rule (ident <?> "a variable name")
  nl     <- rule (token Newline <?> "end of line")

  -- Headers like "double-click" tokenize with the hyphen as a separate
  -- 'Minus' token, the same wrinkle as the tool-script grammar.
  let kw = tok
      doubleClickKw = kw "double" *> token Minus *> kw "click"

  point <- rule $
        (CartesianPoint <$> (token OpenParen *> value <* token Comma) <*> (value <* token CloseParen))
    <|> (ComplexPoint <$> arith)

  pixel <- rule $
    (,) <$> (token OpenParen *> intLit <* token Comma) <*> (intLit <* token CloseParen)

  rgb <- rule $
    (,,) <$> (token OpenParen *> intLit <* token Comma)
         <*> (intLit <* token Comma)
         <*> (intLit <* token CloseParen)

  selectViewer <- rule $
    SelectViewer <$> (kw "select" *> kw "viewer" *> str <* nl)
  selectTool <- rule $
    SelectTool <$> (kw "select" *> kw "tool" *> str <* nl)
  deselectTool <- rule $
    DeselectTool <$ (kw "deselect" *> kw "tool" <* nl)

  setParameter <- rule $
    SetParameter <$> (kw "set" *> kw "parameter" *> ident') <*> (kw "to" *> value <* nl)
  setToolParameter <- rule $
    SetToolParameter <$> (kw "set" *> kw "tool" *> kw "parameter" *> ident') <*> (kw "to" *> value <* nl)
  setColorizerParameter <- rule $
    SetColorizerParameter <$> (kw "set" *> kw "colorizer" *> kw "parameter" *> ident')
                          <*> (kw "to" *> (asChoice <$> value) <* nl)

  -- "800x600" tokenizes as a single number followed by one identifier
  -- token "x600" (identifiers here may contain digits after the first
  -- letter), not separate "x" and "600" tokens -- so the height has to
  -- be pulled out of that identifier directly rather than matched as
  -- its own token.
  xHeight <- rule
    (tokenMatch (\case
        Identifier ('x' : rest@(_:_)) | all isDigit rest -> Just (read rest)
        _ -> Nothing)
      <?> "x<height>")

  resizeViewer <- rule $
    ResizeViewer <$> (kw "resize" *> kw "viewer" *> kw "to" *> intLit) <*> (xHeight <* nl)
  setViewerCenter <- rule $
    SetViewerCenter <$> (kw "set" *> kw "viewer" *> kw "center" *> kw "to" *> point <* nl)
  setViewerPixelSize <- rule $
    SetViewerPixelSize <$> (kw "set" *> kw "viewer" *> kw "pixel" *> kw "size" *> kw "to" *> value <* nl)
  setViewerAspect <- rule $
    SetViewerAspect <$> (kw "set" *> kw "viewer" *> kw "aspect" *> kw "to" *> value <* nl)
  setViewerView <- rule $
    SetViewerView <$> (kw "set" *> kw "viewer" *> kw "view" *> kw "to"
                       *> ((PlaneView <$ kw "plane") <|> (SphereView <$ kw "sphere")) <* nl)

  clickAt <- rule $
    ClickAt <$> (kw "click" *> kw "at" *> point <* nl)
  doubleClickAt <- rule $
    DoubleClickAt <$> (doubleClickKw *> kw "at" *> point <* nl)
  dragFromTo <- rule $
    DragFromTo <$> (kw "drag" *> kw "from" *> point) <*> (kw "to" *> point <* nl)
  pressButton <- rule $
    PressButton <$> (kw "press" *> kw "button" *> str <* nl)
  triggerRefresh <- rule $
    TriggerRefresh <$ (kw "trigger" *> kw "refresh" <* nl)
  triggerTimer <- rule $
    TriggerTimer <$> (kw "trigger" *> kw "timer" *> str <* nl)

  waitForViewer <- rule $
    WaitForViewer <$> (kw "wait" *> kw "for" *> kw "viewer" *> str <* nl)

  captureViewerBase <- rule $
    CaptureViewerBase <$> (kw "capture" *> kw "viewer" *> str) <*> (kw "base" *> kw "to" *> str <* nl)
  captureViewer <- rule $
    CaptureViewer <$> (kw "capture" *> kw "viewer" *> str) <*> (kw "to" *> str <* nl)
  captureViewerAsPreview <- rule $
    CaptureViewerAsPreview <$> (kw "capture" *> kw "viewer" *> str)
                           <*> (isJust <$> optional (kw "base") <* kw "as" <* kw "preview" <* nl)

  assertPixel <- rule $
    AssertPixel
      <$> (kw "assert" *> kw "viewer" *> str)
      <*> (kw "pixel" *> pixel)
      <*> (kw "is" *> rgb)
      <*> (optional (kw "within" *> intLit) <* nl)

  points <- rule ((:) <$> point <*> many (token Comma *> point))
  corner <- rule $
        (kw "top" *> ((TopLeft <$ kw "left") <|> (TopRight <$ kw "right")))
    <|> (kw "bottom" *> ((BottomLeft <$ kw "left") <|> (BottomRight <$ kw "right")))
  movieOptions <- rule $
    MovieOptions <$> (kw "frames" *> intLit) <*> (kw "fps" *> intLit)
                 <*> (isJust <$> optional (kw "loop")) <*> (isJust <$> optional (kw "base"))
                 <*> optional ((,) <$> (kw "inset" *> str)
                                   <*> (fromMaybe BottomRight <$> optional (kw "at" *> corner)))
  coordinate <- rule $
        (ComplexCoordinate <$> ident')
    <|> (RealCoordinates <$> (token OpenParen *> ident' <* token Comma) <*> (ident' <* token CloseParen))
  movieAnimation <- rule $
        (MovieScript <$> (kw "using" *> str))
    <|> (MoviePath <$> (kw "moving" *> coordinate) <*> (kw "along" *> points))
  recordMovie <- rule $
    RecordMovie <$> (kw "record" *> kw "viewer" *> str) <*> (kw "to" *> str)
                <*> movieOptions <*> (movieAnimation <* nl)

  command <- ruleChoice
    [ selectViewer, selectTool, deselectTool
    , setParameter, setToolParameter, setColorizerParameter
    , resizeViewer, setViewerCenter, setViewerPixelSize, setViewerAspect, setViewerView
    , clickAt, doubleClickAt, dragFromTo
    , pressButton, triggerRefresh, triggerTimer
    , waitForViewer
    , captureViewerBase, captureViewer, captureViewerAsPreview

    , assertPixel
    , recordMovie
    ]

  rule (token Indent *> many command <* token Dedent)

-- | A text literal, as the text of a field (a choice's label, say); any
-- other value, as a value.
asChoice :: ParsedValue -> Either String ParsedValue
asChoice pv = maybe (Right pv) Left (textLiteral pv)
