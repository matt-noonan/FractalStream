-- | How code editors color each class of span ("Language.Editor.Highlight"):
-- a Scintilla style for each class, with colors for light and dark
-- windows. Every class has a style of its own, though some share colors
-- by default.
module UI.EditorStyle
  ( styleFor
  , setupStyles
  , showLineNumbers
  ) where

import FractalStream.Prelude hiding (get)

import Language.Editor.Highlight (Highlight(..))

import Graphics.UI.WX hiding (Vertical, Horizontal, when)
import Graphics.UI.WXCore.WxcClasses

-- | The Scintilla style for a class. (0 is plain text; 32 and up are
-- Scintilla's own: the default style, line numbers, ...)
styleFor :: Highlight -> Int
styleFor h = 1 + fromEnum h

-- | Set up an editor's styles, for a light window or a dark one, in the
-- given font (and size, if not the editor's usual one): every style in
-- the same font, and each class's colors on top of that. This can be
-- done again, to change the font.
setupStyles :: StyledTextCtrl a -> Bool -> String -> Maybe Int -> IO ()
setupStyles stc isLight face points = do
  let (plain, background, lineNumbers) =
        if isLight then ("#000000", "#f8f8f8", "fore:#a0a098,back:#f0f0e8")
                   else ("#dbdbdb", "#14191e", "fore:#a0a0a0,back:#101020")
  -- the default style, copied to every other (so they all share its font)
  styledTextCtrlStyleSetSpec stc 32 ("fore:" ++ plain ++ ",back:" ++ background)
  styledTextCtrlStyleSetFaceName stc 32 face
  forM_ points (styledTextCtrlStyleSetSize stc 32)
  styledTextCtrlStyleClearAll stc
  styledTextCtrlStyleSetSpec stc 33 lineNumbers
  forM_ [minBound .. maxBound] $ \h ->
    styledTextCtrlStyleSetSpec stc (styleFor h) (classSpec isLight h)

-- | Show line numbers in an editor's margin, or not. (Its width depends
-- on the font: this goes after 'setupStyles'.)
showLineNumbers :: StyledTextCtrl a -> Bool -> IO ()
showLineNumbers stc on' = do
  -- wide enough for line numbers up to 999, however big the font
  numberWidth <- styledTextCtrlTextWidth stc 33 "_999"
  styledTextCtrlSetMarginWidth stc 0 (if on' then numberWidth + 4 else 0)

-- | A class's colors (and boldness or slant), as a Scintilla style spec.
classSpec :: Bool -> Highlight -> String
classSpec isLight h = if isLight then light else dark
  where
    (light, dark) = case h of
      Keyword         -> ("fore:#7a3e9d,bold", "fore:#c792ea,bold")
      BuiltinFunction -> ("fore:#1f5fbf", "fore:#82aaff")
      BuiltinValue    -> ("fore:#a35200", "fore:#f78c6c")
      TypeName        -> ("fore:#0a7a6a", "fore:#7fdbca")
      ColorName       -> ("fore:#a35200", "fore:#f78c6c")
      NumberLiteral   -> ("fore:#a35200", "fore:#f78c6c")
      StringLiteral   -> ("fore:#2e7d32", "fore:#c3e88d")
      Comment         -> ("fore:#8080a0,italic", "fore:#8a8fb8,italic")
      Operator        -> ("fore:#606060", "fore:#89ddff")
      StanzaHeader    -> ("fore:#403060,back:#e8e0f8,italic", "fore:#c0c0e8,back:#262040,italic")
      Parameter       -> ("fore:#9a2a5a", "fore:#ffcb6b")
      Variable        -> ("fore:#000000", "fore:#dbdbdb")
      Function        -> ("fore:#1f5fbf", "fore:#82aaff")
      CodeBlock       -> ("fore:#7a3e9d", "fore:#c792ea")
      Undefined       -> ("fore:#000000,italic", "fore:#dbdbdb,italic")
