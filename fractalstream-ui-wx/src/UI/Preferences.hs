-- | The Preferences window, for FractalStream's own settings
-- ("FractalStream.Settings"). Each change is saved as soon as it's made.
module UI.Preferences
  ( preferencesWindow
  ) where

import FractalStream.Prelude hiding (get)

import Data.DynamicValue
import FractalStream.Library (projectsDirectory)
import FractalStream.Settings
import UI.EditorKeys (emacsKeysByDefault)
import UI.EditorStyle (Style(..), classStyle, defaultClassStyle, editorBackground)
import UI.CodeEditor (frameIsLight)
import Language.Editor.Highlight (Highlight, highlightName)
import qualified Language.Editor.Highlight as H
import qualified Graphics.UI.WX as WX
import qualified Data.Map as Map
import Numeric (showHex, readHex)

import Graphics.UI.WX hiding (pt, when, Color, Object, Layout, Vertical, Horizontal)
import Graphics.UI.WXCore (windowRaise)
import System.IO (hPutStrLn, stderr)
import Control.Exception (IOException, try)

-- | Show a new Preferences window for the settings in the given
-- variable, returning it.
preferencesWindow :: Variable Settings -> IO (Frame ())
preferencesWindow settingsVar = do
  f <- frame [ text := "Preferences", resizeable := True ]
  p <- panel f []
  -- a tab for each kind of setting
  nb <- notebook p []
  libraryTab <- panel nb []
  editorTab <- panel nb []
  colorsTab <- panel nb []

  folderText <- staticText libraryTab [ text := "" ]
  chooseButton <- button libraryTab [ text := "Choose..." ]
  useDefaultButton <- button libraryTab [ text := "Use the default" ]
  note <- staticText libraryTab [ text := "Changing the folder doesn't move any projects: it only \
                                          \changes where the library browser looks for them." ]
  fontText <- staticText editorTab [ text := "" ]
  chooseFontButton <- button editorTab [ text := "Choose..." ]
  defaultFontButton <- button editorTab [ text := "Use the default" ]
  -- On macOS, the script editor's emacs keys are always on (see
  -- "UI.EditorKeys"); elsewhere they take over Control shortcuts, so
  -- they're up to the user.
  symbolsBox <- checkBox editorTab [ text := "Write scripts with mathematical symbols \
                                     \(\x27F5, \x211D, z\xB2, ...) as they're edited" ]
  numbersBox <- checkBox editorTab [ text := "Show line numbers" ]
  foldBox <- checkBox editorTab [ text := "Fold blocks of code, not just stanzas and tool handlers" ]
  emacsBox <- if emacsKeysByDefault then pure Nothing else
    Just <$> checkBox editorTab [ text := "Emacs-style movement keys (Control-A, Control-E, ...)" ]

  -- The Colors tab: for light windows or dark ones (the kind this one
  -- is, to start with), each class's sample, and buttons and boxes for
  -- changing it.
  isLight0 <- frameIsLight f
  appearance <- choice colorsTab [ items := ["Light windows", "Dark windows"]
                                 , selection := if isLight0 then 0 else 1 ]
  resetAllButton <- button colorsTab [ text := "Reset all" ]
  classRows <- forM [minBound .. maxBound] $ \h -> do
    samplePanel <- panel colorsTab []
    sample <- staticText samplePanel [ text := classLabel h ]
    set samplePanel [ layout := margin 3 (widget sample) ]
    colorButton <- button colorsTab [ text := "Color..." ]
    backgroundButton <- button colorsTab [ text := "Background..." ]
    boldBox <- checkBox colorsTab [ text := "Bold" ]
    italicBox <- checkBox colorsTab [ text := "Italic" ]
    underlineBox <- checkBox colorsTab [ text := "Underline" ]
    resetButton <- button colorsTab [ text := "Reset" ]
    pure (h, samplePanel, sample, colorButton, backgroundButton, boldBox, italicBox, underlineBox, resetButton)

  let showSettings = do
        settings <- getDynamic settingsVar
        folder <- projectsDirectory settings
        set folderText [ text := folder ++ maybe " (the default)" (const "") (setProjectsFolder settings) ]
        set useDefaultButton [ enabled := isJust (setProjectsFolder settings) ]
        forM_ emacsBox $ \box -> set box [ checked := setEmacsKeys settings ]
        set foldBox [ checked := setFoldCodeBlocks settings ]
        set numbersBox [ checked := setLineNumbers settings ]
        set symbolsBox [ checked := setMathSymbols settings ]
        let custom = isJust (setEditorFont settings) || isJust (setEditorFontSize settings)
        set fontText [ text := editorFont settings
                                 ++ maybe "" (\n -> ", " ++ show n ++ " pt") (setEditorFontSize settings)
                                 ++ (if custom then "" else " (the default)") ]
        set defaultFontButton [ enabled := custom ]
        showColors settings
        windowReLayout f
      showColors settings = do
        isLight <- (== 0) <$> get appearance selection
        let changes = (if isLight then setEditorColorsLight else setEditorColorsDark) settings
        set resetAllButton [ enabled := not (Map.null changes) ]
        forM_ classRows $ \(h, samplePanel, sample, _, _, boldBox, italicBox, underlineBox, resetButton) -> do
          let Style{..} = classStyle isLight settings h
          set samplePanel [ bgcolor := hexColor (fromMaybe (editorBackground isLight) stBackground) ]
          set sample [ color := hexColor stColor
                     , bgcolor := hexColor (fromMaybe (editorBackground isLight) stBackground)
                     , font := fontFixed { _fontFace = editorFont settings
                                         , _fontWeight = if stBold then WeightBold else WeightNormal
                                         , _fontShape = if stItalic then ShapeItalic else ShapeNormal
                                         , _fontUnderline = stUnderline } ]
          set boldBox [ checked := stBold ]
          set italicBox [ checked := stItalic ]
          set underlineBox [ checked := stUnderline ]
          set resetButton [ enabled := Map.member (highlightName h) changes ]
      -- change one class's style, for the windows being shown
      changeClass h k = do
        isLight <- (== 0) <$> get appearance selection
        let tidy cs = if cs == noClassStyle then Nothing else Just cs
            alter = Map.alter (tidy . k . fromMaybe noClassStyle) (highlightName h)
        change $ \s -> if isLight then s { setEditorColorsLight = alter (setEditorColorsLight s) }
                                  else s { setEditorColorsDark = alter (setEditorColorsDark s) }
      change k = do
        modifyValue settingsVar k
        settings <- getDynamic settingsVar
        try (saveSettings settings) >>= \case
          Left (e :: IOException) -> do
            hPutStrLn stderr ("Couldn't save the settings: " ++ show e)
            errorDialog f "Couldn't save the settings" (show e)
          Right () -> pure ()
        showSettings

  set chooseButton [ on command := do
                       current <- projectsDirectory =<< getDynamic settingsVar
                       dirOpenDialog f True "Choose the folder for your own projects" current
                         >>= mapM_ (\folder -> change (\s -> s { setProjectsFolder = Just folder })) ]
  set useDefaultButton [ on command := change (\s -> s { setProjectsFolder = Nothing }) ]
  set chooseFontButton [ on command := do
                           settings <- getDynamic settingsVar
                           let current = fontDefault { _fontFace = editorFont settings
                                                     , _fontSize = fromMaybe 12 (setEditorFontSize settings) }
                           fontDialog f current >>= mapM_ (\chosen ->
                             change (\s -> s { setEditorFont = Just (_fontFace chosen)
                                             , setEditorFontSize = Just (_fontSize chosen) })) ]
  set defaultFontButton [ on command := change (\s -> s { setEditorFont = Nothing, setEditorFontSize = Nothing }) ]
  set appearance [ on select := getDynamic settingsVar >>= showColors ]
  set resetAllButton [ on command := do
                         isLight <- (== 0) <$> get appearance selection
                         change (\s -> if isLight then s { setEditorColorsLight = Map.empty }
                                                 else s { setEditorColorsDark = Map.empty }) ]
  forM_ classRows $ \(h, _, _, colorButton, backgroundButton, boldBox, italicBox, underlineBox, resetButton) -> do
    -- (on macOS the picker has no Cancel: closing it gives back the color
    -- it was showing, so picking the class's own color is no change)
    let pick current k = do
          isLight <- (== 0) <$> get appearance selection
          settings <- getDynamic settingsVar
          colorDialog f (hexColor (current (classStyle isLight settings h)))
            >>= mapM_ (\c -> do
                  let picked = colorHex c
                      own = current (defaultClassStyle isLight h)
                  changeClass h (k (if picked == own then Nothing else Just picked)))
        -- a flag that's the class's own again is no change at all
        flag field setField box = set box [ on command := do
          on' <- get box checked
          isLight <- (== 0) <$> get appearance selection
          let own = field (defaultClassStyle isLight h)
          changeClass h (setField (if on' == own then Nothing else Just on')) ]
    set colorButton [ on command := pick stColor (\c cs -> cs { csColor = c }) ]
    set backgroundButton [ on command := do
                             isLight <- (== 0) <$> get appearance selection
                             pick (fromMaybe (editorBackground isLight) . stBackground)
                                  (\c cs -> cs { csBackground = c }) ]
    flag stBold (\v cs -> cs { csBold = v }) boldBox
    flag stItalic (\v cs -> cs { csItalic = v }) italicBox
    flag stUnderline (\v cs -> cs { csUnderline = v }) underlineBox
    set resetButton [ on command := changeClass h (const noClassStyle) ]

  set symbolsBox [ on command := do
                     on' <- get symbolsBox checked
                     change (\s -> s { setMathSymbols = on' }) ]
  set numbersBox [ on command := do
                     on' <- get numbersBox checked
                     change (\s -> s { setLineNumbers = on' }) ]
  set foldBox [ on command := do
                  on' <- get foldBox checked
                  change (\s -> s { setFoldCodeBlocks = on' }) ]
  forM_ emacsBox $ \box ->
    set box [ on command := do
                on' <- get box checked
                change (\s -> s { setEmacsKeys = on' }) ]

  -- The folder's path can grow and wrap sideways; the buttons and the
  -- note keep their size. (See CLAUDE_NOTES.md on wxHaskell's layout.)
  set f [ layout := container p $ margin 12 $ tabs nb
            [ tab "Library" $ container libraryTab $ margin 12 $ column 8
                [ label "My projects folder:"
                , hfill (widget folderText)
                , row 6 [ widget chooseButton, widget useDefaultButton, hglue ]
                , hstretch (widget note)
                , glue
                ]
            , tab "Editor" $ container editorTab $ margin 12 $ column 8 $
                [ label "Font for scripts:"
                , row 6 [ widget fontText, widget chooseFontButton, widget defaultFontButton, hglue ]
                , widget numbersBox
                , widget symbolsBox
                , widget foldBox
                ] ++
                [ widget box | Just box <- [emacsBox] ] ++
                [ glue ]
            , tab "Colors" $ container colorsTab $ margin 12 $ column 8
                [ row 6 [ label "Colors for", widget appearance, hglue, widget resetAllButton ]
                , grid 8 4
                    [ [ fill (widget samplePanel), widget colorButton, widget backgroundButton
                      , widget boldBox, widget italicBox, widget underlineBox, widget resetButton ]
                    | (_, samplePanel, _, colorButton, backgroundButton, boldBox, italicBox, underlineBox, resetButton) <- classRows ]
                , glue
                ]
            ]
        ]
  showSettings
  windowRaise f
  pure f

-- | What the Colors tab calls a class.
classLabel :: Highlight -> String
classLabel = \case
  H.Keyword -> "keywords"
  H.BuiltinFunction -> "built-in functions"
  H.BuiltinValue -> "built-in values"
  H.TypeName -> "types"
  H.ColorName -> "color names"
  H.NumberLiteral -> "numbers"
  H.StringLiteral -> "\"text\""
  H.Comment -> "# comments"
  H.Operator -> "operators"
  H.StanzaHeader -> "@stanza headers"
  H.Parameter -> "parameters"
  H.Variable -> "variables"
  H.Function -> "functions"
  H.CodeBlock -> "code blocks"
  H.Undefined -> "undefined names"

-- | A color written @#rrggbb@, and back.
hexColor :: String -> WX.Color
hexColor s = case s of
  '#' : r1 : r2 : g1 : g2 : b1 : b2 : _ -> rgb (hex [r1, r2]) (hex [g1, g2]) (hex [b1, b2] :: Int)
  _ -> rgb 0 0 (0 :: Int)
  where hex ds = case readHex ds of
          (n, _) : _ -> n
          [] -> 0

colorHex :: WX.Color -> String
colorHex c = '#' : concatMap two [colorRed c, colorGreen c, colorBlue c :: Int]
  where two n = let h = showHex n "" in if length h < 2 then '0' : h else h
