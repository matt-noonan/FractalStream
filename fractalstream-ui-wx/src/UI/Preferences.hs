-- | The Preferences window, for FractalStream's own settings
-- ("FractalStream.Settings"). Each change is saved as soon as it's made.
module UI.Preferences
  ( preferencesWindow
  ) where

import FractalStream.Prelude hiding (get)

import Data.DynamicValue
import FractalStream.Library (projectsDirectory)
import FractalStream.Settings
import FractalStream.Backends (BackendId, backendLabel, defaultBackendOrder)
import UI.EditorKeys (emacsKeysByDefault)
import UI.EditorStyle (Style(..), classStyle, defaultClassStyle, editorBackground)
import UI.CodeEditor (frameIsLight)
import UI.Widgets (choiceBook, choiceBookPage, choiceBookLayout)
import Language.Editor.Highlight (Highlight, highlightName)
import qualified Language.Editor.Highlight as H
import qualified Graphics.UI.WX as WX
import qualified Data.Map as Map
import Numeric (showHex, readHex)

import Graphics.UI.WX hiding (pt, when, Color, Object, Layout, Vertical, Horizontal)
import Graphics.UI.WXCore (windowRaise, wxHORIZONTAL)
import Graphics.UI.WXCore.WxcObject (withObjectPtr, objectFromPtr)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO (hPutStrLn, stderr)
import Control.Exception (IOException, try)

-- | Show a new Preferences window for the settings in the given
-- variable, returning it, given the backends the app has.
preferencesWindow :: [BackendId] -> Variable Settings -> IO (Frame ())
preferencesWindow available settingsVar = do
  f <- frame [ text := "Preferences", resizeable := True ]
  p <- panel f []
  -- a page for each kind of setting, with a menu of them
  book <- choiceBook Nothing p
  libraryTab <- choiceBookPage book
  editorTab <- choiceBookPage book
  colorsTab <- choiceBookPage book
  viewersTab <- choiceBookPage book

  folderText <- staticText libraryTab [ text := "" ]
  chooseButton <- button libraryTab [ text := "Choose..." ]
  useDefaultButton <- button libraryTab [ text := "Use the default" ]
  fontText <- staticText editorTab [ text := "" ]
  chooseFontButton <- button editorTab [ text := "Choose..." ]
  defaultFontButton <- button editorTab [ text := "Use the default" ]
  -- On macOS, the script editor's emacs keys are always on (see
  -- "UI.EditorKeys"); elsewhere they take over Control shortcuts, so
  -- they're up to the user.
  symbolsBox <- checkBox editorTab [ text := "Mathematical symbols"
                                   , tooltip := "Write scripts with mathematical symbols \
                                                \(\x27F5, \x211D, z\xB2, ...) as they're edited" ]
  numbersBox <- checkBox editorTab [ text := "Show line numbers" ]
  foldBox <- checkBox editorTab [ text := "Code folding" ]
  emacsBox <- if emacsKeysByDefault then pure Nothing else
    Just <$> checkBox editorTab [ text := "Emacs movement keys"
                                , tooltip := "Control-A, Control-E, ... move the cursor, as in Emacs" ]

  -- The Viewers tab: how strongly a sphere viewer shades the globe, in
  -- percent (sphere viewers draw again as it's dragged).
  shadingSlider <- horizontalSlider viewersTab 0 100 defaultSphereShading 240
  shadingText <- staticText viewersTab [ text := "100%" ]
  set shadingSlider [ tooltip := "Shades the globe as if it were lit from the upper left. \
                                 \Use 0% for no shading at all." ]
  -- ...and the order viewers try the backends in (GPU_PLAN.md, G1)
  renderingList <- singleListBox viewersTab
    [ items := [], size := sz 320 80
    , tooltip := "Each viewer is drawn with the first of these that can draw it as \
                 \precisely as the CPU's doubles would (the GPU's double-float can't \
                 \zoom as deep, for instance), and so those after the CPU only when \
                 \it can't. A viewer's menu can choose one for that viewer." ]
  upButton <- button viewersTab [ text := "Move up" ]
  downButton <- button viewersTab [ text := "Move down" ]
  defaultOrderButton <- button viewersTab [ text := "Use the default" ]

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
        set shadingSlider [ selection := setSphereShading settings ]
        set shadingText [ text := show (setSphereShading settings) ++ "%" ]
        chosen <- get renderingList selection
        let order = setRendering settings
            describe b = backendLabel b ++ (if b `elem` available then "" else " (not in this version)")
        set renderingList [ items := map describe order ]
        when (chosen >= 0) $ set renderingList [ selection := chosen ]
        set defaultOrderButton [ enabled := order /= defaultBackendOrder ]
        showRenderingButtons
        let custom = isJust (setEditorFont settings) || isJust (setEditorFontSize settings)
        set fontText [ text := editorFont settings
                                 ++ maybe "" (\n -> ", " ++ show n ++ " pt") (setEditorFontSize settings)
                                 ++ (if custom then "" else " (the default)") ]
        set defaultFontButton [ enabled := custom ]
        showColors settings
        windowReLayout f
      showRenderingButtons = do
        chosen <- get renderingList selection
        n <- length . setRendering <$> getDynamic settingsVar
        set upButton [ enabled := chosen > 0 ]
        set downButton [ enabled := chosen >= 0 && chosen < n - 1 ]
      -- move the chosen backend by one place (and keep it chosen)
      moveBackend by = do
        chosen <- get renderingList selection
        order <- setRendering <$> getDynamic settingsVar
        let to = chosen + by
        when (chosen >= 0 && to >= 0 && to < length order) $ do
          let swapped = [ if k == chosen then order !! to else if k == to then order !! chosen else b
                        | (k, b) <- zip [0 ..] order ]
          set renderingList [ selection := to ]
          change (\s -> s { setRendering = swapped })
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

  set shadingSlider [ on command := do
                        v <- get shadingSlider selection
                        current <- setSphereShading <$> getDynamic settingsVar
                        when (v /= current) $ change (\s -> s { setSphereShading = v }) ]
  set renderingList [ on select := showRenderingButtons ]
  set upButton [ on command := moveBackend (-1) ]
  set downButton [ on command := moveBackend 1 ]
  set defaultOrderButton [ on command := change (\s -> s { setRendering = defaultBackendOrder }) ]
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
  pages <- choiceBookLayout book
            [ ("Library", libraryTab, margin 12 $ column 8
                [ label "My projects folder:"
                , hfill (widget folderText)
                , row 6 [ widget chooseButton, widget useDefaultButton, hglue ]
                , glue
                ])
            , ("Editor", editorTab, margin 12 $ column 8 $
                [ label "Font for scripts:"
                , row 6 [ widget fontText, widget chooseFontButton, widget defaultFontButton, hglue ]
                , widget numbersBox
                , widget symbolsBox
                , widget foldBox
                ] ++
                [ widget box | Just box <- [emacsBox] ] ++
                [ glue ])
            , ("Viewers", viewersTab, margin 12 $ column 8
                [ label "Shading of the Riemann sphere:"
                , row 6 [ hfill (widget shadingSlider), widget shadingText ]
                , label "Draw viewers with, in order:"
                , row 6 [ minsize (sz 260 90) (widget renderingList)
                        , column 6 [ widget upButton, widget downButton, widget defaultOrderButton ], hglue ]
                , glue
                ])
            , ("Colors", colorsTab, margin 12 $ column 8
                [ row 6 [ label "Colors for", widget appearance, hglue, widget resetAllButton ]
                , grid 8 4
                    [ [ fill (widget samplePanel), widget colorButton, widget backgroundButton
                      , widget boldBox, widget italicBox, widget underlineBox, widget resetButton ]
                    | (_, samplePanel, _, colorButton, backgroundButton, boldBox, italicBox, underlineBox, resetButton) <- classRows ]
                , glue
                ])
            ]
  set f [ layout := container p $ margin 12 pages ]
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

-- | A horizontal slider from a minimum to a maximum, starting at a value,
-- of a width (in pixels). Not wxHaskell's 'hslider': its binding gets the
-- slider's style wrong on arm64 macOS (the style, a C @long@, is the
-- tenth argument, so it's passed on the stack, where it was read with
-- garbage in it), and the slider came out vertical, or inverted, with
-- labels. This calls wxc's @wxSlider_Create@ with its C types exactly.
horizontalSlider :: Window a -> Int -> Int -> Int -> Int -> IO (Slider ())
horizontalSlider owner lo hi initial width = withObjectPtr owner $ \p ->
  objectFromPtr . castPtr <$>
    wxcSliderCreate (castPtr p) (-1) (fromIntegral initial) (fromIntegral lo) (fromIntegral hi)
                    (-1) (-1) (fromIntegral width) (-1) (fromIntegral wxHORIZONTAL)

foreign import ccall unsafe "wxSlider_Create"
  wxcSliderCreate :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CLong -> IO (Ptr ())
