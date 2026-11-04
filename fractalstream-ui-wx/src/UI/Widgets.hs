module UI.Widgets
  ( wxTimer
  , onOwnDestroy
  , wxWatchDynamic1
  , wxWatchDynamic
  , checkboxWidget
  , selectionWidget
  , plainTextWidget
  , buttonWidget
  , colorWidget
  , expressionWidget
  , scriptWidget
  , scriptWidgetWith
  , scriptEditMenu
  , normalizeEditor
  ) where

import FractalStream.Prelude hiding (get)

import Language.Type
import Data.DynamicValue
import Data.Color hiding (black, white)
import Actor.Layout
import Language.Parser.SourceRange (SourceRange, spanOfSourceRange, SourceSpan(..))
import Language.Editor.Highlight (Highlight, highlightsWith)
import Language.Editor.Normalize (normalizations)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as Utf8
import UI.EditorStyle (styleFor)

import UI.CodeEditor
import UI.AppSettings (appSettings)
import FractalStream.Settings (Settings(..), editorFont)
import UI.EditorStyle (setupStyles, showLineNumbers)
import UI.EditorKeys (emacsKeysByDefault, pasteFittingIndentation, toggleCommentLines)

import Graphics.UI.WX hiding (pt, when, grey, Color)
import Graphics.UI.WXCore.Events
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassesMZ
import Graphics.UI.WXCore.Frame (windowGetFrameParent)
import Graphics.UI.WXCore.WxcClassTypes (ColourPickerCtrl, WxObject)
import Control.Concurrent
import Data.Char (toUpper)
import Data.IORef
import Text.Printf
import qualified Data.Map as Map

simpleDialog :: (MonadIO io, Widget (Window w), Eq t)
             => String
             -> Maybe String
             -> (Window w -> IO t)
             -> (forall b. Window b -> t -> IO (Window w))
             -> Window a
             -> Variable t
             -> io ()
simpleDialog title m'label onOk makeControl p v = liftIO $ do
  editDialog <- dialog p [ text := title, resizeable := True ]
  ep <- panel editDialog []
  v0 <- getDynamic v
  control <- makeControl ep v0
  let widgetRow = case m'label of
        Nothing -> fill (widget control)
        Just l  -> row 5 [ label l, fill (widget control) ]

  epOk <- button ep [ text := "Ok" ]
  epCancel <- button ep [ text := "Cancel" ]
  set editDialog [ layout := margin 10 $ fill $ container ep $ column 5
                 [ widgetRow
                 , row 5 [ margin 3 $ widget epCancel, hglue, margin 3 $ widget epOk ]]]
  windowSetFocus control
  change <- fmap isJust $ showModal editDialog $ \endEdit -> do
    set epOk [ on command := endEdit (Just ()) ]
    set epCancel [ on command := endEdit Nothing ]
  when change $ liftIO (onOk control >>= setValue v)
  void (windowClose editDialog True)


editLabel :: MonadIO io => Window a -> Variable Label -> io ()
editLabel = simpleDialog "Edit label" (Just "Label: ")
  (\w -> Label <$> get w text)
  (\ep (Label lab) -> textEntry ep [ text := lab ])

editString :: MonadIO io => String -> Window a -> Variable String -> io ()
editString s = simpleDialog ("Edit " ++ s) (Just $ uppercase s ++ ": ")
  (\w -> get w text)
  (\ep var -> textEntry ep [ text := var ])
 where
   uppercase = \case
     (c:cs) -> toUpper c : cs
     ""     -> ""

editMultilineText :: MonadIO io => Window a -> Variable String -> io ()
editMultilineText = simpleDialog "Edit text" Nothing
  (\w -> get w text)
  (\ep txt -> textCtrl ep [ text := txt ])

buttonWidget :: MonadIO io => Window a -> Variable String -> io (Button ())
buttonWidget p s = liftIO $ do
  txt0 <- getDynamic s
  btn <- button p [ text := txt0 ]
  editMenu <- menuPane [ text := "" ]
  menuItem editMenu [ text := "Edit button label...", on command := editString "button label" btn s ]
  set btn [ on clickRight := \pt -> menuPopup editMenu pt btn ]
  wxWatchDynamic1 p s $ \newText -> set btn [ text := newText ]
  pure btn

checkboxWidget :: MonadIO io => Window a -> Variable Label -> Parsed String -> Variable Bool -> io (CheckBox ())
checkboxWidget p l v b = liftIO $ do
  Label lab0 <- getDynamic l
  initial <- getDynamic b

  cb <- checkBox p [ text := lab0, checkable := True, checked := initial ]

  editMenu <- menuPane [ text := "" ]
  var <- getDynamic (source v)
  info <- menuItem editMenu [ text := var ++ " : Boolean" ]
  wxWatchDynamic1 cb (source v) $ \newVar -> set info [ text := newVar ++ " : Boolean" ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit label...", on command := editLabel cb l ]
  menuItem editMenu [ text := "Edit variable...", on command := editString "variable name" cb (source v) ]

  set cb [ on command := do
             isChecked <- get cb checked
             void (setValue b isChecked)
         , on clickRight := \pt -> menuPopup editMenu pt cb ]

  wxWatchDynamic1 cb b (\isChecked -> set cb [ checked := isChecked ])
  wxWatchDynamic1 cb l (\(Label lab) -> set cb [ text := lab ])
  pure cb

selectionWidget :: MonadIO io => Window a -> Parsed String -> Variable Int64 -> Variable [String] -> io (Choice ())
selectionWidget p v pick options = liftIO $ do
  ix0' <- getDynamic pick
  opts0 <- getDynamic options
  let ix0 = if fromIntegral ix0' >= length opts0 then 0 else ix0'
  c <- choice p [ selection := fromIntegral ix0, items := opts0 ]

  editMenu <- menuPane [ text := "" ]
  var <- getDynamic (source v)
  info <- menuItem editMenu [ text := var ++ " : ℤ" ]
  wxWatchDynamic1 c (source v) $ \newVar -> set info [ text := newVar ++ " : ℤ" ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit variable...", on command := editString "variable name" c (source v) ]

  set c [ on select := do
            newIx <- fromIntegral <$> get c selection
            setValue pick newIx
        , on clickRight := \pt -> menuPopup editMenu pt c ]

  wxWatchDynamic1 c pick $ \newIx -> do
    numOpts <- length <$> get c items
    if fromIntegral newIx >= numOpts
      then set c [ enabled := False ]
      else set c [ enabled := True, selection := fromIntegral newIx ]
  pure c

plainTextWidget :: MonadIO io => Window a -> Variable String -> io (StaticText ())
plainTextWidget p s = liftIO $ do
  txt <- staticText p [ text := "" ]
  editMenu <- menuPane [ text := "" ]
  menuItem editMenu [ text := "Edit text...", on command := editMultilineText txt s ]
  set txt [ on clickRight := \pt -> menuPopup editMenu pt txt ]
  wxWatchDynamic1 p s $ \newText -> set txt [ text := newText ]
  pure txt

colorWidget :: MonadIO io
            => Window a
            -> Variable Label
            -> Parsed String
            -> Parsed Color
            -> io (StaticText (), ColourPickerCtrl ())
colorWidget p l v col = liftIO $ do
  (r0, g0, b0) <- colorToRGB . fromRight grey <$> getDynamic col
  picker <- feed2 [ visible := True ] 0 $
    initialWindow $ \iD rect' ps s -> do
      e <- colorPickerCtrlCreate p iD (rgb r0 g0 b0) rect' s
      set e ps
      pure e
  let newPick = do
        c <- colorPickerCtrlGetColour picker
        let r = fromIntegral (colorRed   c :: Word8) / 255.0 :: Double
            g = fromIntegral (colorGreen c :: Word8) / 255.0 :: Double
            b = fromIntegral (colorBlue  c :: Word8) / 255.0 :: Double
        setValue (source col) (printf "rgb(%0.3f, %0.3f, %0.3f)" r g b :: String)

  wxWatchDynamic1 p col $ \case
    Left _ -> pure ()
    Right newColor -> do
      let (r,g,b) = colorToRGB newColor
      colorPickerCtrlSetColour picker (rgb r g b)
  windowOnEvent picker [wxEVT_COMMAND_COLOURPICKER_CHANGED] newPick (const newPick)

  editMenu <- menuPane [ text := "" ]
  var <- getDynamic (source v)
  info <- menuItem editMenu [ text := var ++ " : Color" ]
  wxWatchDynamic1 picker (source v) $ \newVar -> set info [ text := newVar ++ " : Color" ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit label...", on command := editLabel picker l ]
  menuItem editMenu [ text := "Edit variable..."
                    , on command := editString "variable name" picker (source v) ]

  Label lab0 <- getDynamic l
  labelTxt <- liftIO $ staticText p [ text := lab0 ]
  wxWatchDynamic1 p l $ \(Label lab) -> do
    set labelTxt [ text := lab ]
    windowReLayout p

  set picker   [ on clickRight := \pt -> menuPopup editMenu pt picker ]
  set labelTxt [ on clickRight := \pt -> menuPopup editMenu pt labelTxt ]

  pure (labelTxt, picker)

expressionWidget :: MonadIO io
                 => Window a
                 -> Variable Label
                 -> UIVariable
                 -> io (StaticText (), TextCtrl (), StaticText ())
expressionWidget = genericExpressionWidget False

genericExpressionWidget :: MonadIO io
                        => Bool
                        -> Window a
                        -> Variable Label
                        -> UIVariable
                        -> io (StaticText (), TextCtrl (), StaticText ())
genericExpressionWidget _canEditEnvironment p l UIVariable{..} = liftIO $ do
  errorMessage <- staticText p [ text := "" ]

  initial <- getDynamic (source exprValue)
  te <- textEntry p [ text := initial
                    , processEnter := True
                    , tooltip := ""
                    ]
  normalBG <- get te bgcolor

  -- TODO: only refresh if the value has changed
  let setErrorMessage = \case
        Nothing -> do
          set te [ bgcolor := normalBG ]
          set errorMessage [ text := "" ]
          windowReLayout p
        Just err -> do
          set te [ bgcolor := rgb 180 80 (80 :: Int)]
          set errorMessage [ text :=  "⚠️", tooltip := err
                           , on click := \_ -> do
                               tw <- tipWindowCreate p "" 1000
                               textCol <- frameIsLight tw <&> \case
                                 True  -> black
                                 False -> white
                               _ <- staticText tw [ text := err
                                                  , font := fontFixed
                                                  , fontSize := 12
                                                  , color := textCol ]
                               windowReLayout tw
                               windowRefresh tw True
                           ]
          windowReLayout p

  set te [ on command := do
             newText <- get te text
             setValue (source exprValue) newText
             getDynamic exprValue >>= \case
               Left msg -> setErrorMessage (Just msg)
               _        -> setErrorMessage Nothing
         , on focus := \case
             True -> propagateEvent
             False -> do
               newText <- get te text
               oldText <- getDynamic (source exprValue)
               when (newText /= oldText) $ do
                 setValue (source exprValue) newText
                 getDynamic exprValue >>= \case
                   Left msg -> setErrorMessage (Just msg)
                   _        -> setErrorMessage Nothing
               propagateEvent
         ]
  wxWatchDynamic1 te (source exprValue) (\newText -> set te [ text := newText ])

  editMenu <- menuPane [ text := "" ]
  info <- menuItem editMenu [ text := "--" ]
  wxWatchDynamic1 te exprName $ \newVar -> do
    let var = fromRight "???" newVar
    ty <- either (const "???") (\(SomeType t) -> ppType t) <$> getDynamic exprType
    set info [ text := var ++ " : " ++ ty ]
  wxWatchDynamic1 te exprType $ \newTy -> do
    var <- fromRight "???" <$> getDynamic exprName
    let ty = either (const "???") (\(SomeType t) -> ppType t) newTy
    set info [ text := var ++ " : " ++ ty ]
  menuLine editMenu
  menuItem editMenu [ text := "Edit label...", on command := editLabel te l ]
  menuItem editMenu [ text := "Edit variable..."
                    , on command := editString "variable name" te (source exprName) ]
  menuItem editMenu [ text := "Edit type..."
                    , on command := editString "type" te (source exprType) ]

  labelTxt <- staticText p [ text := "" ]
  wxWatchDynamic1 p l $ \(Label lab) -> do
    set labelTxt [ text := lab ]
    windowReLayout p

  set labelTxt [ on clickRight := \pt -> menuPopup editMenu pt labelTxt ]
  set te       [ on clickRight := \pt -> menuPopup editMenu pt te ]

  pure (labelTxt, te, errorMessage)

-- | Everything about editing a piece of unified-script text that comes
-- "for free" from a reactive 'Mapped': live syntax coloring (comments)
-- and an inline error indicator at the first error's source span, kept
-- in sync as the text changes. Any pure text -> @'Either' ('SourceRange',
-- 'String') t@ check can drive it.
scriptWidget :: MonadIO io
             => Window a
             -> Mapped CodeString (Either (SourceRange, String) t)
             -> io (StyledTextCtrl (), StaticText ())
scriptWidget = scriptWidgetWith Map.empty

-- | 'scriptWidget', for a script that can refer to names it doesn't
-- define itself (the project's parameters, in a movie's script): what
-- they are, for coloring them.
scriptWidgetWith :: MonadIO io
                 => Map String Highlight
                 -> Window a
                 -> Mapped CodeString (Either (SourceRange, String) t)
                 -> io (StyledTextCtrl (), StaticText ())
scriptWidgetWith known p scriptCode = do
  ce <- liftIO $ codeEditor p scriptCode
  txt <- unCodeString <$> getDynamic (source scriptCode)
  lastText <- liftIO $ newIORef txt
  errorText <- liftIO $ staticText p [ text := "" ]

  -- Writing the script with mathematical symbols ("Language.Editor.
  -- Normalize"), with the math-symbols setting: once it's been edited
  -- here (so that just opening a script doesn't change it), and only
  -- text that has just checked out. The cursor's line is left until the
  -- cursor leaves it, or the editor loses focus, so that what's being
  -- typed isn't rewritten under it (z^2 would become z², and then
  -- typing 0 would make z²0, not z^20).
  edited <- liftIO $ newIORef False
  focused <- liftIO $ newIORef False
  checkedText <- liftIO $ newIORef Nothing
  caretLine <- liftIO $ newIORef (0 :: Int)
  let currentLine = styledTextCtrlGetCurrentPos ce >>= styledTextCtrlLineFromPosition ce
      normalizeExcept skip = do
        symbols <- setMathSymbols <$> getDynamic appSettings
        isEdited <- readIORef edited
        good <- readIORef checkedText
        code <- styledTextCtrlGetText ce
        when (symbols && isEdited && good == Just code) $ do
          let starts = scanl (\off l -> off + length l + 1) 0 (lines code)
              lineOf i = length (takeWhile (<= i) starts) - 1
              edits = [ e | e@(from, _, _) <- normalizations code, Just (lineOf from) /= skip ]
          unless (null edits) $ applyEdits ce code edits

  -- every span colored by what it is ("Language.Editor.Highlight"),
  -- everything else as plain text
  let doSyntaxColoring = do
        code <- styledTextCtrlGetText ce
        let m = editorOffsetMap code
        docLen <- styledTextCtrlGetLength ce
        let byteAt i = Map.findWithDefault docLen i m
        styledTextCtrlStartStyling ce 0 0
        styledTextCtrlSetStyling ce docLen 0
        forM_ (highlightsWith known code) $ \(from, to, h) -> do
          let start' = byteAt from
          styledTextCtrlStartStyling ce start' 0
          styledTextCtrlSetStyling ce (byteAt to - start') (styleFor h)
        pure m

  liftIO $ set ce $
    [ on focus := \tf -> do
        case tf of
          True -> do
            writeIORef focused True
            styledTextCtrlGetText ce >>= writeIORef lastText
          False -> do
            writeIORef focused False
            new <- styledTextCtrlGetText ce
            old <- readIORef lastText
            when (new /= old) $ do
              writeIORef lastText new
              writeIORef edited True
              void (setValue (source scriptCode) (CodeString new))
            normalizeExcept Nothing
        propagateEvent
    ]

  -- Check for script changes periodically. TODO: make this event-driven instead
  --
  -- Owned by the enclosing frame, not `p`: 'wxTimer's cleanup hooks
  -- into the owner's "closing" event, which wxWidgets only ever
  -- generates for a top-level window (a frame/dialog), never for a
  -- plain panel like `p`, regardless of what happens to its parent --
  -- a panel-owned timer's cleanup can't ever fire, so it leaks and
  -- keeps polling this now-destroyed StyledTextCtrl every 200ms for
  -- as long as the process runs, every time the enclosing window
  -- (and so this widget) is closed.
  enclosingFrame <- liftIO (windowGetFrameParent p)
  void $ wxTimer enclosingFrame [ interval := 200, enabled := True
                   , on command := do
                       new <- styledTextCtrlGetText ce
                       old <- readIORef lastText
                       when (new /= old) $ do
                         writeIORef lastText new
                         writeIORef edited True
                         updateFoldLevels ce new
                         setValue (source scriptCode) (CodeString new)
                       -- the cursor's left a line: it can be normalized now
                       lineNow <- currentLine
                       lineBefore <- readIORef caretLine
                       when (lineNow /= lineBefore) $ do
                         writeIORef caretLine lineNow
                         normalizeExcept (Just lineNow) ]
  -- Show the script when it's changed from elsewhere. wxWatchDynamic
  -- runs this up to 100ms after the change, so it also hears, late, about
  -- the changes this editor made itself, by which time there may be more
  -- typing in the editor; replacing the editor's text then would throw
  -- that typing away, and put the cursor at the start. So the script's
  -- current text (not the text this was called with, which may be older)
  -- is only shown if it's something this editor didn't write.
  wxWatchDynamic p (source scriptCode) $ \_ -> do
    CodeString newText <- getDynamic (source scriptCode)
    ours <- readIORef lastText
    oldText <- styledTextCtrlGetText ce
    when (newText /= ours && newText /= oldText) $ do
      writeIORef lastText newText
      styledTextCtrlSetText ce newText
      updateFoldLevels ce newText
  -- Which blocks fold, the font, and line numbers are settings: when
  -- they change, so does the editor.
  foldsCodeBlocks <- liftIO (newIORef . setFoldCodeBlocks =<< getDynamic appSettings)
  fontNow <- liftIO (newIORef . fontOf =<< getDynamic appSettings)
  wxWatchDynamic p appSettings $ \settings -> do
    was <- readIORef foldsCodeBlocks
    when (setFoldCodeBlocks settings /= was) $ do
      writeIORef foldsCodeBlocks (setFoldCodeBlocks settings)
      refreshFolds ce
    fontBefore <- readIORef fontNow
    when (fontOf settings /= fontBefore) $ do
      writeIORef fontNow (fontOf settings)
      isLight <- frameIsLight p
      uncurry (setupStyles ce isLight) (fontOf settings)
    -- (every time: the margin's width depends on the font, too)
    showLineNumbers ce (setLineNumbers settings)
  isError <- liftIO ((isLeft <$> getDynamic scriptCode) >>= \startError ->
                        variable [ value := startError ])
  wxWatchDynamic p scriptCode $ \case
    Left (loc, msg) -> do
      writeIORef checkedText Nothing
      set isError [ value := True ]
      set errorText [ text := "⚠️ " ++ msg, visible := True ]
      windowReLayout p
      code <- styledTextCtrlGetText ce
      m <- doSyntaxColoring
      case convertSourceSpan code <$> spanOfSourceRange loc of
        Nothing -> clearError ce
        Just (s, e) -> do
          -- (byte positions: the map is from characters to bytes, and
          -- the end of the text maps to nothing, so it's its length)
          docLen <- styledTextCtrlGetLength ce
          let byteAt i = Map.findWithDefault docLen i m
              from = byteAt s
          markError ce from (byteAt (e + 1) - from)
    Right _ -> do
      wasError <- get isError value
      void doSyntaxColoring
      clearError ce
      CodeString good <- getDynamic (source scriptCode)
      writeIORef checkedText (Just good)
      isFocused <- readIORef focused
      lineNow <- currentLine
      normalizeExcept (if isFocused then Just lineNow else Nothing)
      when wasError $ do
        set isError [ value := False ]
        set errorText [ text := "", visible := False ]
        windowReLayout p

  void $ liftIO doSyntaxColoring
  pure (ce, errorText)

-- | An Edit menu for a code editor: the usual commands, and toggling
-- comments and normalizing the script. (With emacs keys where Control
-- is the shortcut key, Select All and Paste have no shortcuts, so that
-- Control-A and Control-V stay emacs keys.)
scriptEditMenu :: StyledTextCtrl a -> IO (Menu ())
scriptEditMenu ce = do
  emacs <- setEmacsKeys <$> getDynamic appSettings
  let shortcut letter = if emacs && not emacsKeysByDefault then "" else "\tCtrl+" ++ letter
  m <- menuPane [ text := "&Edit" ]
  _ <- menuItem m [ text := "Undo\tCtrl+Z", on command := styledTextCtrlUndo ce ]
  _ <- menuItem m [ text := "Redo\tShift+Ctrl+Z", on command := styledTextCtrlRedo ce ]
  menuLine m
  _ <- menuItem m [ text := "Cut\tCtrl+X", on command := styledTextCtrlCut ce ]
  _ <- menuItem m [ text := "Copy\tCtrl+C", on command := styledTextCtrlCopy ce ]
  _ <- menuItem m [ text := "Paste" ++ shortcut "V", on command := pasteFittingIndentation ce ]
  _ <- menuItem m [ text := "Select All" ++ shortcut "A", on command := styledTextCtrlSelectAll ce ]
  menuLine m
  _ <- menuItem m [ text := "Toggle Comment\tCtrl+/", on command := toggleCommentLines ce ]
  _ <- menuItem m [ text := "Normalize Script", on command := normalizeEditor ce ]
  pure m

-- | Rewrite all of an editor's script with mathematical symbols
-- ("Language.Editor.Normalize"), whatever the setting for doing it as
-- it's edited, keeping the cursor where it was in the text.
normalizeEditor :: StyledTextCtrl a -> IO ()
normalizeEditor ce = do
  code <- styledTextCtrlGetText ce
  let edits = normalizations code
  unless (null edits) (applyEdits ce code edits)

-- | The font editors use, and its size.
fontOf :: Settings -> (String, Maybe Int)
fontOf settings = (editorFont settings, setEditorFontSize settings)

-- | Make rewrites of an editor's text (given as character ranges of it,
-- in order), as one undo step, keeping the cursor and selection where
-- they were in the text.
applyEdits :: StyledTextCtrl a -> String -> [(Int, Int, String)] -> IO ()
applyEdits ce code edits = do
  docLen <- styledTextCtrlGetLength ce
  let m = editorOffsetMap code
      byteAt i = Map.findWithDefault docLen i m
      bytes = BS.length . Utf8.fromString
      byteEdits = [ (byteAt from, byteAt to, new) | (from, to, new) <- edits ]
      -- where a position goes: moved by the edits before it, or, inside
      -- one, to the end of what replaces it
      shift q = q + sum [ bytes new - (to - from) | (from, to, new) <- byteEdits, to <= q ]
      moved q = case [ (from, new) | (from, to, new) <- byteEdits, from < q, q < to ] of
        (from, new) : _ -> shift from + bytes new
        [] -> shift q
  caret <- styledTextCtrlGetCurrentPos ce
  anchor <- styledTextCtrlGetAnchor ce
  styledTextCtrlBeginUndoAction ce
  forM_ (reverse byteEdits) $ \(from, to, new) -> do
    styledTextCtrlSetTargetStart ce from
    styledTextCtrlSetTargetEnd ce to
    void (styledTextCtrlReplaceTarget ce new)
  styledTextCtrlEndUndoAction ce
  styledTextCtrlSetSelection ce (moved anchor) (moved caret)

convertSourceSpan :: String -> SourceSpan -> (Int, Int)
convertSourceSpan input = \case
  InLine linum s e -> let lo = lineOffset input linum + s - 1
                          hi = e - s + lo
                      in (lo, hi)
  InRows s e -> let lo = lineOffset input
                in (lo s, lo (e + 1))

lineOffset :: String -> Int -> Int
lineOffset input = let m = Map.fromList
                         . ((0,0):)
                         . zip [1..]
                         . map fst
                         . filter ((== '\n') . snd)
                         . zip [1..] $ input
                       li = length input
                   in \lo -> 1 + Map.findWithDefault li lo m

-- | Like `watchDynamic`, but ensures that the action
-- runs on the main UI thread. Automatically attaches
-- the halt action to the destruction of `p`, and runs the
-- action once on construction.
--
-- Deliberately hooks `windowOnDestroy` (`wxEVT_DESTROY`) rather than
-- `on closing` (`wxEVT_CLOSE_WINDOW`, a vetoable close *request*):
-- `windowDestroy` -- used by a viewer rebuild's own teardown, see
-- [[project-rebuild-hang-bug]] -- never fires `closing` at all, so a
-- `closing`-only hook here would silently leak a timer and a
-- `Dynamic`'s listener closure on every rebuild, compounding with
-- every widget `wxWatchDynamic`/`wxWatchDynamic1` ever attached to a
-- rebuilt window. `wxEVT_DESTROY` fires on *every* real destruction
-- path (ordinary user-initiated close included, since the default
-- `closing` behavior itself just calls `windowDestroy`), so this is a
-- strict superset of what hooking `closing` ever covered.
wxWatchDynamic :: (AsDynamic f, MonadIO io) => Window b -> f a -> (a -> IO ()) -> io ()
wxWatchDynamic p dv action = liftIO $ do
  todo <- newMVar []
  halt <- watchDynamic dv $ \x -> modifyMVar_ todo (\actions -> pure (action x : actions))
  _ <- wxTimer p [ interval := 100
                 , enabled := True
                 , on command := tryTakeMVar todo >>= \case
                     Nothing -> pure ()
                     Just actions -> do
                       putMVar todo []
                       sequence_ (reverse actions)
                 ]
  onOwnDestroy p halt

-- | Same as `wxWatchDynamic`, but also
wxWatchDynamic1 :: (AsDynamic f, MonadIO io) => Window b -> f a -> (a -> IO ()) -> io ()
wxWatchDynamic1 p dv action = do
  wxWatchDynamic p dv action
  -- Run the action once
  liftIO (getDynamic dv >>= action)

-- | See 'wxWatchDynamic's doc comment for why this hooks
-- `windowOnDestroy` rather than `on closing`.
wxTimer :: MonadIO io => Window a -> [Prop Timer] -> io Timer
wxTimer w props = liftIO $ do
  t <- timer w props
  onOwnDestroy w (timerStop t)
  pure t

-- | Run an action when a window is destroyed, along with the destroy
-- handlers installed before it.
--
-- Unlike a plain `windowOnDestroy`, this only acts on the window's /own/
-- destruction: `wxWindowDestroyEvent` is a command event, so it propagates
-- up from child windows, and a plain handler runs whenever any child is
-- destroyed -- which, for 'wxWatchDynamic' and 'wxTimer', meant a window
-- that rebuilds some of its widgets (the autocolor window's list) silently
-- stopped every watch and timer on it.
onOwnDestroy :: Window a -> IO () -> IO ()
onOwnDestroy w action = do
  previous <- windowGetOnDestroy w
  let both = action >> previous
  windowOnEvent w [wxEVT_DESTROY] both $ \ev -> do
    src <- eventGetEventObject ev
    when (src == (objectCast w :: WxObject ())) both
    -- (let any ancestors see a child's destruction too, as they would
    -- without this handler)
    eventSkip ev
