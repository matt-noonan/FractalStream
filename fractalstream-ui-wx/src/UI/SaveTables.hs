{- |
Module      : UI.SaveTables
Description : Asking whether to save a project's tables along with it.

A project's tables can be big, so saving what they hold is up to the
user: a small window says how big each table is, and asks whether to save
with them or without them -- before the save dialog, when there is one, so
that choosing the file is the last step. (The native save dialog can't
have a choice of its own added.)
-}
module UI.SaveTables
  ( askSaveTables
  , describeBytes
  ) where

import FractalStream.Prelude hiding (get)

import Actor.SavedTables (TableSummary(..))

import Graphics.UI.WX hiding (when, Color, Layout)
import Graphics.UI.WXCore.WxcClassesMZ (windowDestroy)

import Text.Printf (printf)

-- | Ask whether to save the given tables (which aren't empty): 'Just'
-- the choice, or 'Nothing' if the save was cancelled. The given choice is
-- the default button's.
--
-- (The window has no parent: saving starts from the project's own frame,
-- which is never shown, and a dialog over it brings it on screen, empty,
-- when the dialog closes.)
askSaveTables :: [TableSummary] -> Bool -> IO (Maybe Bool)
askSaveTables tables initially = do
  d <- dialog objectNull [ text := "Save table data?" ]
  p <- panel d []
  let total = sum (map tsBytes tables)
      describe TableSummary{..} = printf "%s:  %s,  %s" tsName
        (intercalate " × " (map show tsSize)) (describeBytes tsBytes)
  intro <- staticText p [ text := "This project's tables hold data:" ]
  details <- staticText p [ text := intercalate "\n" (map describe tables) ]
  note <- staticText p [ text := "About " ++ describeBytes total
                                 ++ " in all, before compression. Without them,\n\
                                    \the tables are empty when the project is opened again." ]
  cancel <- button p [ text := "Cancel" ]
  without <- button p [ text := "Save without tables" ]
  with <- button p [ text := "Save with tables" ]
  set d [ layout := container p $ margin 12 $ column 8
            [ widget intro
            , marginWidth 20 (marginLeft (widget details))
            , widget note
            , row 8 [ widget cancel, hglue, widget without, widget with ] ]
        , defaultButton := (if initially then with else without) ]
  result <- showModal d $ \done -> do
    set cancel [ on command := done Nothing ]
    set without [ on command := done (Just False) ]
    set with [ on command := done (Just True) ]
  void (windowDestroy d)
  pure result

-- | A number of bytes, for people: @"812 bytes"@, @"14 KB"@, @"2.1 MB"@.
describeBytes :: Int -> String
describeBytes n
  | n < 1000 = show n ++ " bytes"
  | n < 1000 * 1000 = scaled 1000 "KB"
  | n < 1000 * 1000 * 1000 = scaled (1000 * 1000) "MB"
  | otherwise = scaled (1000 * 1000 * 1000) "GB"
  where
    scaled :: Double -> String -> String
    scaled unit name =
      let x = fromIntegral n / unit
      in (if x < 10 then printf "%.1f %s" x name else printf "%.0f %s" x name)
