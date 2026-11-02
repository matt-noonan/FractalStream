-- | Making console output safe for any message. Messages can hold
-- characters the console can't show: project titles can be anything, and
-- names are quoted with typographic quotes ("FractalStream.Quoted"). On
-- Windows, the console's encoding is a code page (like CP1252) that has
-- no “ or ”, and GHC throws rather than write a character its handle's
-- encoding can't encode, so a warning could crash the program printing
-- it. 'transliterateConsole' makes stdout and stderr write a substitute
-- for such a character instead.
module FractalStream.Console
  ( transliterateConsole
  ) where

import Control.Monad (forM_)
import System.IO

-- | Keep stdout's and stderr's encodings, but have each write a
-- substitute for a character it can't encode, rather than throw.
transliterateConsole :: IO ()
transliterateConsole = forM_ [stdout, stderr] $ \h ->
  hGetEncoding h >>= mapM_ (\enc ->
    hSetEncoding h =<< mkTextEncoding (takeWhile (/= '/') (show enc) ++ "//TRANSLIT"))
