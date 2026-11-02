-- | Quoting a name (a project's title, a file's path, ...) inside a
-- message for the user, so that it's clear which part of the message is
-- the name and which is the message: @Save the changes to “A filled
-- Julia set” before closing it?@
module FractalStream.Quoted
  ( quoted
  ) where

quoted :: String -> String
quoted name = "“" ++ name ++ "”"
