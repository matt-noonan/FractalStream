module Main ( main ) where

import Graphics.UI.WX

import Viewer

main :: IO ()
main = start welcome

welcome :: IO ()
welcome = do
  -- Create welcome frame
  f <- frame [ text := "FractalStream" ]
  p <- panel f []

  newScript <- button p [ text := "New project"
                        ]
  openScript <- button p [ text := "Open project..."
                         , on command := onOpen f
                         ]

  -- Add menubar
  prj <- menuPane [ text := "&Project"]

  _   <- menuItem prj [ text := "New project"
                      , help := "Open an empty editor"
                      ]
  _   <- menuItem prj [ text := "Open project..."
                      , help := "Open YAML config file"
                      , on command := onOpen f
                      ]
  
  menuLine prj
  
  _   <- menuQuit prj []

  hlp   <- menuHelp      [ text := "&Help" ]
  about <- menuAbout hlp [ text := "About FractalStream" ]

  -- Combine into the top frame
  set f [ layout  := margin 30 $ fill $ container p $ floatCenter $ column 10
            [ expand $ widget newScript
            , expand $ widget openScript
            ]
        , menuBar := [prj, hlp]
        , on (menu about) := onAbout f
        ]

  
  where
    onAbout f = do
      _ <- infoDialog f title msg
      return ()
      
      where
        title = "About 'FractalStream'"
        msg   = "This is an OpenGL backend test"

    onOpen f = do
      mbfname <- fileOpenDialog f False True title fTypes "" ""
      
      case mbfname of
        Nothing    -> return ()
        Just fname -> do openViewers fname
      
      where
        title = "Open Config File"
        fTypes = [ ("YAML config file", ["*.yaml"])
                 , ("All files", ["*.*"])
                 ]