module Main ( main ) where

import Graphics.UI.WX

import LoadShaders ( ShaderSource(..) )
import Viewer

main :: IO ()
main = start welcome

welcome :: IO ()
welcome = do
  -- Create welcome frame
  f <- frame [ text := "Welcome to FractalStream" ]
  p <- panel f []

  open <- button p [ text := "Open GLSL fragment shader..."
                   , on command := onOpen f
                   ]

  -- Add menubar
  prj <- menuPane [ text := "&Project"]
  _   <- menuItem prj [ text := "Open shader..."
                      , help := "Open a GLSL fragment shader"
                      , on command := onOpen f
                      ]
  
  menuLine prj
  
  _   <- menuQuit prj []

  hlp   <- menuHelp      [ text := "&Help" ]
  about <- menuAbout hlp [ text := "About FractalStream" ]

  -- Combine into the top frame
  set f [ layout  := margin 30 $ fill $ container p $ floatCenter $ column 10
                        [ widget open ]
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
        Just fname -> do viewer $ FileSource fname
      
      where
        title = "Open GLSL fragment shader"
        fTypes = [ ("GLSL fragment shader",["*.glsl","*.frag"])
                 , ("All files", ["*.*"])
                 ]