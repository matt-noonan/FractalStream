module UI.Welcome
  ( welcome
  ) where

import Graphics.UI.WX


import UI.ProjectActions
import UI.Menu

welcome :: ProjectActions -> IO ()
welcome ProjectActions{..} = do

  f <- frame [ text := "Welcome to FractalStream!"
             , on resize := propagateEvent ]

  makeMenuBar ProjectActions{..} f []

  p <- panel f []

  let getProject verb action = do
        mprj <- fileOpenDialog objectNull True True
                (verb ++ " a FractalStream project")
                [ ("FractalStream project files", ["*.yaml"])
                , ("All files", ["*.*"])
                ] "" ""
        maybe (pure ()) action mprj

  new <- button p [ text := "New project"
                  , on command := projectNew ]

  open <- button p [ text := "Open existing project..."
                   , on command := getProject "Open" projectOpen
                   ]
  set f [ layout := margin 10 $ fill $ container p $ floatCenter $ column 10
                    [ widget new
                    , widget open
                    ]
        ]
  windowReLayout f
