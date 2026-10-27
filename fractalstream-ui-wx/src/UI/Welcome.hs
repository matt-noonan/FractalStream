module UI.Welcome
  ( welcome
  ) where

import Graphics.UI.WX

import Actor.Ensemble (allTemplates)

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

  template <- chooseTemplate ProjectActions{..} p

  open <- button p [ text := "Open existing project..."
                   , on command := getProject "Open" projectOpen
                   ]
  set f [ layout := margin 10 $ fill $ container p $ floatCenter $ column 10
                    [ widget new
                    , widget template
                    , widget open
                    ]
        ]
  windowReLayout f

chooseTemplate :: ProjectActions -> Window a -> IO (Choice ())
chooseTemplate pa f = do
  c <- choice f [ items := "New project from template..." : map fst allTemplates ]
  set c [ on select := get c selection >>= \case
            0 -> pure ()
            ix -> do
              set c [ selection := 0 ]
              let (name, make) = allTemplates !! (ix - 1)
              make >>= projectOpenTemplate pa name
        ]
  pure c
