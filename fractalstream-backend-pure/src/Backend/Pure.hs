--{-# options_ghc -Wno-unused-matches #-}
module Backend.Pure
  ( interpretViewer
  ) where

import FractalStream.Prelude
import Language.Environment
import Language.Draw
import Language.Code
import Language.Code.InterpretIO
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform (backendRewrites)
import Actor.Viewer
import Data.Indexed.Functor
import Data.Color (colorToRGB, grey)

import Foreign hiding (void)
import Data.IORef

interpretViewer :: forall env t. MissingViewerArgs env
                => Code (ViewerEnv env)
                -> (ViewerFunction env -> IO t) -> IO t
interpretViewer body0 action = do
  -- The same rewrites the LLVM backend applies, so both render alike.
  let body = transformValues backendRewrites body0
      env = toIndex body
  withEnvironment env $ action $ ViewerFunction $ \vaArgs -> pure $ \ViewerArgs{..} -> do
    let (x0, y0) = vaPoint
        (dx, dy) = vaStep
        -- (the pixel size the code sees, which needn't be the step)
        context :: Context HaskellValue (ViewerEnv env)
        context = Bind (Proxy @InternalX)  RealType  x0
                $ Bind (Proxy @InternalY)  RealType  y0
                $ Bind (Proxy @InternalDX) RealType  (fst vaPixel)
                $ Bind (Proxy @InternalDY) RealType  (snd vaPixel)
                $ Bind (Proxy @"color")    ColorType grey
                $ vaArgs

    -- (each pixel's point from the image's origin and its index in the
    -- image: see 'vaOffset')
    let (ox, oy) = vaOffset
    forM_ [0 .. vaHeight - 1] $ \j -> do
      let y = y0 - fromIntegral (oy + j) * dy
      forM_ [0 .. vaWidth - 1] $ \i -> do
        let x = x0 + fromIntegral (ox + i) * dx

        iorefs :: Context IORefTypeOfBinding (ViewerEnv env) <-
          mapContextM (\_ _ -> newIORef) context

        (r, g, b) <- fmap colorToRGB . flip evalStateT iorefs $ do
          update bindingEvidence (Proxy @InternalX) RealType x
          update bindingEvidence (Proxy @InternalY) RealType y
          interpretToIO noDrawing body
          eval (Var (Proxy @"color") ColorType bindingEvidence)

        let offset = fromIntegral (3 * (j * vaWidth + i))
        pokeByteOff vaBuffer (offset + 0) r
        pokeByteOff vaBuffer (offset + 1) g
        pokeByteOff vaBuffer (offset + 2) b

    -- The interpreter has no arena to run out of, and a bad list index
    -- is an exception instead.
    pure mempty

noDrawing :: DrawHandler ScalarIORefM
noDrawing = DrawHandler (\_ -> pure ())
