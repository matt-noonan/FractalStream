{-# options_ghc -Wno-unused-matches #-}
module Backend.Pure
  ( interpretComplexViewer
  ) where

import FractalStream.Prelude
import Language.Environment
import Language.Draw
import Language.Code
import Language.Code.InterpretIO
import Language.Value.Evaluator (HaskellValue)
import Data.Color (colorToRGB)

import Foreign hiding (void)
import Data.IORef

interpretComplexViewer
    :: forall x y dx dy out env t
     . ( KnownEnvironment env
       , NotPresent "[internal argument] #blockWidth" env
       , NotPresent "[internal argument] #blockHeight" env
       , NotPresent "[internal argument] #subsamples" env
       , KnownSymbol x, KnownSymbol y
       , KnownSymbol dx, KnownSymbol dy
       , KnownSymbol out
       , Required x env ~ 'RealT
       , NotPresent x (env `Without` x)
       , Required y env ~ 'RealT
       , NotPresent y (env `Without` y)
       , Required dx env ~ 'RealT
       , NotPresent dx (env `Without` dx)
       , Required dy env ~ 'RealT
       , NotPresent dy (env `Without` dy)
       , Required out env ~ 'ColorT
       , NotPresent out (env `Without` out)
       )
    => Proxy x
    -> Proxy y
    -> Proxy dx
    -> Proxy dy
    -> Proxy out
    -> Code env
    -> ((Int32 -> Int32 -> Int32 -> Context HaskellValue env -> Ptr Word8 -> IO ())
         -> IO t)
    -> IO t
interpretComplexViewer px py pdx pdy out body action = do

  action $ \blockWidth blockHeight subsamples ctx buf -> do

    let x0 = getBinding @x  @'RealT ctx bindingEvidence
        y0 = getBinding @y  @'RealT ctx bindingEvidence
        dx = getBinding @dx @'RealT ctx bindingEvidence
        dy = getBinding @dy @'RealT ctx bindingEvidence

    forM_ (zip [0 .. blockWidth - 1] [y0, y0 - dy ..]) $ \(j, y) -> do
      forM_ (zip [0 .. blockHeight - 1] [x0, x0 + dx ..]) $ \(i, x) -> do

        iorefs :: Context IORefTypeOfBinding env <-
          mapContextM (\_ _ -> newIORef) ctx

        (r, g, b) <- fmap colorToRGB . flip evalStateT iorefs $ do
          update bindingEvidence px RealType x
          update bindingEvidence py RealType y
          interpretToIO noDrawing body
          eval (Var out ColorType bindingEvidence)

        let offset = fromIntegral (3 * (j * blockWidth + i))
        pokeByteOff buf (offset + 0) r
        pokeByteOff buf (offset + 1) g
        pokeByteOff buf (offset + 2) b


    pure ()

noDrawing :: DrawHandler ScalarIORefM
noDrawing = DrawHandler (\_ -> pure ())
