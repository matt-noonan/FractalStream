module Main ( main ) where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL

import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.Rendering.OpenGL as GL

import LoadShaders

main :: IO ()
main
  = run gui

type GLComplex = GL.Vector2 GLfloat

gui :: IO ()
gui
  = do -- create top frame
      f <- frameCreateTopFrame "FractalStream"

      -- for good measure: put a scrolled window inside the frame
      canvas <- glCanvasCreateEx f 
                                 0 
                                 (Rect 0 0 700 700)
                                 0
                                 "GLCanvas"
                                 [GL_RGBA, GL_MAJOR_VERSION 4, GL_DOUBLEBUFFER]
                                 nullPalette

      ctx <- glContextCreateFromNull canvas    
      _   <- glCanvasSetCurrent canvas ctx

      let gl = GLWindow canvas ctx
          w  = fill $ widget canvas
      
      windowSetLayout f w

      -- Set background and shading
      clearColor $= Color4 0 0 0 0
      shadeModel $= Flat
 
      -- Create mesh buffer
      triangles <- genObjectName
      bindVertexArrayObject $= Just triangles

      -- Create vertex buffer
      let vertices = [ Vertex2 (-1.0) (-1.0)  -- Triangle 1
                     , Vertex2   1.0  (-1.0)
                     , Vertex2 (-1.0)   1.0
                     , Vertex2   1.0  (-1.0)  -- Triangle 2
                     , Vertex2   1.0    1.0 
                     , Vertex2 (-1.0)   1.0 
                     ] :: [Vertex2 GLfloat]
          numVertices = length vertices
          vertexSize = sizeOf (Vertex2 0.0 (0.0 :: GLfloat))

      arrayBuffer <- genObjectName
      bindBuffer ArrayBuffer $= Just arrayBuffer
      withArray vertices $ \ptr -> do
        let size = fromIntegral (numVertices * vertexSize)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

      let firstIndex = 0
          vPosition = AttribLocation 0
          mesh = Mesh triangles (fromIntegral firstIndex) (fromIntegral numVertices)
      
      vertexAttribPointer vPosition $= ( ToFloat
                                       , VertexArrayDescriptor 2 
                                                               Float 
                                                               0 
                                                               $ bufferOffset $ firstIndex * vertexSize
                                       )
      vertexAttribArray vPosition $= Enabled

      -- Load, bind, link, and compile shaders
      program <- loadShaders [ ShaderInfo VertexShader (FileSource "app/vert.glsl")
                             , ShaderInfo FragmentShader (FileSource "app/frag.glsl")
                             ]
      currentProgram $= Just program

      let c = Vector2 (-0.5) 0.0 :: GLComplex
          d = Vector2   6.0  6.0 :: GLComplex
          m = Vector2   0.0  0.0 :: GLComplex

      -- Set uniforms
      setUniform program "uCenter" c
      setUniform program "uDiameter" d
      setUniform program "uMouse" m

      -- Set variables to keep track of the last uniform values
      viewCenter <- varCreate c
      viewDiameter <- varCreate d
      mousePosition <- varCreate m
      dragStart <- varCreate Nothing
    
      -- to show the effect of double-buffering, we track the mouse with a small disc.
      windowOnMouse canvas True $ onMouse program canvas mousePosition dragStart viewCenter viewDiameter

      -- set paint event handler:
      windowOnPaint canvas $ onPaint mesh gl

      -- set resize event handler:
      windowOnSize canvas $ onSize program canvas ctx viewDiameter

      -- show the frame
      _ <- windowShow f
      windowRaise f
      return ()

  where
    -- update the mouse position and force a repaint
    onMouse program canvas mousePosition dragStart viewCenter viewDiameter event
      = do
          let WXC.Point i j = mousePos event
          
          Vector2 cx cy <- varGet viewCenter
          Vector2 dx dy <- varGet viewDiameter

          WXC.Size w h <- windowGetClientSize canvas

          let x = cx + (fromIntegral i / fromIntegral w - 0.5) * dx
              y = cy - (fromIntegral j / fromIntegral h - 0.5) * dy
              m = Vector2 x y
        
          case event of
              MouseMotion _ _ -> do
                setUniform program "uMouse" m
                varSet mousePosition m

              MouseLeftUp _ _ -> do
                varSet dragStart Nothing

              MouseLeftDown _ _ -> do
                varSet dragStart $ Just m

              MouseLeftDrag _ _ -> do
                ds <- varGet dragStart

                case ds of
                  Nothing -> do return ()
                  Just (Vector2 dsx dsy) -> do
                    let t = Vector2 (dsx - x + cx) (dsy - y + cy)

                    setUniform program "uCenter" t
                    varSet viewCenter t

                    windowRefresh canvas False

              MouseWheel downward _ _ -> do
                let speed = 1.2
                    scalingFactor = if downward then speed else 1/speed

                    cxNew = x + scalingFactor * (cx - x)
                    cyNew = y + scalingFactor * (cy - y)
                    c = Vector2 cxNew cyNew

                    dxNew = dx * scalingFactor
                    dyNew = dy * scalingFactor
                    d = Vector2 dxNew dyNew
                
                setUniform program "uCenter" c
                setUniform program "uDiameter" d

                varSet viewCenter c
                varSet viewDiameter d

                windowRefresh canvas False
              
              _ -> do propagateEvent

    -- do some painting.
    onPaint (Mesh triangles firstIndex numVertices) (GLWindow canvas ctx) _dc _view
      = do 
          _ <- glCanvasSetCurrent canvas ctx
          clear [ ColorBuffer ]  

          bindVertexArrayObject $= Just triangles
          drawArrays Triangles firstIndex numVertices
          
          flush
          _ <- glCanvasSwapBuffers canvas
          
          return ()

    onSize program canvas ctx viewDiameter
      = do 
          _ <- glCanvasSetCurrent canvas ctx
          WXC.Size w h <- windowGetClientSize canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          (Vector2 width height) <- varGet viewDiameter
          
          let r = if w <= h
                    then Vector2 width $ fromIntegral h / fromIntegral w * width
                    else Vector2 (fromIntegral w / fromIntegral h * height) height
          
          setUniform program "uDiameter" r

          varSet viewDiameter r

data GLWindow a b = GLWindow (GLCanvas a) (GLContext b)
data Mesh = Mesh VertexArrayObject ArrayIndex NumArrayIndices

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

setUniform :: Uniform a => Program -> String -> a -> IO ()
setUniform program var val = do
  -- TODO: figure out error checking here
  location <- get (uniformLocation program var)
  uniform location $= val
