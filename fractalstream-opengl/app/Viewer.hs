{-# LANGUAGE OverloadedRecordDot #-}
module Viewer ( openViewers ) where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL

import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.Rendering.OpenGL as GL

import Config
import Palette
import Shaders

openViewers :: FilePath -> IO ()
openViewers configPath 
  = do
      -- Load config file and stores OpenGL programs
      p <- loadConfig configPath

      -- Open all viewers 
      mapM_ openViewer p.viewers

openViewer :: Viewer -> IO ()
openViewer viewer
  = do
      -- Create top frame
      f <- frameCreateTopFrame viewer.title

      -- Create GLCanvas and GLContext
      let initRect = (Rect 0 0 viewer.width_pixels viewer.height_pixels)
          options  = [GL_RGBA, GL_MAJOR_VERSION 4, GL_DOUBLEBUFFER]
      
      canvas <- glCanvasCreateEx f 0 initRect 0 "GLCanvas" options nullPalette

      ctx <- glContextCreateFromNull canvas    
      _   <- glCanvasSetCurrent canvas ctx

      let w  = fill $ widget canvas
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

      let vPosition = AttribLocation 0
          vDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr
      
      vertexAttribPointer vPosition $= (ToFloat, vDescriptor)
      vertexAttribArray vPosition $= Enabled

      -- Create color palette
      initTexture

      -- Load program from Viewer record
      program <- getProgram $ addHeader viewer.coord viewer.code
      currentProgram $= Just program

      -- Set uniforms
      let c = Vector2 viewer.center_x viewer.center_y :: GLComplex
          d = Vector2 viewer.radius viewer.radius :: GLComplex
          m = Vector2 0.0 0.0 :: GLComplex

      setUniform program "_center" c
      setUniform program "_diameter" d
      setUniform program "_mouse" m

      -- Set variables to keep track of the last uniform values
      viewCenter <- varCreate c
      viewDiameter <- varCreate d
      mousePosition <- varCreate m
      dragStart <- varCreate Nothing
    
      -- Set event handlers
      windowOnPaint canvas $ onPaint canvas ctx triangles numVertices
      windowOnSize canvas $ onSize canvas ctx program viewDiameter
      windowOnMouse canvas True $ 
        onMouse canvas ctx program mousePosition dragStart viewCenter viewDiameter

      -- Show the frame
      _ <- windowShow f
      windowRaise f
      return ()

  where
    -- Run vertex and fragment shaders
    onPaint canvas ctx triangles numVertices _ _
      = do 
          _ <- glCanvasSetCurrent canvas ctx
          clear [ ColorBuffer ]  

          bindVertexArrayObject $= Just triangles
          drawArrays Triangles 0 (fromIntegral numVertices)
          
          flush
          _ <- glCanvasSwapBuffers canvas
          
          return ()

    -- Change uniforms that track canvas dimensions
    onSize canvas ctx p viewDiameter
      = do 
          _ <- glCanvasSetCurrent canvas ctx
          WXC.Size w h <- windowGetClientSize canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          (Vector2 width height) <- varGet viewDiameter
          
          let r = if w <= h
                    then Vector2 width $ fromIntegral h / fromIntegral w * width
                    else Vector2 (fromIntegral w / fromIntegral h * height) height
          
          setUniform p "_diameter" r

          varSet viewDiameter r

    onMouse canvas ctx p mousePosition dragStart viewCenter viewDiameter event
      = do
          _ <- glCanvasSetCurrent canvas ctx

          -- First find the mouse position in the view coordinates
          let WXC.Point i j = mousePos event
          
          Vector2 cx cy <- varGet viewCenter
          Vector2 dx dy <- varGet viewDiameter

          WXC.Size w h <- windowGetClientSize canvas

          let x = cx + (fromIntegral i / fromIntegral w - 0.5) * dx
              y = cy - (fromIntegral j / fromIntegral h - 0.5) * dy
              m = Vector2 x y
        
          case event of
              -- Update mouse position
              MouseMotion _ _ -> do
                setUniform p "_mouse" m
                varSet mousePosition m

              -- Stopped dragging
              MouseLeftUp _ _ -> do
                varSet dragStart Nothing

              -- Started dragging
              MouseLeftDown _ _ -> do
                varSet dragStart $ Just m

              -- Picked a point (change globals in all programs)
              -- MouseLeftDClick _ _ -> do
              --   forM_ vs $ \a -> setUniform p ("_" ++ a.var) m

              -- Pan the view
              MouseLeftDrag _ _ -> do
                ds <- varGet dragStart

                case ds of
                  Nothing -> do return ()
                  Just (Vector2 dsx dsy) -> do
                    let t = Vector2 (dsx - x + cx) (dsy - y + cy)

                    setUniform p "_center" t
                    varSet viewCenter t

                    windowRefresh canvas False

              -- Zoom in/out
              MouseWheel downward _ _ -> do
                let speed = 1.2
                    scalingFactor = if downward then speed else 1/speed

                    cxNew = x + scalingFactor * (cx - x)
                    cyNew = y + scalingFactor * (cy - y)
                    c = Vector2 cxNew cyNew

                    dxNew = dx * scalingFactor
                    dyNew = dy * scalingFactor
                    d = Vector2 dxNew dyNew
                
                setUniform p "_center" c
                setUniform p "_diameter" d

                varSet viewCenter c
                varSet viewDiameter d

                windowRefresh canvas False
              
              -- Pass the event forward
              _ -> do propagateEvent

type GLComplex = GL.Vector2 GLfloat

setUniform :: Uniform a => Program -> String -> a -> IO ()
setUniform p var_ val = do
  -- TODO: figure out error checking here
  location <- get (uniformLocation p var_)
  uniform location $= val

getProgram :: String -> IO Program
getProgram fragSource = do
  loadShaders [ ShaderInfo VertexShader $ StringSource vertexCode
              , ShaderInfo FragmentShader $ StringSource fragSource
              ]

vertexCode :: String
vertexCode =
  "#version 410 core\n\
  \\n\
  \layout (location = 0) in vec2 pos;\n\
  \\n\
  \out vec4 FragPos;\n\
  \\n\
  \void main() {\n\
  \  FragPos = vec4(pos, 0.0, 1.0);\n\
  \  gl_Position = FragPos;\n\
  \}"

addHeader :: String -> String -> String
addHeader varName sourceCode = printf
  "#version 410 core \n\
  \\n\
  \uniform vec2 _mouse;\n\
  \uniform vec2 _diameter;\n\
  \uniform vec2 _center;\n\
  \uniform sampler1D uTexture;\n\
  \\n\
  \in vec4 FragPos;\n\
  \\n\
  \out vec4 color;\n\
  \\n\
  \// Complex Multiplication\n\
  \vec2 _cMul(vec2 a, vec2 b) {\n\
    \return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);\n\
  \}\n\
  \\n\
  \// Complex Division\n\
  \vec2 _cDiv(vec2 a, vec2 b) {\n\
  \  return vec2(a.x * b.x + a.y * b.y, a.y * b.x - a.x * b.y) / (b.x * b.x + b.y * b.y);\n\
  \}\n\
  \\n\
  \void main() {\n\
  \  vec2 %s = _center + FragPos.xy * _diameter / 2.0;\n\
  \  %s\n\
  \}" varName sourceCode