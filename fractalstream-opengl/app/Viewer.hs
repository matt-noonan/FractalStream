{-# LANGUAGE OverloadedRecordDot, RecordWildCards, FlexibleContexts #-}
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
      -- Load config file
      c <- loadConfig configPath

      -- Create header for global uniform variables
      let vars = map coord c.viewers
          header = concat $ map (printf "uniform vec2 _%s;\n") vars

      -- Open all viewers 
      viewerInfos <- mapM (openViewer vars header) c.viewers

      -- Add mouse events last, because the might affect all viewers
      mapM_ (\v -> windowOnMouse v.canvas True $ onMouse viewerInfos v) viewerInfos

  where
    onMouse viewerInfos v event
      = do
          _ <- glCanvasSetCurrent v.canvas v.ctx

          -- First find the mouse position in the view coordinates
          let p@(WXC.Point i j) = mousePos event
          
          Vector2 cx cy <- varGet v.viewCenter
          Vector2 dx dy <- varGet v.viewDiameter

          WXC.Size w h <- windowGetClientSize v.canvas

          let x = cx + (fromIntegral i / fromIntegral w - 0.5) * dx
              y = cy - (fromIntegral j / fromIntegral h - 0.5) * dy
              m = Vector2 x y
        
          case event of
              -- Update mouse position
              MouseMotion _ _ -> do
                setUniform v.program "_mouse" m
                varSet v.mousePosition m

              -- Stop dragging and pick a point (if pointer didn't move much)
              MouseLeftUp _ _ -> do
                p0 <- varGet v.dragStartPoint

                let pickPoint viewerInfo = do
                      _   <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
                      setUniform viewerInfo.program v.var m
                      windowRefresh viewerInfo.canvas False
                
                case p0 of
                  Nothing -> return ()
                  Just (WXC.Point i' j') -> if abs(i - i') < 3 && abs(j - j') < 3
                                              then mapM_ pickPoint viewerInfos
                                              else return ()
                
                varSet v.dragStart Nothing
                varSet v.dragStartPoint Nothing

              -- Start dragging
              MouseLeftDown _ _ -> do
                varSet v.dragStart $ Just m
                varSet v.dragStartPoint $ Just p

              -- Pan the view
              MouseLeftDrag _ _ -> do
                ds <- varGet v.dragStart

                case ds of
                  Nothing -> do return ()
                  Just (Vector2 dsx dsy) -> do
                    let t = Vector2 (dsx - x + cx) (dsy - y + cy)

                    setUniform v.program "_center" t
                    varSet v.viewCenter t

                    windowRefresh v.canvas False

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
                
                setUniform v.program "_center" c
                setUniform v.program "_diameter" d

                varSet v.viewCenter c
                varSet v.viewDiameter d

                windowRefresh v.canvas False
              
              -- Pass the event forward
              _ -> do propagateEvent

openViewer :: [String] -> String -> Viewer -> IO ViewerInfo
openViewer vars header viewer
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

      -- Compile shader programs
      program <- getProgram $ addHeader header viewer.coord viewer.code
      currentProgram $= Just program

      -- Set uniforms
      let c = Vector2 viewer.center_x viewer.center_y :: GLComplex
          d = Vector2 viewer.radius viewer.radius :: GLComplex
          m = Vector2 0.0 0.0 :: GLComplex

      setUniform program "_center" c
      setUniform program "_diameter" d
      setUniform program "_mouse" m

      -- Set variable uniforms (for point picking)
      let var = "_" ++ viewer.coord
      mapM_ (\v -> setUniform program ("_" ++ v) m) vars

      -- Set variables to keep track of the last uniform values
      viewCenter <- varCreate c
      viewDiameter <- varCreate d
      mousePosition <- varCreate m
      dragStart <- varCreate Nothing
      dragStartPoint <- varCreate Nothing

      -- Store all data in a record
      let viewerInfo = ViewerInfo{..}
    
      -- Set event handlers
      windowOnPaint canvas $ onPaint viewerInfo triangles numVertices
      windowOnSize canvas $ onSize viewerInfo

      -- Show the frame
      _ <- windowShow f
      windowRaise f
      return viewerInfo

  where
    -- Run vertex and fragment shaders
    onPaint viewerInfo triangles numVertices _dc _rect
      = do 
          _ <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
          clear [ ColorBuffer ]  

          bindVertexArrayObject $= Just triangles
          drawArrays Triangles 0 (fromIntegral numVertices)
          
          flush
          _ <- glCanvasSwapBuffers viewerInfo.canvas
          
          return ()

    -- Change uniforms that track canvas dimensions
    onSize viewerInfo
      = do 
          _ <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
          WXC.Size w h <- windowGetClientSize viewerInfo.canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          (Vector2 width height) <- varGet viewerInfo.viewDiameter
          
          let r = if w <= h
                    then Vector2 width $ fromIntegral h / fromIntegral w * width
                    else Vector2 (fromIntegral w / fromIntegral h * height) height
          
          setUniform viewerInfo.program "_diameter" r

          varSet viewerInfo.viewDiameter r

type GLComplex = GL.Vector2 GLfloat

data ViewerInfo = ViewerInfo 
  { program         :: Program
  , canvas          :: GLCanvas ()
  , ctx             :: GLContext ()
  , viewCenter      :: Var GLComplex
  , viewDiameter    :: Var GLComplex
  , mousePosition   :: Var GLComplex
  , dragStart       :: Var (Maybe GLComplex)
  , dragStartPoint :: Var (Maybe (WXC.Point2 Int))
  , var             :: String
  }

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

addHeader :: String -> String -> String -> String
addHeader header varName sourceCode = printf
  "#version 410 core \n\
  \\n\
  \uniform vec2 _mouse;\n\
  \uniform vec2 _diameter;\n\
  \uniform vec2 _center;\n\
  \uniform sampler1D uTexture;\n\
  \\n\
  \%s\
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
  \}" header varName sourceCode