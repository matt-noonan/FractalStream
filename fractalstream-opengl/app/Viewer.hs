{-# LANGUAGE OverloadedRecordDot, RecordWildCards, FlexibleContexts, LambdaCase #-}
module Viewer ( openViewers ) where

import Control.Lens
import Linear.V2
import Linear.Matrix
import Linear.OpenGL
import Text.Printf

import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL

import Linear.Projection as L
import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.Rendering.OpenGL as GL

import Config
import Meshes
import Palette
import Shaders

openViewers :: FilePath -> IO ()
openViewers configPath
  = do
      -- Load config file
      c <- loadConfig configPath

      -- Create header for global uniform variables
      let vars = map coord c.viewers
          -- types = [if projective v then "vec4" else "vec2" | v <- c.viewers]
          -- header = concat $ zipWith (printf "uniform %s _%s;\n") types vars
          header = concatMap (printf "uniform vec2 _%s;\n") vars

      -- Open all viewers
      viewerInfos <- mapM (openViewer vars header) c.viewers

      -- Add mouse events last, because the might affect all viewers
      mapM_ (\v -> windowOnMouse v.canvas True $ onMouse viewerInfos v) viewerInfos

      -- Close all viewers of the project at the same time
      -- This is to avoid setting variables out of scope.
      -- TODO: Add error checking to avoid these errors in general.
      mapM_ (\v -> windowOnClose v.vFrame $ onClose viewerInfos) viewerInfos

  where
    onClose = mapM_ (\v -> windowDestroy v.vFrame)

    onMouse viewerInfos v event = case v.projective of
      False -> do
          _ <- glCanvasSetCurrent v.canvas v.ctx

          -- First find the mouse position in the view coordinates
          let WXC.Point i j = mousePos event

          V2 cx cy <- varGet v.viewCenter
          V2 dx dy <- varGet v.viewDiameter

          WXC.Size w h <- windowGetClientSize v.canvas

          let x = cx + (fromIntegral i / fromIntegral w - 0.5) * dx
              y = cy - (fromIntegral j / fromIntegral h - 0.5) * dy
              m = V2 x y

          case event of
            -- Update mouse position
            MouseMotion _ _ -> do
              setUniform v.program "_mouse" $ m ^. vector2V
              varSet v.mousePosition m

            -- Stop dragging and pick a point (if pointer didn't move much)
            MouseLeftUp _ _ -> do
              t <- varGet v.currentTool
              case t of
                SelectPoint -> propagateEvent
                Navigate    -> do varSet v.dragStart Nothing

            -- Start dragging or pick a point
            MouseLeftDown _ _ -> do
              t <- varGet v.currentTool
              case t of
                  Navigate    -> do varSet v.dragStart (Just m)
                  SelectPoint -> mapM_ (pickPoint m v) viewerInfos

            -- Pan the view
            MouseLeftDrag _ _ -> do
              t <- varGet v.currentTool
              case t of
                SelectPoint -> mapM_ (pickPoint m v) viewerInfos

                Navigate -> do
                  ds <- varGet v.dragStart

                  case ds of
                    Nothing -> return ()

                    Just (V2 dsx dsy) -> do
                      let c = V2 (dsx - x + cx) (dsy - y + cy)
                          d = V2 dx dy
                          pm = getOrtho c d

                      varSet v.viewCenter c
                      varSet v.projMatrix pm
                      setUniform v.program "_projMatrix" $ pm ^. m44GLmatrix

                      windowRefresh v.canvas False

            -- Zoom in/out
            MouseWheel downward _ _ -> do
              let speed = 1.2
                  scalingFactor = if downward then speed else 1/speed

                  cxNew = x + scalingFactor * (cx - x)
                  cyNew = y + scalingFactor * (cy - y)
                  c = V2 cxNew cyNew

                  dxNew = dx * scalingFactor
                  dyNew = dy * scalingFactor
                  d = V2 dxNew dyNew
                  pm = getOrtho c d

              varSet v.viewCenter c
              varSet v.viewDiameter d

              varSet v.projMatrix pm
              setUniform v.program "_projMatrix" $ pm ^. m44GLmatrix

              windowRefresh v.canvas False

            -- Pass the event forward
            _ -> do propagateEvent

      True -> do
            -- Pass the event forward
            propagateEvent


openViewer :: [String] -> String -> Viewer -> IO ViewerInfo
openViewer vars header viewer
  = do
      -- Create top frame
      vFrame <- frameCreateTopFrame viewer.title

      -- Create GLCanvas and GLContext
      let initRect = Rect 0 0 viewer.width_pixels viewer.height_pixels
          options  = [ GL_RGBA, GL_MAJOR_VERSION 4, GL_DOUBLEBUFFER, GL_DEPTH_SIZE 16 ]

      canvas <- glCanvasCreateEx vFrame 0 initRect 0 "GLCanvas" options nullPalette

      ctx <- glContextCreateFromNull canvas
      _   <- glCanvasSetCurrent canvas ctx

      let w  = fill $ widget canvas
      windowSetLayout vFrame w

      -- Put smaller z in the back
      depthFunc $= Just Less

      -- Don't draw back faces
      cullFace $= Just Back

      -- Set background and buffers
      clearColor $= Color4 0.2 0.3 0.3 1.0
      clear [ ColorBuffer, DepthBuffer ]

      -- Create plane mesh object
      let projective = viewer.projective
      mesh <- if projective then createSphereMesh else createPlaneMesh

      -- Create color palette
      initTexture

      -- Compile shader programs
      let initialValueFormat = if projective
                                  then "vec4 %s = vec4(FragPos.xy, 1.0 + FragPos.z, 0.0);\n\
                                        \// c = uMobiusMatrix * c;\n"
                                  else "vec2 %s = FragPos.xy / FragPos.w;\n"
          initialValueCode = printf initialValueFormat viewer.coord

      program <- getProgram projective $ addHeader header initialValueCode viewer.code
      currentProgram $= Just program

      -- Set uniforms
      let aspect = fromIntegral viewer.width_pixels / fromIntegral viewer.height_pixels :: GLfloat
          c = V2 viewer.center_x viewer.center_y
          d = V2 (aspect * viewer.height) viewer.height
          m = V2 0.0 0.0 :: GLComplex
          n = viewer.max_iterations
          e = viewer.escape_radius
          r = viewer.convergence_radius
          pm = if projective
                  then L.perspective (45 * pi / 180) aspect 0.1 5.0
                  else getOrtho c d

      setUniform program "_mouse" $ m ^. vector2V
      setUniform program "_max_iterations" n
      setUniform program "_escape_radius" e
      setUniform program "_convergence_radius" r
      setUniform program "_projMatrix" $ pm ^. m44GLmatrix

      -- Set variable uniforms (for point picking)
      let var = "_" ++ viewer.coord
      mapM_ (\v -> setUniform program ("_" ++ v) (m ^. vector2V)) vars

      -- Set variables to keep track of the last uniform values
      viewCenter <- varCreate c
      viewDiameter <- varCreate d
      mousePosition <- varCreate m
      maxIterations <- varCreate n
      escapeRadius <- varCreate e
      convergenceRadius <- varCreate r
      projMatrix <- varCreate pm

      dragStart <- varCreate Nothing
      currentTool <- varCreate Navigate

      -- Add tools
      tools <- WX.menuPane [ WX.text WX.:= "&Tools"]

      _   <- WX.menuItem tools [ WX.text WX.:= "Navigate\tN"
                               , WX.help WX.:= "Drag to pan the view"
                               , WX.on WX.command WX.:= switchTool currentTool Navigate
                               ]
      _   <- WX.menuItem tools [ WX.text WX.:= "Select\tS"
                               , WX.help WX.:= "Click to select point"
                               , WX.on WX.command WX.:= switchTool currentTool SelectPoint
                               ]

      WX.menuLine tools
      WX.set vFrame [ WX.menuBar WX.:= [tools] ]

      -- Store all data in a record
      let viewerInfo = ViewerInfo{..}

      -- Set event handlers
      windowOnPaint canvas $ onPaint viewerInfo mesh
      windowOnSize canvas $ onSize viewerInfo
      windowOnKeyChar vFrame $ onKeyChar viewerInfo

      -- Show the frame
      _ <- windowShow vFrame
      windowRaise vFrame
      return viewerInfo

  where
    -- Run vertex and fragment shaders
    onPaint viewerInfo mesh _dc _rect
      = do
          _ <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
          clear [ ColorBuffer, DepthBuffer ]

          bindVertexArrayObject $= Just mesh.triangles
          drawArrays Triangles 0 (fromIntegral mesh.numVertices)

          flush
          _ <- glCanvasSwapBuffers viewerInfo.canvas

          return ()

    -- Change uniforms that track canvas dimensions
    onSize viewerInfo = case viewerInfo.projective of
      False -> do
          _ <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
          WXC.Size w h <- windowGetClientSize viewerInfo.canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          (V2 width height) <- varGet viewerInfo.viewDiameter
          c <- varGet viewerInfo.viewCenter

          let d = if w <= h
                    then V2 width $ fromIntegral h / fromIntegral w * width
                    else V2 (fromIntegral w / fromIntegral h * height) height

              pm = getOrtho c d

          varSet viewerInfo.viewDiameter d
          varSet viewerInfo.projMatrix pm
          setUniform viewerInfo.program "_projMatrix" $ pm ^. m44GLmatrix

      True -> do
          _ <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
          WXC.Size w h <- windowGetClientSize viewerInfo.canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          let aspect = fromIntegral w / fromIntegral h :: GLfloat
              fovRatio = if h > w then 1/aspect else 1.0
              pm = L.perspective (fovRatio * 45 * pi / 180) aspect 0.1 5.0

          varSet viewerInfo.projMatrix pm
          setUniform viewerInfo.program "_projMatrix" $ pm ^. m44GLmatrix


    onKeyChar viewerInfo eventKey
      = case eventKey of
          EventKey (KeyChar 'n') _ _ -> switchTool viewerInfo.currentTool Navigate

          EventKey (KeyChar 's') _ _ -> switchTool viewerInfo.currentTool SelectPoint

          _ -> propagateEvent

type GLComplex = V2 GLfloat

data Tool = Navigate | SelectPoint
  deriving (Eq, Show)

data ViewerInfo = ViewerInfo
  { vFrame            :: WX.Frame ()
  , program           :: Program
  , canvas            :: GLCanvas ()
  , ctx               :: GLContext ()
  , projective        :: Bool
  , projMatrix        :: Var (M44 GLfloat)
  , viewCenter        :: Var GLComplex
  , viewDiameter      :: Var GLComplex
  , maxIterations     :: Var GLint
  , escapeRadius      :: Var GLfloat
  , convergenceRadius :: Var GLfloat
  , mousePosition     :: Var GLComplex
  , dragStart         :: Var (Maybe GLComplex)
  , currentTool       :: Var Tool
  , var               :: String
  }

setUniform :: Uniform a => Program -> String -> a -> IO ()
setUniform p var_ val = do
  -- TODO: figure out error checking here
  location <- get (uniformLocation p var_)
  uniform location $= val

getOrtho :: Floating a => V2 a -> V2 a -> M44 a
getOrtho (V2 centerX centerY) (V2 diameterX diameterY) =
  L.inverseOrtho left right bottom top near far
    where left = centerX - diameterX / 2
          right = centerX + diameterX / 2
          bottom = centerY - diameterY / 2
          top = centerY + diameterY / 2
          near = -1
          far = 1

getProgram :: Bool -> String -> IO Program
getProgram projective fragSource = do
  loadShaders [ ShaderInfo VertexShader $ StringSource $ vertexCode projective
              , ShaderInfo FragmentShader $ StringSource fragSource
              ]

pickPoint :: GLComplex -> ViewerInfo -> ViewerInfo -> IO ()
pickPoint mousePointer v viewerInfo = do
  _   <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
  setUniform viewerInfo.program v.var $ mousePointer ^. vector2V
  windowRefresh viewerInfo.canvas False

switchTool :: Var Tool -> Tool -> IO ()
switchTool = varSet

vertexCode :: Bool -> String
vertexCode = \case
  False -> "#version 410 core\n\
            \uniform mat4 _projMatrix;\n\
            \\n\
            \layout (location = 0) in vec2 pos;\n\
            \\n\
            \out vec4 FragPos;\n\
            \\n\
            \void main() {\n\
            \  FragPos = _projMatrix * vec4(pos, 0.0, 1.0);\n\
            \  gl_Position = vec4(pos, 0.0, 1.0);\n\
            \}"

  True -> "#version 410 core\n\
          \uniform mat4 _projMatrix;\n\
          \\n\
          \layout (location = 0) in vec3 pos;\n\
          \\n\
          \out vec4 FragPos;\n\
          \\n\
          \void main() {\n\
          \  FragPos = vec4(pos, 1.0);\n\
          \  gl_Position = _projMatrix * (vec4(pos, 1.0) - vec4(0.0, 0.0, 3.0, 0.0));\n\
          \}"

fragConstants :: String
fragConstants = "\n\
  \#define M_PI 3.1415926535897932384626433832795\n\
  \#define EULER 2.718281828459045235360287471352\n\
  \#define WHITE (vec4(1.0, 1.0, 1.0, 1.0))\n\
  \#define BLACK (vec4(0.0, 0.0, 0.0, 1.0))\n\
  \#define RED (vec4(1.0, 0.0, 0.0, 1.0))\n\
  \#define GREEN (vec4(0.0, 1.0, 0.0, 1.0))\n\
  \#define BLUE (vec4(0.0, 0.0, 1.0, 1.0))\n\
  \#define YELLOW (vec4(1.0, 1.0, 0.0, 1.0))\n\
  \#define MAGENTA (vec4(1.0, 0.0, 1.0, 1.0))\n\
  \#define CYAN (vec4(0.0, 1.0, 1.0, 1.0))\n\
  \\n"

fragUniforms :: String
fragUniforms = "\n\
  \uniform vec2 _mouse;\n\
  \uniform int _max_iterations;\n\
  \uniform float _escape_radius;\n\
  \uniform float _convergence_radius;\n\
  \uniform sampler1D uTexture;\n\
  \\n"

baseFunctions :: String
baseFunctions =
  "\n\
  \// Complex Viewer Functions\n\
  \// Complex Multiplication\n\
  \vec2 _cMul(vec2 a, vec2 b) {\n\
  \  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);\n\
  \}\n\
  \\n\
  \// Complex Division\n\
  \vec2 _cDiv(vec2 a, vec2 b) {\n\
  \  return vec2(a.x * b.x + a.y * b.y, a.y * b.x - a.x * b.y) / (b.x * b.x + b.y * b.y);\n\
  \}\n\
  \\n\
  \// Complex square root\n\
  \vec2 _sqrt(vec2 a) {\n\
  \  float sqrtR = sqrt(length(a));\n\
  \  float theta = atan(a.y, a.x);\n\
  \  return vec2(sqrtR * cos(theta / 2.0), sqrtR * sin(theta / 2.0));\n\
  \} \n\
  \\n\
  \// Exponentiation to a real power (with any base)\n\
  \vec2 _cPow(vec2 a, float b) {\n\
  \  float powR = pow(length(a), b);\n\
  \  float theta = atan(a.y, a.x);\n\
  \  return vec2(powR * cos(b * theta), powR * sin(b * theta));\n\
  \}\n\
  \\n\
  \// Complex Exponential\n\
  \vec2 _cExp(vec2 a) {\n\
  \  float expR = exp(a.x); \n\
  \  return vec2(expR * cos(a.y), expR * sin(a.y));\n\
  \}\n\
  \\n\
  \// Complex Logarithm\n\
  \vec2 _cLog(vec2 a) {\n\
  \  return vec2(log(length(a)), atan(a.y, a.x));\n\
  \}\n\
  \\n\
  \\n\
  \// Projective Viewer Functions\
  \// Complex Addition in Projective Coordinates\n\
  \vec4 _pAdd(vec4 a, vec4 b) {\n\
  \  return normalize(vec4(\n\
  \    a.x * b.z - a.y * b.w + a.z * b.x - a.w * b.y,\n\
  \    a.x * b.w + a.y * b.z + a.z * b.y + a.w * b.x,\n\
  \    a.z * b.z - a.w * b.w,\n\
  \    a.z * b.w + a.w * b.z\n\
  \  ));\n\
  \}\n\
  \\n\
  \// Complex Subtraction in Projective Coordinates\n\
  \vec4 _pSub(vec4 a, vec4 b) {\n\
  \  return normalize(vec4(\n\
  \    a.x * b.z - a.y * b.w - a.z * b.x + a.w * b.y,\n\
  \    a.x * b.w + a.y * b.z - a.z * b.y - a.w * b.x,\n\
  \    a.z * b.z - a.w * b.w,\n\
  \    a.z * b.w + a.w * b.z\n\
  \  ));\n\
  \}\n\
  \\n\
  \// Complex Additive Inverse in Projective Coordinates\n\
  \vec4 _pOpp(vec4 a) {return vec4(-a.xy, a.zw);}\n\
  \\n\
  \// Complex Multiplication in Projective Coordinates\n\
  \vec4 _pMul(vec4 a, vec4 b) {\n\
  \  return normalize(vec4(\n\
  \    a.x * b.x - a.y * b.y,\n\
  \    a.x * b.y + a.y * b.x,\n\
  \    a.z * b.z - a.w * b.w,\n\
  \    a.z * b.w + a.w * b.z\n\
  \  ));\n\
  \}\n\
  \\n\
  \// Complex Division in Projective Coordinates\n\
  \vec4 _pDiv(vec4 a, vec4 b) {\n\
  \  return normalize(vec4(\n\
  \    a.x * b.z - a.y * b.w,\n\
  \    a.x * b.w + a.y * b.z,\n\
  \    a.z * b.x - a.w * b.y,\n\
  \    a.z * b.y + a.w * b.x\n\
  \  ));\n\
  \}\n\
  \\n\
  \// Complex Multiplicative Inverse in Projective Coordinates\n\
  \vec4 _pInv(vec4 a) {return vec4(a.zw, a.xy);}\n\
  \\n\
  \// Distance in the Complex Projective Line\n\
  \// Assumes that both vec4s are normalized\n\
  \float _pDist(vec4 a, vec4 b) {\n\
  \  return length(vec2(\n\
  \    a.x * b.z - a.y * b.w - a.z * b.x + a.w * b.y,\n\
  \    a.x * b.w + a.y * b.z + a.z * b.y + a.w * b.x\n\
  \  ));\n\
  \}"

addHeader :: String -> String -> String -> String
addHeader = printf
  "#version 410 core \n\
  \%s\
  \%s\
  \\n\
  \%s\
  \\n\
  \in vec4 FragPos;\n\
  \out vec4 color;\n\
  \%s\
  \void main() {\n\
  \  %s\n\
  \  %s\n\
  \}" fragConstants fragUniforms baseFunctions