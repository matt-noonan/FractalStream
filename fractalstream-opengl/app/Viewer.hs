{-# LANGUAGE OverloadedRecordDot, RecordWildCards, FlexibleContexts, LambdaCase #-}
module Viewer ( openViewers ) where

import Text.Printf

import Graphics.UI.WXCore hiding ( when )
import Graphics.Rendering.OpenGL

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

    onMouse viewerInfos v event
      = do
          _ <- glCanvasSetCurrent v.canvas v.ctx

          -- First find the mouse position in the view coordinates
          let WXC.Point i j = mousePos event

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
                    Just (Vector2 dsx dsy) -> do
                      let v' = Vector2 (dsx - x + cx) (dsy - y + cy)

                      setUniform v.program "_center" v'
                      varSet v.viewCenter v'

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
      clearColor $= Color4 0 0 0 1
      clear [ ColorBuffer, DepthBuffer ]

      -- Create plane mesh object
      mesh <- if viewer.projective then createSphereMesh else createPlaneMesh

      -- Create color palette
      initTexture

      -- Compile shader programs
      let initialValueFormat = if viewer.projective
                                  then "vec4 %s = vec4(FragPos.xy, 1.0 + FragPos.z, 0.0);\n\
                                        \// c = uMobiusMatrix * c;\n"
                                  else "vec2 %s = _center + FragPos.xy * _diameter / 2.0;\n"
          initialValueCode = printf initialValueFormat viewer.coord

      program <- getProgram viewer.projective $ addHeader header initialValueCode viewer.code
      currentProgram $= Just program

      -- Set uniforms
      let aspect = fromIntegral viewer.height_pixels / fromIntegral viewer.width_pixels :: GLfloat
          c = Vector2 viewer.center_x viewer.center_y :: GLComplex
          d = Vector2 viewer.width (aspect * viewer.width) :: GLComplex
          m = Vector2 0.0 0.0 :: GLComplex
          n = viewer.max_iterations
          e = viewer.escape_radius
          r = viewer.convergence_radius

      setUniform program "_center" c
      setUniform program "_diameter" d
      setUniform program "_mouse" m
      setUniform program "_max_iterations" n
      setUniform program "_escape_radius" e
      setUniform program "_convergence_radius" r

      -- Set variable uniforms (for point picking)
      let var = "_" ++ viewer.coord
      mapM_ (\v -> setUniform program ("_" ++ v) m) vars

      -- Set variables to keep track of the last uniform values
      viewCenter <- varCreate c
      viewDiameter <- varCreate d
      mousePosition <- varCreate m
      maxIterations <- varCreate n
      escapeRadius <- varCreate e
      convergenceRadius <- varCreate r

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

    onKeyChar viewerInfo eventKey
      = case eventKey of
          EventKey (KeyChar 'n') _ _ -> switchTool viewerInfo.currentTool Navigate

          EventKey (KeyChar 's') _ _ -> switchTool viewerInfo.currentTool SelectPoint

          _ -> propagateEvent

type GLComplex = GL.Vector2 GLfloat

data Tool = Navigate | SelectPoint
  deriving (Eq, Show)

data ViewerInfo = ViewerInfo
  { vFrame            :: WX.Frame ()
  , program           :: Program
  , canvas            :: GLCanvas ()
  , ctx               :: GLContext ()
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

getProgram :: Bool -> String -> IO Program
getProgram projective fragSource = do
  loadShaders [ ShaderInfo VertexShader $ StringSource $ vertexCode projective
              , ShaderInfo FragmentShader $ StringSource fragSource
              ]

pickPoint :: GLComplex -> ViewerInfo -> ViewerInfo -> IO ()
pickPoint mousePointer v viewerInfo = do
  _   <- glCanvasSetCurrent viewerInfo.canvas viewerInfo.ctx
  setUniform viewerInfo.program v.var mousePointer
  windowRefresh viewerInfo.canvas False

switchTool :: Var Tool -> Tool -> IO ()
switchTool = varSet

vertexCode :: Bool -> String
vertexCode = \case
  False -> "#version 410 core\n\
            \\n\
            \layout (location = 0) in vec2 pos;\n\
            \\n\
            \out vec4 FragPos;\n\
            \\n\
            \void main() {\n\
            \  FragPos = vec4(pos, 0.0, 1.0);\n\
            \  gl_Position = FragPos;\n\
            \}"

  True -> "#version 410 core\n\
          \\n\
          \layout (location = 0) in vec3 pos;\n\
          \\n\
          \out vec4 FragPos;\n\
          \\n\
          \void main() {\n\
          \  FragPos = vec4(pos, 1.0);\n\
          \  gl_Position = vec4(0.95 * pos, 1.0) - vec4(0.0, 0.0, 1.0, 0.0);\n\
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
  \uniform vec2 _diameter;\n\
  \uniform vec2 _center;\n\
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