{-# LANGUAGE OverloadedRecordDot, RecordWildCards, FlexibleContexts, LambdaCase #-}
module Viewer ( openViewers ) where

import Backend.GLSL (compileViewerScript)

import Control.Lens
import Linear
import Linear.OpenGL
import Text.Printf

import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL hiding ( normalize )

import Linear.Projection as L
import qualified Graphics.UI.WX as WX
import qualified Graphics.UI.WXCore as WXC
import qualified Graphics.Rendering.OpenGL as GL

import Config
import Meshes
import Palette
import Shaders

openViewers :: FilePath -> IO ()
openViewers configPath = do
    -- Load config file
    c <- loadConfig configPath

    -- Create header for global uniform variables
    let varNames = map coord c.viewers
        header = concatMap (printf "uniform vec2 _%s;\n") varNames

    -- Open all viewers
    viewerInfos <- mapM (openViewer varNames header) c.viewers

    -- Add mouse events last, because they might affect all viewers
    mapM_ (\v -> windowOnMouse v.canvas True $ onMouse viewerInfos v) viewerInfos

    -- Close all viewers of the project at the same time
    -- This is to avoid setting variables out of scope.
    -- TODO: Add error checking to avoid these errors in general.
    mapM_ (\v -> windowOnClose v.vFrame $ onClose viewerInfos) viewerInfos

  where
    onClose = mapM_ (\v -> windowDestroy v.vFrame)

    onMouse viewerInfos ViewerInfo{..} event = case viewType of
      ComplexPlane -> do
          _ <- glCanvasSetCurrent canvas ctx

          -- First find the mouse position in the view coordinates
          let WXC.Point i j = mousePos event

          c0@(V2 cx cy) <- varGet viewCenter
          d0@(V2 dx dy) <- varGet viewDiameter

          WXC.Size w h <- windowGetClientSize canvas

          let x = cx + (fromIntegral i / fromIntegral w - 0.5) * dx
              y = cy - (fromIntegral j / fromIntegral h - 0.5) * dy
              m = V2 x y

          case event of
            -- Update mouse position
            MouseMotion _ _ -> do
              setUniform program "_mouse" $ m ^. vector2V
              varSet mousePosition m

            -- Stop dragging
            MouseLeftUp _ _ -> do
              t <- varGet currentTool
              case t of
                SelectPoint -> propagateEvent
                DragView    -> do varSet dragStart Nothing

            -- Start dragging or pick a point
            MouseLeftDown _ _ -> do
              t <- varGet currentTool
              case t of
                  DragView    -> do varSet dragStart (Just m)
                  SelectPoint -> mapM_ (pickPoint m varName) viewerInfos

            -- Pan the view or change point continuously
            MouseLeftDrag _ _ -> do
              t <- varGet currentTool
              case t of
                SelectPoint -> mapM_ (pickPoint m varName) viewerInfos

                DragView -> do
                  maybe_ds <- varGet dragStart

                  case maybe_ds of
                    Nothing -> return ()

                    Just ds -> do
                      let c = ds ^-^ m ^+^ c0
                          pm = getOrtho c d0

                      varSet viewCenter c
                      varSet projMatrix pm
                      setUniform program "_projMatrix" $ pm ^. m44GLmatrix

                      windowRefresh canvas False

            -- Zoom in/out
            MouseWheel downward _ _ -> do
              let speed = 1.2
                  scalingFactor = if downward then speed else 1/speed

                  c = m ^+^ (c0 - m) ^* scalingFactor

                  d = scalingFactor *^ d0
                  pm = getOrtho c d

              varSet viewCenter c
              varSet viewDiameter d

              varSet projMatrix pm
              setUniform program "_projMatrix" $ pm ^. m44GLmatrix

              windowRefresh canvas False

            -- Pass the event forward
            _ -> propagateEvent

      ProjectiveSpace -> do
          _ <- glCanvasSetCurrent canvas ctx

          -- First find the mouse position in the view coordinates
          let WXC.Point i j = mousePos event
          WXC.Size w h <- windowGetClientSize canvas

          let aspect = fromIntegral w / fromIntegral h :: GLfloat
              x = 2 * (fromIntegral i / fromIntegral w - 0.5) :: GLfloat
              y = - 2 * (fromIntegral j / fromIntegral h - 0.5) :: GLfloat
              m = V2 x y

          case event of
            -- Update mouse position
            MouseMotion _ _ -> do
              setUniform program "_mouse" $ m ^. vector2V
              varSet mousePosition m

            -- Stop dragging
            MouseLeftUp _ _ -> do
              varSet dragStart Nothing

            -- Start dragging
            MouseLeftDown _ _ -> do
              t <- varGet currentTool
              case t of
                  DragView    -> do varSet dragStart (Just m)
                  SelectPoint -> do
                    -- putStrLn $ show m

                    -- _invLocal <- inv44 <$> varGet localMatrix
                    -- invProj <- inv44 <$> varGet projMatrix

                    -- let direction = invProj !* (V4 x y (-3) 0)
                    -- putStrLn $ show direction

                    mapM_ (pickPoint m varName) viewerInfos

            -- Rotate sphere
            MouseLeftDrag _ _ -> do
              t <- varGet currentTool

              case t of
                SelectPoint -> do
                  mapM_ (pickPoint m varName) viewerInfos

                DragView -> do
                  ds <- varGet dragStart

                  case ds of
                    Nothing -> return ()

                    Just m0 -> do
                      local0 <- varGet localMatrix

                      -- TODO: check if I can simplify the computations below

                      -- Take the axis perpendicular to the movement
                      -- Get the quaternion representing the rotation
                      -- Apply rotation along the axis to the coordinate system

                      let V2 dx dy = m ^-^ m0
                          axis = (inv33 $ local0 ^._m33) !* (V3 (-dy) (aspect * dx) 0)
                          q = axisAngle axis $ norm axis
                          local = local0 !*! (m33_to_m44 $ fromQuaternion q)

                      setUniform program "_localMatrix" $ local ^. m44GLmatrix
                      varSet localMatrix local

                      -- dragStart only serves as the last position in 3D
                      -- TODO: Maybe I should make 2D relative to last position too
                      varSet dragStart $ Just m

                      windowRefresh canvas False

            MouseWheel downward _ _ -> do
              local <- varGet localMatrix
              mobius0 <- varGet mobiusMatrix

              -- TODO: check if I can simplify the computations below

              -- The computation below applies a hyperbolic isometry in H^3 to S^2
              -- The two fixed points are v and -v where v points to the viewer

              let invLocal = inv44 local
                  northPole = toProjective $ invLocal !* V4 0 0 1 0
                  southPole = toProjective $ invLocal !* V4 0 0 (-1) 0

                  speed = 1.2
                  scalingFactor = if downward then speed else 1/speed

                  mobius = mobius0 !*! (hyperbolicMobius northPole southPole scalingFactor)

              varSet mobiusMatrix mobius
              setUniform program "_mobiusMatrix" $ mobius ^. m44GLmatrix

              windowRefresh canvas False

            -- Pass the event forward
            _ -> propagateEvent


openViewer :: [String] -> String -> Viewer -> IO ViewerInfo
openViewer vars header Viewer{..} = do
    -- Create top frame
    vFrame <- frameCreateTopFrame title

    -- Create GLCanvas and GLContext
    let initRect = Rect 0 0 width_pixels height_pixels
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
    let viewType = if is_projective then ProjectiveSpace else ComplexPlane

    mesh <- case viewType of
              ProjectiveSpace -> createSphereMesh
              ComplexPlane -> createPlaneMesh

    -- Create color palette
    initTexture

    -- Compile shader programs
    let initialValueFormat = case viewType of
            ComplexPlane    -> "vec2 %s = FragPos.xy / FragPos.w;\n"
            ProjectiveSpace -> "vec4 %s = _mobiusMatrix * vec4(FragPos.xy, 1.0 + FragPos.z, 0.0);\n"
        initialValueCode = printf initialValueFormat coord

    glslCode <- case fs_code of
      Nothing  -> pure code
      Just src -> case compileViewerScript is_projective coord src of
        Left err  -> do
          putStrLn $ "FS compile error: " ++ err
          putStrLn "Falling back to raw GLSL."
          pure code
        Right gen -> pure gen

    program <- getProgram is_projective $ addHeader header initialValueCode glslCode
    currentProgram $= Just program

    -- Set uniforms
    let aspect = fromIntegral width_pixels / fromIntegral height_pixels :: GLfloat

        initialMaxIterations     = max_iterations
        initialEscapeRadius      = escape_radius
        initialConvergenceRadius = convergence_radius

        initialViewCenter        = V2 center_x center_y
        initialViewDiameter      = V2 (aspect * view_height) view_height
        initialMousePosition     = V2 0.0 0.0 :: GLComplex

        initialLocalMatrix       = identity :: GLMatrix
        initialMobiusMatrix      = identity :: GLMatrix

        initialProjectionMatrix  = case viewType of
            ProjectiveSpace -> L.perspective (45 * pi / 180) aspect 0.1 5.0
            ComplexPlane    -> getOrtho initialViewCenter initialViewDiameter

        -- Only relevant for projective views
        initialInverseProjectionMatrix = case viewType of
            ProjectiveSpace -> L.inversePerspective (45 * pi / 180) aspect 0.1 5.0
            ComplexPlane    -> identity :: GLMatrix

    setUniform program "_max_iterations"     initialMaxIterations
    setUniform program "_escape_radius"      initialEscapeRadius
    setUniform program "_convergence_radius" initialConvergenceRadius
    setUniform program "_mouse"              $ initialMousePosition ^. vector2V
    setUniform program "_projMatrix"         $ initialProjectionMatrix ^. m44GLmatrix
    setUniform program "_localMatrix"        $ initialLocalMatrix ^. m44GLmatrix
    setUniform program "_mobiusMatrix"       $ initialMobiusMatrix ^. m44GLmatrix

    -- Set variable uniforms (for point picking)
    let varName = "_" ++ coord
    mapM_ (\v -> setUniform program ("_" ++ v) (initialMousePosition ^. vector2V)) vars

    -- Set variables to keep track of the last uniform values
    maxIterations     <- varCreate initialMaxIterations
    escapeRadius      <- varCreate initialEscapeRadius
    convergenceRadius <- varCreate initialConvergenceRadius

    viewCenter        <- varCreate initialViewCenter
    viewDiameter      <- varCreate initialViewDiameter
    mousePosition     <- varCreate initialMousePosition

    localMatrix       <- varCreate initialLocalMatrix
    mobiusMatrix      <- varCreate initialMobiusMatrix

    projMatrix        <- varCreate initialProjectionMatrix
    invProjMatrix     <- varCreate initialInverseProjectionMatrix
    dragStart         <- varCreate Nothing
    currentTool       <- varCreate DragView

    -- Store all data in a record
    let viewerInfo = ViewerInfo{..}

    -- Add tools
    tools <- WX.menuPane [ WX.text WX.:= "&Tools"]

    _   <- WX.menuItem tools [ WX.text WX.:= "Drag view\tD"
                              , WX.help WX.:= "Drag to pan the view"
                              , WX.on WX.command WX.:= switchTool currentTool DragView
                              ]
    _   <- WX.menuItem tools [ WX.text WX.:= "Select point\tS"
                              , WX.help WX.:= "Click to select point"
                              , WX.on WX.command WX.:= switchTool currentTool SelectPoint
                              ]

    _   <- WX.menuItem tools [ WX.text WX.:= "Reset view\tR"
                              , WX.help WX.:= "Reset view to initial state"
                              , WX.on WX.command WX.:= resetView viewerInfo
                              ]

    WX.menuLine tools
    WX.set vFrame [ WX.menuBar WX.:= [tools] ]

    -- Set event handlers
    windowOnPaint   canvas $ onPaint viewerInfo mesh
    windowOnSize    canvas $ onSize viewerInfo
    windowOnKeyChar vFrame $ onKeyChar viewerInfo

    -- Show the frame
    _ <- windowShow vFrame
    windowRaise vFrame
    return viewerInfo

  where
    -- Run vertex and fragment shaders
    onPaint ViewerInfo{..} mesh _dc _rect = do
      _ <- glCanvasSetCurrent canvas ctx
      clear [ ColorBuffer, DepthBuffer ]

      bindVertexArrayObject $= Just mesh.triangles
      drawArrays Triangles 0 (fromIntegral mesh.numVertices)

      flush
      _ <- glCanvasSwapBuffers canvas

      return ()

    -- Change uniforms that track canvas dimensions
    onSize ViewerInfo{..} = case viewType of
      ComplexPlane -> do
          _ <- glCanvasSetCurrent canvas ctx
          WXC.Size w h <- windowGetClientSize canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          (V2 width height) <- varGet viewDiameter
          c <- varGet viewCenter

          let d = if w <= h
                    then V2 width $ fromIntegral h / fromIntegral w * width
                    else V2 (fromIntegral w / fromIntegral h * height) height

              pm = getOrtho c d

          varSet viewDiameter d
          varSet projMatrix pm
          setUniform program "_projMatrix" $ pm ^. m44GLmatrix

      ProjectiveSpace -> do
          _ <- glCanvasSetCurrent canvas ctx
          WXC.Size w h <- windowGetClientSize canvas
          viewport $= (Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))

          let aspect = fromIntegral w / fromIntegral h :: GLfloat
              fovRatio = if h > w then 1 / aspect else 1.0
              pm = L.perspective (fovRatio * 45 * pi / 180) aspect 0.1 5.0

          varSet projMatrix pm
          setUniform program "_projMatrix" $ pm ^. m44GLmatrix


    onKeyChar viewerInfo eventKey
      = case eventKey of
          EventKey (KeyChar 'd') _ _ -> switchTool viewerInfo.currentTool DragView

          EventKey (KeyChar 's') _ _ -> switchTool viewerInfo.currentTool SelectPoint

          EventKey (KeyChar 'r') _ _ -> resetView viewerInfo

          _ -> propagateEvent

type GLComplex    = V2 GLfloat
type GLVector     = V4 GLfloat
type GLProjective = V4 GLfloat
type GLMatrix     = M44 GLfloat

data ViewType = ComplexPlane | ProjectiveSpace

data Tool = DragView | SelectPoint
  deriving (Eq, Show)

data ViewerInfo = ViewerInfo
  { vFrame              :: WX.Frame ()

  -- OpenGL setup
  , program             :: Program
  , canvas              :: GLCanvas ()
  , ctx                 :: GLContext ()
  , varName             :: String

  -- Viewer State
  , mousePosition       :: Var GLComplex
  , dragStart           :: Var (Maybe GLComplex)
  , projMatrix          :: Var GLMatrix
  , invProjMatrix       :: Var GLMatrix

  -- Options
  , currentTool         :: Var Tool

  -- Dynamical variables
  , maxIterations       :: Var GLint
  , escapeRadius        :: Var GLfloat
  , convergenceRadius   :: Var GLfloat

  -- Flag for 2D/3D view
  , viewType            :: ViewType

  -- 2D view variables
  , viewCenter          :: Var GLComplex
  , viewDiameter        :: Var GLComplex
  , initialViewCenter   :: GLComplex
  , initialViewDiameter :: GLComplex

  -- 3D view variables
  , localMatrix         :: Var GLMatrix
  , mobiusMatrix        :: Var GLMatrix
  }

setUniform :: Uniform a => Program -> String -> a -> IO ()
setUniform p varName val = do
  -- TODO: figure out error checking here
  location <- get (uniformLocation p varName)
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
getProgram is_projective fragSource = do
  loadShaders [ ShaderInfo VertexShader $ StringSource $ vertexCode is_projective
              , ShaderInfo FragmentShader $ StringSource fragSource
              ]

pickPoint :: GLComplex -> String -> ViewerInfo -> IO ()
pickPoint mousePointer pointVarName ViewerInfo{..} = do
  _   <- glCanvasSetCurrent canvas ctx
  setUniform program pointVarName $ mousePointer ^. vector2V
  windowRefresh canvas False

switchTool :: Var Tool -> Tool -> IO ()
switchTool = varSet

resetView :: ViewerInfo -> IO ()
resetView ViewerInfo{..} = case viewType of
  ComplexPlane -> do
    let c = initialViewCenter
        d = initialViewDiameter
        pm = getOrtho c d

    varSet viewCenter c
    varSet viewDiameter d
    varSet projMatrix pm

    setUniform program "_projMatrix" $ pm ^. m44GLmatrix

    windowRefresh canvas False

  ProjectiveSpace -> do
    varSet localMatrix identity
    varSet mobiusMatrix identity

    setUniform program "_localMatrix" (identity :: GLMatrix)
    setUniform program "_mobiusMatrix" (identity :: GLMatrix)

    windowRefresh canvas False

toProjective :: GLVector -> GLProjective
toProjective v@(V4 v1 v2 v3 _) =
  if distance v (V4 0 0 (-1) 0) < 1e-5
    then V4 1 0 0 0
    else normalize $ V4 v1 v2 (1 + v3) 0

hyperbolicMobius :: GLProjective -> GLProjective -> GLfloat -> GLMatrix
hyperbolicMobius (V4 p1 p2 p3 p4) (V4 q1 q2 q3 q4) scalingFactor =
    V4 (V4   a   b   c   d)
       (V4 (-b)  a (-d)  c)
       (V4   e   f   g   h)
       (V4 (-f)  e (-h)  g)
    where
      a = p1 * q3 - p2 * q4 - scalingFactor * (p3 * q1 - p4 * q2)
      b = p1 * q4 + p2 * q3 - scalingFactor * (p3 * q2 + p4 * q1)
      c = (1 - scalingFactor) * (p3 * q3 - p4 * q4)
      d = (1 - scalingFactor) * (p3 * q4 + p4 * q3)
      e = (scalingFactor - 1) * (p1 * q1 - p2 * q2)
      f = (scalingFactor - 1) * (p1 * q2 + p2 * q1)
      g = scalingFactor * (p1 * q3 - p2 * q4) - p3 * q1 - p4 * q2
      h = scalingFactor * (p1 * q4 + p2 * q3) - p3 * q2 + p4 * q1

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
          \uniform mat4 _localMatrix;\n\
          \uniform mat4 _projMatrix;\n\
          \\n\
          \layout (location = 0) in vec3 pos;\n\
          \\n\
          \out vec4 FragPos;\n\
          \\n\
          \void main() {\n\
          \  FragPos = vec4(pos, 1.0);\n\
          \  gl_Position = _projMatrix * (_localMatrix * vec4(pos, 1.0) - vec4(0.0, 0.0, 3.0, 0.0));\n\
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
  \uniform int _max_iterations;\n\
  \uniform float _escape_radius;\n\
  \uniform float _convergence_radius;\n\
  \uniform vec2 _mouse;\n\
  \uniform mat4 _mobiusMatrix;\n\
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