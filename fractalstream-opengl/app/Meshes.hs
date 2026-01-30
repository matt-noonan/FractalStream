{-# LANGUAGE RankNTypes #-}
module Meshes ( createPlaneMesh, createSphereMesh, VAOInfo(..) ) where

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Graphics.Rendering.OpenGL

import Data.List.NonEmpty (NonEmpty(..), toList)
import qualified Data.List.NonEmpty as NE

createPlaneMesh :: IO VAOInfo
createPlaneMesh = createVAO StaticDraw 2 $
  Vertex2 (-1.0) (-1.0) :| -- Triangle 1
  [ Vertex2   1.0  (-1.0)
  , Vertex2 (-1.0)   1.0
  , Vertex2   1.0  (-1.0) -- Triangle 2
  , Vertex2   1.0    1.0
  , Vertex2 (-1.0)   1.0
  ]

createSphereMesh :: IO VAOInfo
createSphereMesh = createVAO StaticDraw 3 $ toSphere
                    <$> (createCubeSubdivision 4 >>= unpack)

data VAOInfo = VAOInfo
  { triangles   :: VertexArrayObject
  , numVertices :: Int
  }

createVAO :: (Vertex (v GLfloat), Storable (v GLfloat), Show (v GLfloat))
          => BufferUsage -> GLint -> NonEmpty (v GLfloat) -> IO VAOInfo
createVAO bufferUsage stride vertices = do
  -- Create VAO
  planeVAO <- genObjectName
  bindVertexArrayObject $= Just planeVAO

  -- Create and bind VBO to VAO
  let n = length vertices
      vertexSize = sizeOf $ NE.head vertices

  planeVBO <- genObjectName
  bindBuffer ArrayBuffer $= Just planeVBO
  withArray (toList vertices) $ \ptr -> do
    let size = fromIntegral (n * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, bufferUsage)

  let vPosition = AttribLocation 0
      vDescriptor = VertexArrayDescriptor stride Float 0 nullPtr

  vertexAttribPointer vPosition $= (ToFloat, vDescriptor)
  vertexAttribArray vPosition $= Enabled

  return $ VAOInfo planeVAO n

midpoint :: Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat
midpoint (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) =
  Vertex3 ((x1 + x2)/2) ((y1 + y2)/2) ((z1 + z2)/2)

data Triangle3 = Triangle3 (Vertex3 GLfloat) (Vertex3 GLfloat) (Vertex3 GLfloat)

subdivideTriangle :: Triangle3 -> NonEmpty Triangle3
subdivideTriangle (Triangle3 v1 v2 v3) =
  Triangle3 v12 v23 v31 :|
    [ Triangle3 v1 v12 v31
    , Triangle3 v2 v23 v12
    , Triangle3 v3 v31 v23
    ]
  where
    v12 = midpoint v1 v2
    v23 = midpoint v2 v3
    v31 = midpoint v3 v1

-- subdivideTriangles :: NonEmpty Triangle3 -> NonEmpty Triangle3
-- subdivideTriangles = join $ subdivideTriangle <$>

createCubeSubdivision :: Int -> NonEmpty Triangle3
createCubeSubdivision n
  -- Position of faces are with respect to the screen
  = fn $ Triangle3 v000 v010 v100 :| [Triangle3 v010 v110 v100 -- Back face
          , Triangle3 v000 v100 v001, Triangle3 v001 v100 v101 -- Bottom face
          , Triangle3 v000 v001 v011, Triangle3 v000 v011 v010 -- Left face
          , Triangle3 v100 v110 v101, Triangle3 v101 v110 v111 -- Right face
          , Triangle3 v010 v011 v111, Triangle3 v010 v111 v110 -- Top face
          , Triangle3 v001 v101 v011, Triangle3 v011 v101 v111 -- Front face
          ]

  where
    v000 = Vertex3 (-1.0) (-1.0) (-1.0 :: GLfloat)
    v001 = Vertex3 (-1.0) (-1.0)  (1.0 :: GLfloat)
    v010 = Vertex3 (-1.0)   1.0  (-1.0 :: GLfloat)
    v011 = Vertex3 (-1.0)   1.0   (1.0 :: GLfloat)
    v100 = Vertex3   1.0  (-1.0) (-1.0 :: GLfloat)
    v101 = Vertex3   1.0  (-1.0)  (1.0 :: GLfloat)
    v110 = Vertex3   1.0    1.0  (-1.0 :: GLfloat)
    v111 = Vertex3   1.0    1.0   (1.0 :: GLfloat)

    -- Subdivide triangles n times
    fn = foldr (.) id $ replicate n (join . NE.map subdivideTriangle)

unpack :: Triangle3 -> NonEmpty (Vertex3 GLfloat)
unpack (Triangle3 a b c) = a :| [b, c]

toSphere :: Vertex3 GLfloat -> Vertex3 GLfloat
toSphere (Vertex3 x y z) = Vertex3 a b c where
    d = sqrt (x*x + y*y + z*z)
    a = x / d
    b = y / d
    c = z / d
