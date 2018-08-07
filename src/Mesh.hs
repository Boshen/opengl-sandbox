module Mesh ( buildMesh ) where

import           Control.Monad.State.Strict
import           Foreign.Marshal.Array
import           Foreign.Ptr
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Chunk                      (blockSize)
import           States

buildMesh :: Game Mesh
buildMesh = do
  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  liftIO $ do
    GL.bindVertexArrayObject $= Just vao
    GL.bindBuffer GL.ArrayBuffer $= Just vbo

    withArray vertices $ \ptr ->
      GL.bufferData GL.ArrayBuffer $= (fromIntegral $ length vertices * 4, ptr, GL.StreamDraw)

    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 0) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 24 (intPtrToPtr 0))

    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    GL.vertexAttribPointer (GL.AttribLocation 1) $=
      (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 24 (intPtrToPtr 12))

  return $ Mesh { meshVAO = vao
                , meshVBO = vbo
                }

vertices :: [Float]
vertices = concat [ vertex (V3 x y z) | x <- arr, y <- arr, z <- arr ]
  where arr = [0..blockSize - 1]

vertex :: V3 Int -> [Float]
vertex pos = concat vx
  where
      (V3 x y z) = fromIntegral <$> pos
      n1 = [0, 0, 1]
      n2 = [0, 0, -1]
      n3 = [1, 0, 0]
      n4 = [-1, 0, 0]
      n5 = [0, -1, 0]
      n6 = [0, 1, 0]
      p1 = [x + 0,  y + 1,  z + 1]
      p2 = [x + 0,  y + 0,  z + 1]
      p3 = [x + 1,  y + 1,  z + 1]
      p4 = [x + 1,  y + 0,  z + 1]
      p5 = [x + 0,  y + 0,  z + 0]
      p6 = [x + 0,  y + 1,  z + 0]
      p7 = [x + 1,  y + 0,  z + 0]
      p8 = [x + 1,  y + 1,  z + 0]
      vx = [ p1, n1 , p2, n1 , p3, n1 , p4, n1
           , p5, n2 , p6, n2 , p7, n2 , p8, n2
           , p4, n3 , p7, n3 , p3, n3 , p8, n3
           , p2, n4 , p1, n4 , p5, n4 , p6, n4
           , p2, n5 , p5, n5 , p4, n5 , p7, n5
           , p6, n6 , p1, n6 , p8, n6 , p3, n6
           ]
