{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Graphics.VR.Pal
import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Sound.Pd1
import Data.Foldable
import Control.Monad

data Uniforms = Uniforms 
  { uMVP :: UniformLocation (M44 GLfloat) } 
  deriving Data

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer, GLsizei)
makeLine shader = do

  let verts = flip map [0..255] $ \i -> 
                let x = (fromIntegral (i::Int) / 255) * 2 - 1
                in V3 x 0 0
      vertCount = length verts
      normals = replicate vertCount (V3 0 0 1)
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts)
  normalsBuffer   <- bufferData GL_STATIC_DRAW (concatMap toList normals)

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignAttribute shader "aPosition" 3
    withArrayBuffer normalsBuffer   $ assignAttribute shader "aNormal" 3

  return (vao, positionsBuffer, fromIntegral vertCount)

main :: IO ()
main = do
  VRPal{..} <- initVRPal "Pd Mic FFT" NoGCPerFrame []
  addToLibPdSearchPath "test"
  patch <- makePatch "test/test-fft"
  
  let fftArrayName = local patch "mic-fft"

  shader    <- createShaderProgram "test/geo.vert" "test/geo.frag"
  Uniforms{..} <- acquireUniforms shader
  useProgram shader
  (lineVAO, lineBuffer, lineVertCount) <- makeLine shader

  glClearColor 0.01 0.01 0.05 1
  whileWindow gpWindow $ do

    let view = viewMatrix (V3 0 0 5) (axisAngle (V3 0 1 0) 0)
    projection <- getWindowProjection gpWindow 45 0.1 1000
    (x,y,w,h) <- getWindowViewport gpWindow
    glViewport x y w h

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    processEvents gpEvents (closeOnEscape gpWindow)
    
    mValues <- readArray fftArrayName (0::Int) 256
    let _ = mValues :: Maybe [GLfloat]
    forM_ mValues $ \values -> do
      let ys = (*10) . (/255) <$> values
          newVerts = flip map (zip [0..] ys) $ \(i, y) ->
            let x = fromIntegral i / fromIntegral lineVertCount
                x' = x * 2 - 1
            in V3 x' y 0
      bufferSubData lineBuffer (concatMap toList newVerts)
    

    let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 0 0)
    uniformM44 uMVP (projection !*! view !*! model)
    withVAO lineVAO $ 
      glDrawArrays GL_LINE_STRIP 0 lineVertCount

    swapBuffers gpWindow
