{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Graphics.VR.Pal
import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Sound.Pd1
import Data.Foldable

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

fftToVerts :: Fractional b => [b] -> [V3 b]
fftToVerts values = newVerts
  where
      ys = (/255) <$> values
      newVerts = flip map (zip [0..] ys) $ \(i, y) ->
        -- Spread x from -1 to 1
        let x = (fromIntegral (i::Int) / 256) * 2 - 1
        in V3 x y 0


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

    let view = viewMatrix (V3 0 0 2) (axisAngle (V3 0 1 0) 0)
    projection <- getWindowProjection gpWindow 45 0.1 1000
    (x,y,w,h)  <- getWindowViewport gpWindow
    glViewport x y w h

    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    processEvents gpEvents (closeOnEscape gpWindow)
    
    -- Get the latest FFT from Pd
    mValues <- readArray fftArrayName (0::Int) 256
    -- Place its values into our line's GL buffer
    forM_ mValues $ \values -> do
      let newVerts = fftToVerts values
      bufferSubData lineBuffer (concatMap toList newVerts)
    
    -- Draw the line
    let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 0 0)
    uniformM44 uMVP (projection !*! view !*! model)
    withVAO lineVAO $ 
      glDrawArrays GL_LINE_STRIP 0 lineVertCount

    swapBuffers gpWindow
