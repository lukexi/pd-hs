{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Graphics.VR.Pal
import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Sound.Pd
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
  
  positionsBuffer <- bufferData GL_DYNAMIC_DRAW (concatMap toList verts :: [GLfloat])
  normalsBuffer   <- bufferData GL_STATIC_DRAW (concatMap toList normals :: [GLfloat])

  vao <- newVAO
  withVAO vao $ do
    withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT  3
    withArrayBuffer normalsBuffer   $ assignFloatAttribute shader "aNormal" GL_FLOAT 3

  return (vao, positionsBuffer, fromIntegral vertCount)

fftToVerts :: [GLfloat] -> [V3 GLfloat]
fftToVerts values = newVerts
  where
      ys = (/255) <$> values
      newVerts = flip map (zip [0..] ys) $ \(i, y) ->
        -- Spread x from -1 to 1
        let x = (fromIntegral (i::Int) / 256) * 2 - 1
        in V3 x y 0

main :: IO ()
main = withPd $ \pd -> do
  vrPal@VRPal{..} <- initVRPal "Pd Mic FFT" NoGCPerFrame []
  addToLibPdSearchPath pd "test"

  patch  <- makePatch pd "test/test-fft"
  
  let fftArrayName = local patch "mic-fft"

  shader <- createShaderProgram "test/geo.vert" "test/geo.frag"
  useProgram shader
  Uniforms{..} <- acquireUniforms shader
  (lineVAO, lineBuffer, lineVertCount) <- makeLine shader

  glClearColor 0.01 0.01 0.05 1
  let view = viewMatrix (V3 0 0 2) (axisAngle (V3 0 1 0) 0)
  whileWindow gpWindow $ do
    processEvents gpEvents (closeOnEscape gpWindow)

    -- Get the latest FFT from Pd
    mValues <- readArray pd fftArrayName (0::Int) 256

    -- Place its values into our line's GL buffer
    forM_ mValues $ \values -> do
      let newVerts = fftToVerts values
      bufferSubData lineBuffer (concatMap toList newVerts)
    
    -- Draw the line
    renderWith vrPal view
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)) 
      $ \projection eyeView -> do
          let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 0 0)
          uniformM44 uMVP (projection !*! eyeView !*! model)
          withVAO lineVAO $ 
            glDrawArrays GL_LINE_STRIP 0 lineVertCount
