{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.VR.Pal
import Graphics.GL
import Graphics.GL.Pal
import Sound.Pd
import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Control.Monad.Trans
data Uniforms = Uniforms
  { uMVP :: UniformLocation (M44 GLfloat) }
  deriving Data

makeLine :: Program -> IO (VertexArrayObject, ArrayBuffer (V3 GLfloat), GLsizei)
makeLine shader = do

    let verts = flip map [0..255] $ \i ->
                    let x = (fromIntegral (i::Int) / 255) * 2 - 1
                    in V3 x 0 0
        vertCount = length verts
        normals = replicate vertCount (V3 0 0 1)

    positionsBuffer <- bufferData GL_DYNAMIC_DRAW (verts :: [V3 GLfloat])
    normalsBuffer   <- bufferData GL_STATIC_DRAW (normals :: [V3 GLfloat])

    vao <- newVAO
    withVAO vao $ do
        withArrayBuffer positionsBuffer $ assignFloatAttribute shader "aPosition" GL_FLOAT 3
        withArrayBuffer normalsBuffer   $ assignFloatAttribute shader "aNormal"   GL_FLOAT 3

    return (vao, positionsBuffer, fromIntegral vertCount)

fftToVerts :: V.Vector GLfloat -> V.Vector (V3 GLfloat)
fftToVerts values = newVerts
  where
    ys = (/255) <$> values
    newVerts = flip V.imap ys $ \i y ->
      -- Spread x from -1 to 1
      let x = (fromIntegral (i::Int) / 256) * 2 - 1
      in V3 x y 0

main :: IO ()
main = withPd $ \pd -> do
    vrPal@VRPal{..} <- initVRPal "Pd Mic FFT" []
    addToLibPdSearchPath pd "test"

    patch  <- makePatch pd "test/test-fft"

    let fftArrayName = local patch "mic-fft"

    shader <- createShaderProgram "test/geo.vert" "test/geo.frag"
    useProgram shader
    Uniforms{..} <- acquireUniforms shader
    (lineVAO, lineBuffer, lineVertCount) <- makeLine shader

    glClearColor 0.01 0.01 0.05 1
    let player = Pose (V3 0 0 2) (axisAngle (V3 0 1 0) 0)
    whileWindow gpWindow $ do
        (headM44, events) <- tickVR vrPal (transformationFromPose player)
        forM_ events (\ev -> case ev of GLFWEvent e -> closeOnEscape gpWindow e; _ -> return ())

        -- Get the latest FFT from Pd
        mValues <- readArray pd fftArrayName (0::Int) 256

        forM_ mValues $ \values -> do
            -- Place its values into our line's GL buffer
            let newVerts = fftToVerts (VS.convert values)
            newVertsM <- VS.unsafeThaw (V.convert newVerts)
            bufferSubDataV lineBuffer newVertsM

        -- Draw the line
        renderWith vrPal headM44
            $ \projection eyeView -> do
                (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
                let model = mkTransformation (axisAngle (V3 1 1 0) 0) (V3 0 0 0)
                uniformM44 uMVP (projection !*! eyeView !*! model)
                withVAO lineVAO $
                    glDrawArrays GL_LINE_STRIP 0 lineVertCount

