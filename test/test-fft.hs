{-# LANGUAGE RecordWildCards #-}
import Graphics.VR.Pal
import Graphics.GL
import Graphics.GL.Pal
import Graphics.UI.GLFW.Pal
import Sound.Pd1
-- import System.Random
import Control.Monad

main :: IO ()
main = do
  VRPal{..} <- initVRPal "Pd Mic FFT" NoGCPerFrame []
  addToLibPdSearchPath "test"
  patch <- makePatch "test/test-fft"
  
  let fftArrayName = local patch "mic-fft"

  glClearColor 0.1 0.1 0.1 1
  whileWindow gpWindow $ do
    processEvents gpEvents (closeOnEscape gpWindow)
    
    mValues <- readArray fftArrayName 0 256
    forM_ mValues $ \values -> do
      let r = (values !! 25) / 255 * 10
      glClearColor r 0 0 1
    
    glClear GL_COLOR_BUFFER_BIT

    swapBuffers gpWindow
