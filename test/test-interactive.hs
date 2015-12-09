{-# LANGUAGE RecordWildCards #-}
import Graphics.VR.Pal
import Graphics.GL
import Graphics.UI.GLFW.Pal
import Sound.Pd1
import System.Random
import Control.Monad

keyToNote :: [(Key, Atom)]
keyToNote = 
  [ (Key'A, 60)
  , (Key'W, 61)
  , (Key'S, 62)
  , (Key'E, 63)
  , (Key'D, 64)
  , (Key'F, 65)
  , (Key'T, 66)
  , (Key'G, 67)
  , (Key'Y, 68)
  , (Key'H, 69)
  , (Key'U, 70)
  , (Key'J, 71)
  , (Key'K, 72)
  , (Key'O, 73)
  , (Key'L, 74)
  , (Key'P, 75)
  ]

main :: IO ()
main = do
  VRPal{..} <- initVRPal "Pd Interactive" NoGCPerFrame []

  patch <- makePatch "test/test-interactive"
  
  glClearColor 0.1 0.1 0.1 1
  whileVR vrPal $ \headM44 hands -> do
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      forM_ keyToNote $ \(key, note) ->
        onKeyDown e key $ do
          [r,g,b] <- replicateM 3 randomIO
          glClearColor r g b 1
          send patch "note" (Atom note)
    
    glClear GL_COLOR_BUFFER_BIT

    swapBuffers gpWindow
