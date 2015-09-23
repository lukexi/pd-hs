import Sound.Pd1
import Control.Concurrent
import Control.Monad
import Linear.Extra

main :: IO ()
main = do
    p1 <- makePatch "world"

    sources <- getPdSources
    forM_ sources $ \sourceID -> alSourcePosition sourceID ((V3 0 0 0) :: V3 Double)

    --_ <- forkIO $ forM_ [200,210..1500] $ \freq -> do
    --    send p1 "freq" $ Atom $ Float (freq)
    --    threadDelay 100000
    _ <- forkIO $ forM_ (map ((*2) . sin) [0,0.01..]) $ \pos -> do
        --print $ "Listener now at " ++ show pos
        --alListenerPosition ((V3 0 0 (pos*5)) :: V3 Double)
        alListenerPosition (V3 0 0 (pos*5) :: V3 Double)
        alListenerOrientation ((axisAngle (V3 0 1 0) 0) :: Quaternion Double)

        threadDelay 10000
    threadDelay 50000000
