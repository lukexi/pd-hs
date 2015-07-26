import Sound.Pd1
import Control.Concurrent
import Control.Monad
import Linear

main :: IO ()
main = do
    p1 <- makePatch "world"

    sources <- getPdSources
    forM_ sources $ \sourceID -> alSourcePosition sourceID (V3 0 0 (-5))

    _ <- forkIO $ forM_ [200,210..1500] $ \freq -> do
        send p1 "freq" $ Atom $ Float (freq)
        threadDelay 100000
    _ <- forkIO $ forM_ (map ((*2) . sin) [0,0.05..]) $ \pos -> do
        --print $ "Listener now at " ++ show pos
        alListenerPosition (V3 (-1) 0 5)
        alListenerOrientation (axisAngle (V3 0 1 0) 0)

        threadDelay 10000
    threadDelay 50000000
