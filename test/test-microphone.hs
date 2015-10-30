import Sound.Pd1
import Control.Concurrent
import Control.Monad
import Linear.Extra

main :: IO ()
main = do
    p1 <- makePatch "test"

    _ <- forkIO $ forM_ [400,410..1500] $ \freq -> do
        send p1 "freq" $ Atom $ Float (freq)
        threadDelay 100000
    
    _ <- forkIO $ forM_ [-10, -9..] $ \pos -> do
        putStrLn $ "Listener now at " ++ show pos
        alListenerPosition (V3 (pos::Double) 0 0)
        threadDelay 100000
    threadDelay 5000000
