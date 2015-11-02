import Sound.Pd1
import Control.Concurrent
import Control.Monad
import Linear.Extra

main :: IO ()
main = do
    p1 <- makePatch "test/test"
    p2 <- makePatch "test/test"
    p3 <- makePatch "test/test"

    _ <- subscribe (local p1 "output") $ \i -> putStrLn $ "Subscription A: " ++ show i
    _ <- subscribe (local p1 "output") $ \i -> putStrLn $ "Subscription B: " ++ show i
    _ <- subscribe (local p2 "output") $ \i -> putStrLn $ "Subscription C: " ++ show i
    _ <- subscribe (local p2 "output") $ \i -> putStrLn $ "Subscription D: " ++ show i

    _ <- forkIO $ forM_ [400,410..1500] $ \freq -> do
        send p1 "freq" $ Atom $ Float (freq)
        threadDelay 100000
    _ <- forkIO $ forM_ (reverse [400,410..1500]) $ \freq -> do
        send p2 "freq" $ Atom $ Float (freq * 4)
        threadDelay 110000
    _ <- forkIO $ forM_ [400,410..1500] $ \freq -> do
        send p3 "freq" $ Atom $ Float (freq * 7)
        threadDelay 130000
    _ <- forkIO $ forM_ [-5, -0.9..] $ \pos -> do
        putStrLn $ "Listener now at " ++ show pos
        alListenerPosition (V3 pos 0 0)
        threadDelay 100000
    threadDelay 5000000
    
        
