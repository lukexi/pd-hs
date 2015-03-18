import Sound.Pd1
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    p1 <- makePatch "test"
    p2 <- makePatch "test"
    p3 <- makePatch "test"

    subscribe (local p1 "output") $ \i -> putStrLn $ "Subscription A: " ++ show i
    subscribe (local p1 "output") $ \i -> putStrLn $ "Subscription B: " ++ show i
    subscribe (local p2 "output") $ \i -> putStrLn $ "Subscription C: " ++ show i
    subscribe (local p2 "output") $ \i -> putStrLn $ "Subscription D: " ++ show i

    forkIO $ forM_ [400,410..1500] $ \freq -> do
        send p1 "freq" $ Atom $ Float (freq)
        threadDelay 100000
    forkIO $ forM_ (reverse [400,410..1500]) $ \freq -> do
        send p2 "freq" $ Atom $ Float (freq * 4)
        threadDelay 110000
    forkIO $ forM_ [400,410..1500] $ \freq -> do
        send p3 "freq" $ Atom $ Float (freq * 7)
        threadDelay 130000
    threadDelay 5000000
    
        
