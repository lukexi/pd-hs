import Sound.Pd
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
    putStrLn "Starting pd..."
    pd <- initLibPd

    putStrLn "Making patches..."
    p1 <- makePatch pd "test"
    p2 <- makePatch pd "test"
    p3 <- makePatch pd "test"

    subscribe pd (local p1 "output") $ \i -> putStrLn $ "Subscription A: " ++ show i
    subscribe pd (local p1 "output") $ \i -> putStrLn $ "Subscription B: " ++ show i
    subscribe pd (local p2 "output") $ \i -> putStrLn $ "Subscription C: " ++ show i
    subscribe pd (local p2 "output") $ \i -> putStrLn $ "Subscription D: " ++ show i

    forkIO $ forM_ [400,410..1500] $ \freq -> do
        send pd p1 "freq" $ Atom $ Float (freq)
        threadDelay 100000
    forkIO $ forM_ (reverse [400,410..1500]) $ \freq -> do
        send pd p2 "freq" $ Atom $ Float (freq * 4)
        threadDelay 110000
    forkIO $ forM_ [400,410..1500] $ \freq -> do
        send pd p3 "freq" $ Atom $ Float (freq * 7)
        threadDelay 130000
    threadDelay 5000000
    
        
