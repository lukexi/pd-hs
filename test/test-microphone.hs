import Sound.Pd
import Control.Concurrent
import Control.Monad
import Linear.Extra
import Data.Time

main :: IO ()
main = withPd $ \pd -> do

    p1 <- makePatch pd "test/test-microphone"

    -- _ <- forkIO $ forM_ [400,410..1500] $ \freq -> do
    --     send p1 "freq" $ Atom $ Float (freq)
    --     threadDelay 100000
    
    _ <- forkIO . forever $ do
        loc <- sin . (*2) . realToFrac . utctDayTime <$> getCurrentTime
        alListenerPosition (V3 (loc::Double) 0 0)
        threadDelay 10000
    forever $ threadDelay 20000000
