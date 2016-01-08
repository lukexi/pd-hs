import Control.Concurrent
import Sound.Pd

main :: IO ()
main = withPd $ \pd -> do
    patch <- makePatch pd "test/test-meta-empty"
    let contentsReceiver = "pd-" ++ local patch "contents"
    
    putStrLn "Adding osc~"

    sendGlobal pd contentsReceiver (Message "obj" [Float 50, Float 50, String "osc~", Float 330])
    putStrLn "Adding dac~"
    sendGlobal pd contentsReceiver (Message "obj" [Float 50, Float 50, String "dac~", Float 1, Float 2])
    putStrLn "Connecting"
    sendGlobal pd contentsReceiver (Message "connect" [Float 0, Float 0, Float 1, Float 0])

    threadDelay 1000000
    putStrLn "Adding second osc~"
    sendGlobal pd contentsReceiver (Message "obj" [Float 50, Float 50, String "osc~", Float 550])
    sendGlobal pd contentsReceiver (Message "connect" [Float 2, Float 0, Float 1, Float 1])

    threadDelay 5000000
