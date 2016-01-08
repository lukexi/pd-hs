{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Sound.Pd

-- MAIN REFERENCE FOR METAPATCHING:
-- https://puredata.info/docs/tutorials/TipsAndTricks#undocumented-pd-internal-messages

main :: IO ()
main = withPd $ \pd -> do

    _patch <- makePatch pd "test/test-meta-empty"
    -- This is how we talk to a subpatch, in this case [pd $0contents]
    -- (assuming [pd $0contents] already exists in the patch!)
    -- let receiver = "pd-" ++ local patch "contents"


    -- This is how we talk to the patch file itself. If it's open multiple times, the edits
    -- will take place in all open copies!
    -- NOTE: That's fixable using namecanvas if desired, see link above.
    let receiver = "pd-test/test-meta-empty.pd"
    
    putStrLn "Adding osc~"

    sendGlobal pd receiver (Message "obj" [50, 50, "osc~", 330])
    putStrLn "Adding dac~"
    sendGlobal pd receiver (Message "obj" [50, 50, "dac~", 1, 2])
    putStrLn "Connecting"
    sendGlobal pd receiver (Message "connect" [0, 0, 1, 0])

    threadDelay 1000000
    putStrLn "Adding second osc~"
    sendGlobal pd receiver (Message "obj" [50, 50, "osc~", 550])
    sendGlobal pd receiver (Message "connect" [2, 0, 1, 1])

    threadDelay 5000000
