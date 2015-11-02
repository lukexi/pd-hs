module Sound.Pd1 (
    makePatch,
    makePolyPatch,
    makeWeakPatch,
    withPatch,
    closePatch,
    send,
    sendGlobal,
    subscribe,
    makeReceiveChan,
    Pd.local,
    Pd.Patch,
    Pd.Receiver,
    Pd.Atom(..),
    Pd.Message(..),
    Pd.PolyPatch(..),
    Pd.getPolyVoice,
    Pd.OpenALSource,
    Pd.alSourcePosition,
    Pd.alListenerPosition,
    Pd.alListenerOrientation,
    Pd.alListenerPose,
    Pd.alListenerGain,
    Pd.copyOpenALHRTFs,
    readArray,
    writeArray,
    arraySize,
    addToLibPdSearchPath,
    getPdSources
    ) where
import qualified Sound.Pd as Pd

import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe
import Data.IORef
import Control.Monad.Trans

{-# NOINLINE pdRef #-}
pdRef :: IORef Pd.PureData
pdRef = unsafePerformIO $ newIORef =<< Pd.initLibPd

getPd :: MonadIO m => m Pd.PureData
getPd = liftIO $ readIORef pdRef

getPdSources :: MonadIO m => m [Pd.OpenALSource]
getPdSources = liftIO $ do
    pd <- getPd
    return $ Pd.pdSources pd

addToLibPdSearchPath :: MonadIO m => FilePath -> m ()
addToLibPdSearchPath path = do
    -- We just need to get Pd here to make sure LibPd is initialized before we add to its search path
    _pd <- getPd
    Pd.addToLibPdSearchPath path

makePatch :: MonadIO m => FilePath -> m Pd.Patch
makePatch fileName = do
    pd <- getPd
    Pd.makePatch pd fileName

makePolyPatch :: MonadIO m => Int -> FilePath -> m Pd.PolyPatch
makePolyPatch count fileName = do
    pd <- getPd
    Pd.makePolyPatch pd count fileName

withPatch :: FilePath -> (Pd.Patch -> IO a) -> IO a
withPatch fileName action = do
    pd <- getPd
    Pd.withPatch pd fileName action

makeWeakPatch :: MonadIO m => FilePath -> m Pd.Patch
makeWeakPatch fileName = do
    pd <- getPd
    Pd.makeWeakPatch pd fileName

closePatch :: MonadIO m => Pd.Patch -> m ()
closePatch patch = do
    pd <- getPd
    Pd.closePatch pd patch

send :: MonadIO m => Pd.Patch -> Pd.Receiver -> Pd.Message -> m ()
send patch receiver msg = do
    pd <- getPd
    Pd.send pd patch receiver msg

makeReceiveChan :: MonadIO m => Pd.Receiver -> m (TChan Pd.Message)
makeReceiveChan name = do
    pd <- getPd
    Pd.makeReceiveChan pd name

-- | Sends a message to the given global receiver name
sendGlobal :: MonadIO m => Pd.Receiver -> Pd.Message -> m ()
sendGlobal r msg = do
    pd <- getPd
    Pd.sendGlobal pd r msg
 
subscribe :: MonadIO m => Pd.Receiver -> (Pd.Message -> IO a) -> m ThreadId
subscribe name handler = do
    pd <- getPd
    Pd.subscribe pd name handler

arraySize :: Num a => String -> IO a
arraySize arrayName = do
    pd <- getPd
    Pd.arraySize pd arrayName

readArray :: (Integral a, Fractional b) => String -> a -> a -> IO (Maybe [b])
readArray arrayName offset count = do
    pd <- getPd
    Pd.readArray pd arrayName offset count

writeArray :: (Real a, Integral b, MonadIO m) => String -> [a] -> b -> m ()
writeArray arrayName values offset = do
    pd <- getPd
    Pd.writeArray pd arrayName values offset
