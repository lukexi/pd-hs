{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, LambdaCase, TupleSections, DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
module Sound.Pd.Core where
import Sound.Pd.Internal
import Foreign.C
import Foreign.Marshal.Array
import System.FilePath
import System.Directory
import System.Environment
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.StablePtr
import Data.String
import Data.Data
import Control.Exception
import Linear.Extra
import System.Mem.Weak

data Atom = String String | Float Float deriving Show

-- With OverloadedStrings, allows writing e.g. List ["freq", 440, "ratio", 1.6] 
instance IsString Atom where
    fromString = String

instance IsString Message where
    fromString = Atom . String

instance Num Atom where
    (Float a) + (Float b) = Float (a + b)
    _ + _ = error "Can't + Float by String or vice versa"
    (Float a) - (Float b) = Float (a - b)
    _ - _ = error "Can't - Float by String or vice versa"
    (Float a) * (Float b) = Float (a * b)
    _ * _ = error "Can't * Float by String or vice versa"
    abs (Float a) = Float (abs a)
    abs _ = error "Can't abs String"
    signum (Float a) = Float (signum a)
    signum _ = error "Can't signum String"
    fromInteger = Float . fromInteger

instance Fractional Atom where
    (Float a) / (Float b) = Float (a / b)
    _ / _ = error "Can't divide Float by String or vice versa"
    fromRational = Float . fromRational

data Message = Bang | Atom Atom | List [Atom] | Message String [Atom] deriving Show

type Receiver = String

-- | Creates a localized version of a receiver name by prepending the patch instance's '$0' ID
local :: Patch -> Receiver -> Receiver
local (Patch _ (DollarZero dz)) receiver = show dz ++ receiver

-- | Sends a message to the patch-local ($0-prepended) version of the given receiver name
send :: MonadIO m => PureData -> Patch -> Receiver -> Message -> m ()
send pd patch receiver msg = sendGlobal pd (local patch receiver) msg

-- | Sends a message to the given global receiver name
sendGlobal :: MonadIO m => PureData -> Receiver -> Message -> m ()
sendGlobal pd r msg = liftIO $ pdRun (pdThreadChan pd) $ void $ sendGlobal' msg
    where
    sendGlobal' Bang               = bang    r
    sendGlobal' (Atom (String s))  = symbol  r s
    sendGlobal' (Atom (Float f))   = float   r f
    sendGlobal' (Message sel args) = message r sel args
    sendGlobal' (List args)        = list    r args

data PureData = PureData 
    { pdChannels   :: TVar (Map Receiver (TChan Message))
    , pdThreadChan :: PdChan
    , pdSources    :: [OpenALSource]
    }

-- | Returns the request receiver-channel for subscription, creating it if necessary (returning True if so, False otherwise)
acquireChannel :: Receiver -> TVar (Map Receiver (TChan Message)) -> STM (TChan Message, Bool)
acquireChannel name channelsVar = do
    channels <- readTVar channelsVar
    case Map.lookup name channels of
        Just chan -> return (chan, False)
        Nothing -> do
            chan <- newBroadcastTChan
            writeTVar channelsVar (Map.insert name chan channels)
            return (chan, True)

-- | Create a libpd instance. Must be called before anything else.
-- Libpd currently only supports one instance (but work is underway to fix this)
initLibPd :: MonadIO m => m PureData
initLibPd = liftIO $ do
    -- Run all commands on the same thread, as libpd is not thread safe.
    runChan <- newChan
    _ <- forkOS . forever $ join (readChan runChan)

    channelsVar <- newTVarIO Map.empty

    printBuffer <- newMVar ""
    setPrintHook $ \case
        "\n" -> putStrLn =<< modifyMVar  printBuffer (return . ("",))
        word ->              modifyMVar_ printBuffer (return . (++ word))

    -- Set libpd's messages-from-pd hooks to write to a unified channel
    receiveChan <- newTChanIO
    let write = atomically . writeTChan receiveChan
    setBangHook    (\r     -> write (r, Bang))
    setSymbolHook  (\r s   -> write (r, Atom (String s)))
    setFloatHook   (\r f   -> write (r, Atom (Float f)))
    setListHook    (\r l   -> write (r, List l))
    setMessageHook (\r m l -> write (r, Message m l))
    _ <- forkIO . forever . atomically $ do
        (rec, msg)   <- readTChan receiveChan
        (channel, _) <- acquireChannel rec channelsVar
        writeTChan channel msg

    libpd_init 
    
    let numberOfOpenALSources = 16
    sources <- peekArray (fromIntegral numberOfOpenALSources) 
        =<< startAudio numberOfOpenALSources 128 
        =<< newStablePtr runChan
    -- putStrLn $ "Initialized OpenAL with " ++ show numberOfOpenALSources ++ " sources: " ++ show sources

    let pd = PureData 
            { pdChannels   = channelsVar
            , pdThreadChan = runChan
            , pdSources    = sources
            }

    return pd

makeReceiveChan :: MonadIO m => PureData -> Receiver -> m (TChan Message)
makeReceiveChan (PureData {pdChannels=channelsVar}) name = liftIO $ do
    (channel, isNew) <- atomically $ do
        (bChan, isNew) <- acquireChannel name channelsVar
        chan <- dupTChan bChan
        return (chan, isNew)
    when isNew $ void $ bind name
    return channel

-- | Subscribe to a given receiver name's output.
subscribe :: MonadIO m => PureData -> Receiver -> (Message -> IO a) -> m ThreadId
subscribe pd name handler = liftIO $ do
    channel <- makeReceiveChan pd name
    forkIO . forever $ 
        handler =<< atomically (readTChan channel)


--------
-- Files
--------

data Patch = Patch { phFile :: File, phDollarZero :: DollarZero } deriving Typeable

newtype DollarZero = DollarZero Int

-- | Spawn a new instance of the given patch name (sans .pd extension)
makePatch :: MonadIO m => PureData -> FilePath -> m Patch
makePatch pd fileName = liftIO $ pdRun (pdThreadChan pd) $ do
    file <- openFile (fileName <.> "pd") "."
    dz   <- getDollarZero file
    return $ Patch file dz

-- | This should work, but doesn't seem to work perfectly with Halive yet
makeWeakPatch :: MonadIO m => PureData -> FilePath -> m Patch
makeWeakPatch pd fileName = liftIO $ do
    patch@(Patch file _) <- makePatch pd fileName
    addFinalizer patch $ do
        putStrLn $ "Closing weak patch: " ++ fileName
        closeFile file
    return patch

closePatch :: MonadIO m => PureData -> Patch -> m ()
closePatch pd (Patch file _) = liftIO $ pdRun (pdThreadChan pd) $ closeFile file


withPatch :: PureData -> FilePath -> (Patch -> IO a) -> IO a
withPatch pd name = bracket (makePatch pd name) (closePatch pd)


-- | Add a directory to the searchpath to find Pd patches in
addToLibPdSearchPath :: MonadIO m => String -> m ()
addToLibPdSearchPath dir = liftIO $ 
    withCString dir $ \d -> 
    libpd_add_to_search_path d

-- | Open the given filename in the given dir
openFile :: String -> String -> IO File
openFile name dir = 
    withCString name $ \n -> 
    withCString dir $ \d -> 
    libpd_openfile n d


closeFile :: File -> IO ()
closeFile = libpd_closefile

getDollarZero :: File -> IO DollarZero
getDollarZero file = DollarZero . fromIntegral <$> libpd_getdollarzero file

-------------------
-- Sending messages
-------------------

-- | Send a bang to the given receiver name
bang :: MonadIO m => Receiver -> m Int
bang receiver = liftIO $ fromIntegral <$> 
    withCString receiver libpd_bang

-- | Send a float to the given receiver name
float :: MonadIO m => Receiver -> Float -> m Int
float receiver f = liftIO $ fromIntegral <$> 
    withCString receiver (\r -> libpd_float r (realToFrac f))

-- | Send a symbol to the given receiver name
symbol :: MonadIO m => Receiver -> String -> m Int
symbol receiver sym = liftIO $ fromIntegral <$> 
    withCString receiver (\r -> 
    withCString sym      (\s -> 
    libpd_symbol r s))

-- | Send a message (a selector with arguments) to the given receiver name
message :: MonadIO m => Receiver -> String -> [Atom] -> m Int
message receiver sel args = liftIO $ do
    _ <- startMessage $ length args
    forM_ args $ \case
        String s -> addSymbol s
        Float  f -> addFloat f
    finishMessage receiver sel

-- | Send a list to the given receiver name
list :: MonadIO m => Receiver -> [Atom] -> m Int
list receiver args = liftIO $ do
    _ <- startMessage $ length args
    forM_ args $ \case
        String s -> addSymbol s
        Float  f -> addFloat f
    finishList receiver

----------------
-- List building
----------------
startMessage :: Int -> IO Int
startMessage = fmap fromIntegral . libpd_start_message . fromIntegral


finishMessage :: String -> String -> IO Int
finishMessage receiver sel = fmap fromIntegral $
    withCString receiver $ \r ->
        withCString sel $ \s ->
            libpd_finish_message r s

finishList :: String -> IO Int
finishList receiver = fmap fromIntegral $
    withCString receiver libpd_finish_list

addFloat :: Float -> IO ()
addFloat = libpd_add_float . realToFrac


addSymbol :: String -> IO ()
addSymbol sym = withCString sym libpd_add_symbol

--------
-- Hooks
--------
setPrintHook :: (String -> IO ()) -> IO ()
setPrintHook printHook = 
    libpd_set_printhook =<< mkPrintHook (peekCString >=> printHook)


setBangHook :: (String -> IO ()) -> IO ()
setBangHook bangHook = 
    libpd_set_banghook =<< mkBangHook (peekCString >=> bangHook)

setFloatHook :: (String -> Float -> IO ()) -> IO ()
setFloatHook floatHook = 
    libpd_set_floathook =<< mkFloatHook (\crec cf -> do
        rec <- peekCString crec
        let f = realToFrac cf
        floatHook rec f)


setSymbolHook :: (String -> String -> IO ()) -> IO ()
setSymbolHook symbolHook = 
    libpd_set_symbolhook =<< mkSymbolHook (\crec csym -> do
        rec <- peekCString crec
        sym <- peekCString csym
        symbolHook rec sym)

setListHook :: (Receiver -> [Atom] -> IO ()) -> IO ()
setListHook listHook = 
    libpd_set_listhook =<< mkListHook (\crec count cargs -> do
        rec <- peekCString crec
        args <- convertList count cargs
        listHook rec args)

setMessageHook :: (Receiver -> String -> [Atom] -> IO ()) -> IO ()
setMessageHook messageHook = 
    libpd_set_messagehook =<< mkMessageHook (\crec cmsg count cargs -> do
        rec  <- peekCString crec
        msg  <- peekCString cmsg
        args <- convertList count cargs
        messageHook rec msg args)


------------------
-- List conversion
------------------
convertList :: CInt -> AtomPtr -> IO [Atom]
convertList count cargs = do
    let atomPtrs = iterate libpd_next_atom cargs
    forM [0..(fromIntegral count - 1)] $ \i -> do
        let atom = atomPtrs !! i
        isF <- isFloat atom
        if isF
            then getFloat atom
            else do
                isS <- isSymbol atom
                if isS 
                    then getSymbol atom
                    else error "Unsupported atom type"

getSymbol :: AtomPtr -> IO Atom
getSymbol a = String <$> (peekCString =<< libpd_get_symbol a)

getFloat :: AtomPtr -> IO Atom
getFloat a = Float . realToFrac <$> libpd_get_float a

isSymbol :: AtomPtr -> IO Bool
isSymbol a = (> 0) <$> libpd_is_symbol a

isFloat :: AtomPtr -> IO Bool
isFloat a = (> 0) <$> libpd_is_float a

----------
-- Binding
----------
symbolExists :: Receiver -> IO Bool
symbolExists receiver = (> 0) <$> withCString receiver libpd_exists


bind :: Receiver -> IO Binding
bind receiver = withCString receiver libpd_bind


unbind :: Binding -> IO ()
unbind = libpd_unbind

------------
-- Polyphony
------------

newtype PolyPatch = PolyPatch {unPolyPatch::MVar [Patch]}

makePolyPatch :: MonadIO m => PureData -> Int -> FilePath -> m PolyPatch
makePolyPatch pd count patchName = liftIO $ PolyPatch <$>
  (newMVar =<< replicateM count (makePatch pd patchName))

-- | Round-robin voice allocation
getPolyVoice :: MonadIO m => PolyPatch -> m Patch
getPolyVoice (PolyPatch voices) = liftIO $ modifyMVar voices $ \(v:vs) -> return (vs++[v], v)

---------
-- OpenAL
---------
quaternionToUpAt :: (RealFloat a, Conjugate a) => Quaternion a -> (V3 a, V3 a)
quaternionToUpAt quat = ( rotate quat (V3 0 (-1) 0) -- I expected to want 0 1 0, but this gives us correct results
                        , rotate quat (V3 0 0 (-1))
                        )

quaternionToUpAtList :: (RealFloat a, Conjugate a) => Quaternion a -> [a]
quaternionToUpAtList quat = [uX, uY, uZ, aX, aY, aZ] 
  where (V3 uX uY uZ, V3 aX aY aZ) = quaternionToUpAt quat

alSourcePosition :: (MonadIO m, RealFloat a) => OpenALSource -> V3 a -> m ()
alSourcePosition   sourceID (fmap realToFrac -> V3 x y z) = liftIO $ withArray [x,y,z] 
  (setOpenALSourcePositionRaw sourceID)

alListenerPose :: (MonadIO m, RealFloat a) => Pose a -> m ()
alListenerPose (Pose position orientation) = do
    alListenerPosition position
    alListenerOrientation orientation

alListenerPosition :: (MonadIO m, RealFloat a) => V3 a -> m ()
alListenerPosition          (fmap realToFrac -> V3 x y z) = liftIO $ withArray [x,y,z] 
  setOpenALListenerPositionRaw

alListenerOrientation :: (MonadIO m, RealFloat a) => Quaternion a -> m ()
alListenerOrientation quat = liftIO $ withArray (quaternionToUpAtList (realToFrac <$> quat))
  setOpenALListenerOrientationRaw

alListenerGain :: (MonadIO m, RealFloat a) => a -> m ()
alListenerGain gain = liftIO $ setOpenALListenerGainRaw (realToFrac gain)

copyOpenALHRTFs :: IO (Either IOException ())
copyOpenALHRTFs = liftIO . try $ do
  mAppDataDir <- lookup "APPDATA" <$> getEnvironment

  forM_ mAppDataDir $ \appDataDir -> do
    let openALDir      = "openal"   </> "hrtf"
        appDataHRTFDir = appDataDir </> openALDir

    createDirectoryIfMissing True appDataHRTFDir

    contents <- filter (not . (`elem` [".", ".."])) <$> getDirectoryContents openALDir
    forM_ contents $ \file -> do
      putStrLn $ "Copying " ++ (openALDir </> file) ++ " to " ++ (appDataHRTFDir </> file)
      copyFile (openALDir </> file) (appDataHRTFDir </> file)
