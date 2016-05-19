{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.Pd.OpenAL where
import Linear.Extra
import Control.Monad.Trans
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import System.Environment
import System.Directory
import System.FilePath
import Foreign.Marshal hiding (void)
import Control.Monad
import Control.Exception

newtype OpenALSource = OpenALSource CUInt deriving (Show, Storable, Eq, Ord)

foreign import ccall "setOpenALSourcePositionRaw"
    setOpenALSourcePositionRaw :: OpenALSource -> Ptr CFloat -> IO ()

foreign import ccall "setOpenALListenerOrientationRaw"
    setOpenALListenerOrientationRaw :: Ptr CFloat -> IO ()

foreign import ccall "setOpenALListenerPositionRaw"
    setOpenALListenerPositionRaw :: Ptr CFloat -> IO ()

foreign import ccall "setOpenALListenerGainRaw"
    setOpenALListenerGainRaw :: CFloat -> IO ()

foreign import ccall "setOpenALDistanceModelInverse"
    setOpenALDistanceModelInverse :: IO ()

foreign import ccall "setOpenALDistanceModelLinear"
    setOpenALDistanceModelLinear :: IO ()

foreign import ccall "setOpenALDistanceModelExponent"
    setOpenALDistanceModelExponent :: IO ()

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
            -- putStrLn $ "Copying " ++ (openALDir </> file) ++ " to " ++ (appDataHRTFDir </> file)
            copyFile (openALDir </> file) (appDataHRTFDir </> file)
