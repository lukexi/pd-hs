{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, LambdaCase, TupleSections #-}
module Sound.Pd.Internal where
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Control.Concurrent
import Control.Monad

data FileOpaque
type File = Ptr FileOpaque

data BindingOpaque
type Binding = Ptr BindingOpaque

data AtomOpaque
type AtomPtr = Ptr AtomOpaque

data AudioOpaque
type AudioPtr = Ptr AudioOpaque
type PdChan = Chan (IO ())

pdRun :: PdChan -> IO a -> IO a
pdRun chan action = do
    lock <- newEmptyMVar
    writeChan chan (putMVar lock =<< action)
    takeMVar lock

foreign import ccall "startAudio" startAudio :: StablePtr PdChan -> IO AudioPtr

foreign import ccall "&finishAudio" finishAudio :: FunPtr (AudioPtr -> IO ())

foreign import ccall "libpd_process_float"  libpd_process_float  :: CInt -> Ptr CFloat -> Ptr CFloat -> IO CInt
foreign import ccall "libpd_process_double" libpd_process_double :: CInt -> Ptr CDouble -> Ptr CDouble -> IO CInt

foreign export ccall processFloat :: StablePtr PdChan -> CInt -> Ptr CDouble -> Ptr CDouble -> IO ()
processFloat :: StablePtr PdChan -> CInt -> Ptr CDouble -> Ptr CDouble -> IO ()
processFloat stablePdChan ticks inBuffer outBuffer = do
    chan <- deRefStablePtr stablePdChan
    pdRun chan $ void $ libpd_process_double ticks inBuffer outBuffer

foreign import ccall "libpd_init" libpd_init :: IO ()

foreign import ccall "libpd_openfile" libpd_openfile :: CString -> CString -> IO File

foreign import ccall "libpd_closefile" libpd_closefile :: File -> IO ()

foreign import ccall "libpd_getdollarzero" libpd_getdollarzero :: File -> IO CInt

foreign import ccall "libpd_bang" libpd_bang :: CString -> IO CInt

foreign import ccall "libpd_float" libpd_float :: CString -> CFloat -> IO CInt
foreign import ccall "libpd_symbol" libpd_symbol :: CString -> CString -> IO CInt
foreign import ccall "libpd_start_message" libpd_start_message :: CInt -> IO CInt
foreign import ccall "libpd_finish_message" libpd_finish_message :: CString -> CString -> IO CInt
foreign import ccall "libpd_finish_list" libpd_finish_list :: CString -> IO CInt
foreign import ccall "libpd_add_float" libpd_add_float :: CFloat -> IO ()
foreign import ccall "libpd_add_symbol" libpd_add_symbol :: CString -> IO ()
type CPrintHook = CString -> IO ()
foreign import ccall "wrapper"
   mkPrintHook :: CPrintHook -> IO (FunPtr CPrintHook)
foreign import ccall "libpd_set_printhook" libpd_set_printhook :: FunPtr CPrintHook -> IO ()
type CBangHook = CString -> IO ()
foreign import ccall "wrapper"
   mkBangHook :: CBangHook -> IO (FunPtr CBangHook)
foreign import ccall "libpd_set_banghook" libpd_set_banghook :: FunPtr CBangHook -> IO ()
type CFloatHook = CString -> CFloat -> IO ()
foreign import ccall "wrapper"
   mkFloatHook :: CFloatHook -> IO (FunPtr CFloatHook)

foreign import ccall "libpd_set_floathook" libpd_set_floathook :: FunPtr CFloatHook -> IO ()
type CSymbolHook = CString -> CString -> IO ()
foreign import ccall "wrapper"
   mkSymbolHook :: CSymbolHook -> IO (FunPtr CSymbolHook)

foreign import ccall "libpd_set_symbolhook" libpd_set_symbolhook :: FunPtr CSymbolHook -> IO ()
type CListHook = CString -> CInt -> AtomPtr -> IO ()
foreign import ccall "wrapper"
   mkListHook :: CListHook -> IO (FunPtr CListHook)

foreign import ccall "libpd_set_listhook" libpd_set_listhook :: FunPtr CListHook -> IO ()
type CMessageHook = CString -> CString -> CInt -> AtomPtr -> IO ()
foreign import ccall "wrapper"
   mkMessageHook :: CMessageHook -> IO (FunPtr CMessageHook)

foreign import ccall "libpd_set_messagehook" libpd_set_messagehook :: FunPtr CMessageHook -> IO ()

foreign import ccall "libpd_get_symbol" libpd_get_symbol :: AtomPtr -> IO CString

foreign import ccall "libpd_get_float" libpd_get_float :: AtomPtr -> IO CFloat
foreign import ccall "libpd_is_symbol" libpd_is_symbol :: AtomPtr -> IO CInt
foreign import ccall "libpd_is_float" libpd_is_float :: AtomPtr -> IO CInt

foreign import ccall "libpd_next_atom" libpd_next_atom :: AtomPtr -> AtomPtr

foreign import ccall "libpd_exists" libpd_exists :: CString -> IO CInt

foreign import ccall "libpd_bind" libpd_bind :: CString -> IO Binding
foreign import ccall "libpd_unbind" libpd_unbind :: Binding -> IO ()