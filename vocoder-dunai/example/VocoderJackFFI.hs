module VocoderJackFFI where

import qualified Sound.JACK as JACK
import qualified Sound.JACK.Audio as Audio
import Sound.JACK.Exception
import qualified Data.Vector.Storable.Mutable as MV
import Control.Monad.Exception.Synchronous
import Control.Monad.Trans.Class(lift)
import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Alloc

mvectorPtr :: MV.MVector s Float -> ForeignPtr Float
mvectorPtr = fst . MV.unsafeToForeignPtr0

foreign import ccall unsafe "&vocoder_process" vocoderProcess :: FunPtr (JACK.Process a)

foreign import ccall "wrapper" createTick :: IO () -> IO (FunPtr (IO ()))

withVocoderProcess :: ThrowsErrno e 
                   => JACK.Client
                   -> [(Audio.Port JACK.Input, MV.IOVector Float)]
                   -> (Audio.Port JACK.Output, MV.IOVector Float)
                   -> IO ()
                   -> ExceptionalT e IO b
                   -> ExceptionalT e IO b
withVocoderProcess client iports (oport, obuf) tickfun cont = do
    param <- lift $ mallocBytes $ sizeOf (portPtr oport) * (2 * (length iports + 1) + 2)
    lift $ do
        pokeElemOff param 0 $ intPtrToPtr $ fromIntegral $ length iports
        tick <- createTick tickfun
        pokeElemOff param 1 $ castFunPtrToPtr tick
        pokeElemOff param 2 (portPtr oport)
        withForeignPtr (mvectorPtr obuf) $ pokeElemOff param 3
        forM_ (zip [4, 6..] iports) $ \(k, (p, b)) -> do
            pokeElemOff param k (portPtr p)
            withForeignPtr (mvectorPtr b) $ pokeElemOff param (k+1)
    JACK.setProcess client vocoderProcess param
    ret <- cont
    lift $ free param
    return ret
    where
    portPtr (JACK.Port ptr) = castPtr ptr


