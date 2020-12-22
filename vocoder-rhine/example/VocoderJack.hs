{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Sound.JACK as JACK
import qualified Sound.JACK.Audio as Audio
import qualified Data.Vector.Storable as V
import Data.Array.Storable as A
import Control.Monad.Trans.Class(lift)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Vocoder.Rhine
import FRP.Rhine
import MVarClock

type AudioV = V.Vector Audio.Sample

type EventIO = EventMVarT AudioV IO

processing :: (MonadIO m, Tag cl ~ AudioV) => MVar AudioV -> ClSF m cl () ()
processing omvar = tagS >>> arrMCl (liftIO . fmap (const ()) . tryPutMVar omvar)

main :: IO ()
main = do
    imvar <- newEmptyMVar
    omvar <- newEmptyMVar 
    forkIO $ flow $ processing omvar @@ (mVarClockOn imvar :: HoistClock EventIO IO (MVarClock AudioV))
    JACK.handleExceptions $ 
        JACK.withClientDefault "vocoder-jack" $ \client -> 
        JACK.withPort client "input" $ \iport ->
        JACK.withPort client "output" $ \oport ->
        JACK.withProcess client (lift . process imvar omvar iport oport) $
        JACK.withActivation client $
        lift $ JACK.waitForBreak
    
process :: MVar AudioV -> MVar AudioV -> Audio.Port JACK.Input -> Audio.Port JACK.Output -> JACK.NFrames -> IO ()
process imvar omvar iport oport nframes@(JACK.NFrames frames) = do
    iArr <- Audio.getBufferArray iport nframes
    oArr <- Audio.getBufferArray oport nframes
    iVec <- V.generateM (fromIntegral frames) $ \i -> fmap realToFrac $ A.readArray iArr $ JACK.NFrames $ fromIntegral i
    tryPutMVar imvar iVec
    moVec <- tryTakeMVar omvar
    case moVec of
        Just oVec ->
            forM_ (JACK.nframesIndices nframes) $ \ni@(JACK.NFrames i) ->
                writeArray oArr ni $ realToFrac $ oVec V.! fromIntegral i
        Nothing ->
            forM_ (JACK.nframesIndices nframes) $ \ni ->
                writeArray oArr ni 0

