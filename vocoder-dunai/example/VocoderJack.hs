{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Sound.JACK as JACK
import qualified Sound.JACK.Audio as Audio
import qualified Data.Vector.Storable as V
import Data.Array.Storable as A
import Data.List.Split
import Text.Read hiding (lift)
import Control.Monad.Trans.Class(lift)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Arrow
import Options.Applicative
import Vocoder
import Vocoder.Filter
import Vocoder.Window
import Vocoder.Dunai
import FRP.Rhine
import MVarClock

type AudioV = V.Vector Audio.Sample

type EventIO = EventMVarT AudioV IO

data WindowType = BoxWindow | HammingWindow | HannWindow | BlackmanWindow | FlatTopWindow deriving (Read, Show)

data Options = Options {
    optMaybeFrameSize :: Maybe Length,
    optWindowSize :: Length,
    optHopSize :: HopSize,
    optWindowType :: WindowType,
    optFilter :: Filter
}

optFrameSize opts = maybe (optWindowSize opts) id $ optMaybeFrameSize opts

optWindow opts = windowFun (optWindowType opts) (optWindowSize opts)

windowFun BoxWindow = boxWindow
windowFun HammingWindow = hammingWindow
windowFun HannWindow = hannWindow
windowFun BlackmanWindow = blackmanWindow
windowFun FlatTopWindow = flatTopWindow

paramsFor opts = vocoderParams (optFrameSize opts) (optHopSize opts) (optWindow opts)

auto2 :: (Read a, Read b) => ReadM (a, b)
auto2 = maybeReader $ f . splitOn ","
    where
    f [a,b] = (,) <$> readMaybe a  <*> readMaybe b
    f _ = Nothing

filterP = (lowpassBrickwall <$> option auto
             ( long "lowpassBrickwall"
            <> metavar "FREQ"
            <> help "Low-pass brickwall filter"))
      <|> (highpassBrickwall <$> option auto
             ( long "highpassBrickwall"
            <> metavar "FREQ"
            <> help "High-pass brickwall filter"))
      <|> (uncurry bandpassBrickwall <$> option auto2
             ( long "bandpassBrickwall"
            <> metavar "FREQ,FREQ"
            <> help "Band-pass brickwall filter"))
      <|> (uncurry bandstopBrickwall <$> option auto2
             ( long "bandstopBrickwall"
            <> metavar "FREQ,FREQ"
            <> help "Band-stop brickwall filter"))

options :: Parser Options
options = Options
    <$> optional (option auto
        ( long "frameSize"
       <> metavar "SIZE"
       <> help "Size of zero-padded FFT frame, must be >= windowSize"))
    <*> option auto
        ( long "windowSize"
       <> metavar "SIZE"
       <> value 512
       <> showDefault
       <> help "Size of STFT window, must be divisible by hopSize")
    <*> option auto
        ( long "hopSize"
       <> metavar "SIZE"
       <> value 32
       <> showDefault
       <> help "STFT hop size, must be a power of 2")
    <*> option auto
        ( long "windowType"
       <> metavar "TYPE"
       <> value BlackmanWindow
       <> showDefault
       <> help "Type of STFT window")
    <*> filterP

runFilter client opts i = do
    rate <- liftIO $ JACK.getSampleRate client
    let freqStep = fromIntegral rate / fromIntegral (optFrameSize opts)
    return $ map (optFilter opts freqStep) i

processing :: (MonadIO m, Tag cl ~ AudioV) => JACK.Client -> Options -> MVar AudioV -> ClSF m cl () ()
processing client opts omvar = 
            tagS 
        >>> arr (V.map realToFrac) 
        >>> timeless (processS params $ arrM $ runFilter client opts)
        >>> arr (V.map realToFrac) 
        >>> arrMCl (liftIO . fmap (const ()) . tryPutMVar omvar)
    where
    params = paramsFor opts

main :: IO ()
main = execParser opts >>= run
    where
    opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Process JACK stream"
           <> header "Phase vocoder audio processing")

run opts = do
    imvar <- newEmptyMVar
    omvar <- newEmptyMVar 
    JACK.handleExceptions $ 
        JACK.withClientDefault "vocoder-jack" $ \client -> 
        JACK.withPort client "input" $ \iport ->
        JACK.withPort client "output" $ \oport ->
        JACK.withProcess client (lift . processJack imvar omvar iport oport) $
        JACK.withActivation client $ do
            lift $ forkIO $ flow $ processing client opts omvar @@ (mVarClockOn imvar :: HoistClock EventIO IO (MVarClock AudioV))
            lift $ JACK.waitForBreak
    
processJack :: MVar AudioV -> MVar AudioV -> Audio.Port JACK.Input -> Audio.Port JACK.Output -> JACK.NFrames -> IO ()
processJack imvar omvar iport oport nframes@(JACK.NFrames frames) = do
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

