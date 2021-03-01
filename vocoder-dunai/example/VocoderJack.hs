{-# LANGUAGE TypeFamilies, RankNTypes, PatternSynonyms, ViewPatterns, TupleSections #-}
module Main where

import qualified Sound.JACK as JACK
import Sound.JACK.Exception
import qualified Sound.JACK.Audio as Audio
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.List.Split
import Text.Read hiding (lift)
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Reader(ReaderT(..))
import Control.Monad.Exception.Synchronous(ExceptionalT)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Control.Arrow
import Options.Applicative
import System.Exit
import Vocoder
import Vocoder.Filter
import Vocoder.Window
import Vocoder.Dunai
import FRP.Rhine
import MVarClock
import ProcessingTree
import VocoderJackFFI

type AudioV = V.Vector Audio.Sample

type EventIO = EventMVarT () IO

type MyClock = HoistClock EventIO IO (MVarClock ())

type MyMonad = ReaderT (TimeInfo MyClock) IO

data WindowType = BoxWindow | HammingWindow | HannWindow | BlackmanWindow | FlatTopWindow deriving (Read, Show)

data Cmd = SourceCmd Int | MSFCmd (MSF MyMonad [STFTFrame] [STFTFrame]) | FilterCmd (Filter MyMonad) | NamedCmd String | BindCmd String | BinaryCmd (STFTFrame -> STFTFrame -> STFTFrame)

data Options = Options {
    optClientName :: String,
    optMaybeFrameSize :: Maybe Length,
    optWindowSize :: Length,
    optHopSize :: HopSize,
    optWindowType :: WindowType,
    optProcessingTree :: ProcessingTree MyMonad
}

optFrameSize :: Options -> Length
optFrameSize opts = maybe (optWindowSize opts) id $ optMaybeFrameSize opts

optWindow :: Options -> Window
optWindow opts = windowFun (optWindowType opts) (optWindowSize opts)

optSources :: Options -> Int
optSources opts = numSourcesPT $ optProcessingTree opts

windowFun :: WindowType -> Length -> Window
windowFun BoxWindow = boxWindow
windowFun HammingWindow = hammingWindow
windowFun HannWindow = hannWindow
windowFun BlackmanWindow = blackmanWindow
windowFun FlatTopWindow = flatTopWindow

paramsFor :: Options -> VocoderParams
paramsFor opts = vocoderParams (optFrameSize opts) (optHopSize opts) (optWindow opts)

auto2 :: (Read a, Read b) => ReadM (a, b)
auto2 = maybeReader $ f . splitOn ","
    where
    f [a,b] = (,) <$> readMaybe a  <*> readMaybe b
    f _ = Nothing

auto3 :: (Read a, Read b, Read c) => ReadM (a, b, c)
auto3 = maybeReader $ f . splitOn ","
    where
    f [a,b,c] = (,,) <$> readMaybe a  <*> readMaybe b <*> readMaybe c
    f _ = Nothing

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a,b,c) = f a b c

processingP :: Parser (ProcessingTree MyMonad)
processingP = parseCommands <$> many commandP

ptht :: [ProcessingTree MyMonad] -> [ProcessingTree MyMonad]
ptht (h:t) = h:t
ptht [] = [PTSource 0]

infixr :?

pattern (:?) :: ProcessingTree MyMonad
             -> [ProcessingTree MyMonad] 
             -> [ProcessingTree MyMonad]
pattern h :? t <- (ptht -> h:t)

parseCommands :: [Cmd] -> ProcessingTree MyMonad
parseCommands cmds = p [] cmds 
    where
    p (h :? _) [] = h
    p s             (SourceCmd k : t) = p (PTSource k : s) t
    p s             (NamedCmd n  : t) = p (PTNamed n : s) t
    p (h :? s)      (MSFCmd f    : t) = p (PTMSF f h : s) t
    p (h :? s)      (FilterCmd f : t) = p (PTFilter f h : s) t
    p (h :? i :? s) (BinaryCmd f : t) = p (PTBinary f i h : s) t
    p (h :? s)      (BindCmd n   : t) = p (PTBind n h : s) t

mkBinary :: (Double -> Double -> Double)
         -> (Double -> Double -> Double)
         -> STFTFrame -> STFTFrame -> STFTFrame
mkBinary op1 op2 (mag1, ph_inc1) (mag2, ph_inc2) = (V.zipWith op1 mag1 mag2, V.zipWith op2 ph_inc1 ph_inc2)

commandP :: Parser Cmd
commandP = FilterCmd <$> filterP
       <|> MSFCmd <$> msfP
       <|> (SourceCmd <$> option auto
             ( long "source"
            <> metavar "NUM"
            <> help "Source from JACK input"))
       <|> (NamedCmd <$> option auto
             ( long "named"
            <> metavar "NAME"
            <> help "Source from named stream"))
       <|> (BindCmd <$> option auto
             ( long "bind"
            <> metavar "NAME"
            <> help "Bind stream to name"))
       <|> (flag' (BinaryCmd $ mkBinary (*) (+))
             ( long "multiply"
            <> help "Multiply two streams"))
       <|> (flag' (BinaryCmd $ mkBinary (/) (-))
             ( long "divide"
            <> help "Divide two streams"))
       <|> (flag' (BinaryCmd addFrames)
             ( long "add"
            <> help "Add two streams"))

delayMSF :: Int -> MSF MyMonad [STFTFrame] [STFTFrame]
delayMSF k = mealy f []
    where 
    f i s = (take (length i) s', drop (length s' - k) s') where s' = s ++ i

msfP :: Parser (MSF MyMonad [STFTFrame] [STFTFrame])
msfP = (delayMSF <$> option auto
             ( long "delay"
            <> metavar "HOPS"
            <> help "Delay the signal by some number of STFT hops"))

filterP :: Parser (Filter MyMonad)
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
      <|> (uncurry lowpassButterworth <$> option auto2
             ( long "lowpassButterworth"
            <> metavar "DEG,FREQ"
            <> help "Low-pass Butterworth-style filter"))
      <|> (uncurry highpassButterworth <$> option auto2
             ( long "highpassButterworth"
            <> metavar "DEG,FREQ"
            <> help "High-pass Butterworth-style filter"))
      <|> (uncurry3 bandpassButterworth <$> option auto3
             ( long "bandpassButterworth"
            <> metavar "DEG,FREQ,FREQ"
            <> help "Band-pass Butterworth-style filter"))
      <|> (uncurry3 bandstopButterworth <$> option auto3
             ( long "bandstopButterworth"
            <> metavar "DEG,FREQ,FREQ"
            <> help "Band-stop Butterworth-style filter"))
      <|> (amplify <$> option auto
             ( long "amplify"
            <> metavar "COEFF"
            <> help "Change amplitude"))
      <|> (pitchShiftInterpolate <$> option auto
             ( long "pitchShiftInterpolate"
            <> metavar "COEFF"
            <> help "Interpolative pitch-shift"))
      <|> (envelopeFilter <$> option auto
             ( long "envelope"
            <> metavar "KSIZE"
            <> help "Calculate spectral envelope"))
      <|> (flag' (randomPhaseFilter)
             ( long "randomPhase"
            <> help "Randomize phases (Paulstretch effect)"))

options :: Parser Options
options = Options
    <$> option auto
        ( long "clientName"
       <> metavar "NAME"
       <> value "vocoder-jack"
       <> help "JACK client name")
    <*> optional (option auto
        ( long "frameSize"
       <> metavar "SIZE"
       <> help "Size of zero-padded FFT frame, must be >= windowSize"))
    <*> option auto
        ( long "windowSize"
       <> metavar "SIZE"
       <> value 1024
       <> showDefault
       <> help "Size of STFT window, must be divisible by hopSize")
    <*> option auto
        ( long "hopSize"
       <> metavar "SIZE"
       <> value 128
       <> showDefault
       <> help "STFT hop size, must be a power of 2")
    <*> option auto
        ( long "windowType"
       <> metavar "TYPE"
       <> value BlackmanWindow
       <> showDefault
       <> help "Type of STFT window")
    <*> processingP

runFilter :: JACK.Client -> Options -> ClSF IO MyClock [[STFTFrame]] [STFTFrame]
runFilter client opts = ret
    where
    freqStep = do
        rate <- liftIO $ JACK.getSampleRate client
        return $ fromIntegral rate / fromIntegral (optFrameSize opts)
    (Just ret) = elaboratePT freqStep (optProcessingTree opts)

processing :: JACK.Client -> Options -> [VM.IOVector Float] -> VM.IOVector Float -> ClSF IO MyClock () ()
processing client opts ibufs obuf = 
            arrMCl (\_ -> liftIO $ mapM V.freeze ibufs)
        >>> arr (map $ V.map realToFrac) 
        >>> ((analysisSrcs srcs >>> runFilter client opts >>> synthesis par (zeroPhase par)) &&& arr (V.length . head))
        >>> sumFramesWithLengthS (vocHopSize par) >>> volumeFix par
        >>> arr (V.map realToFrac) 
        >>> arrMCl (liftIO . V.copy obuf)
    where
    par = paramsFor opts
    srcs = optSources opts
    analysisSrcs 0 = pure []
    analysisSrcs k = (:) <$> (arr (!! (srcs-k)) >>> framesOfS (vocInputFrameLength par) (vocHopSize par) >>> analysis par (zeroPhase par)) <*> analysisSrcs (k-1)

main :: IO ()
main = execParser opts >>= run
    where
    opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Process JACK stream"
           <> header "Phase vocoder audio processing")

withInputPorts :: (ThrowsPortRegister e, ThrowsErrno e)
               => JACK.Client
               -> Options
               -> ([Audio.Port JACK.Input] -> ExceptionalT e IO a)
               -> ExceptionalT e IO a
withInputPorts client opts cont = f (optSources opts) [] where
    f 0 l = cont l
    f k l = JACK.withPort client ("input" ++ show k) $ \iport -> f (k-1) (iport:l)

run :: Options -> IO ()
run opts = do
    cmvar <- newEmptyMVar
    JACK.handleExceptions $ 
        JACK.withClientDefault (optClientName opts) $ \client ->
        withInputPorts client opts $ \iports ->
        JACK.withPort client "output" $ \oport -> do
            bufsize <- lift $ JACK.getBufferSize client
            obuf <- lift $ VM.new bufsize
            iportbufs <- lift $ forM iports $ \p -> (p, ) <$> VM.new bufsize
            withVocoderProcess client iportbufs (oport, obuf) (tryPutMVar cmvar () >> return ()) $
                JACK.withBufferSize client (\(JACK.NFrames bs) -> if fromIntegral bs == bufsize then return () else lift $ exitFailure) $
                JACK.withLatency client (latency iports oport (JACK.NFrames $ fromIntegral bufsize)) $
                JACK.withActivation client $ do
                    _ <- lift $ forkIO $ flow $ processing client opts (map snd iportbufs) obuf @@ mVarClockOn cmvar
                    lift $ JACK.waitForBreak
    
latency :: [Audio.Port JACK.Input]
        -> Audio.Port JACK.Output
        -> JACK.NFrames
        -> JACK.LatencyCallbackMode
        -> IO ()
latency ips op (JACK.NFrames bufsize) m | m == JACK.jackCaptureLatency = do
    ranges <- forM ips $ \ip -> do
        JACK.LatencyRange (JACK.NFrames a) (JACK.NFrames b) <- JACK.getLatencyRange ip m
        return (a, b)
    let a = maximum $ map fst ranges
    let b = maximum $ map snd ranges
    JACK.setLatencyRange op m (JACK.LatencyRange (JACK.NFrames $ a+bufsize) (JACK.NFrames $ b+bufsize))
latency ips op (JACK.NFrames bufsize) m | otherwise = do
    JACK.LatencyRange (JACK.NFrames a) (JACK.NFrames b) <- JACK.getLatencyRange op m
    forM_ ips $ \ip ->
        JACK.setLatencyRange ip m (JACK.LatencyRange (JACK.NFrames $ a+bufsize) (JACK.NFrames $ b+bufsize))

