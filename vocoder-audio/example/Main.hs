{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.List.Split
import Text.Read
import Vocoder
import Vocoder.Window
import Vocoder.Audio
import Vocoder.Conduit.Filter
import Sound.File.Sndfile
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Control.Monad
import Control.Monad.Trans.Resource

data WindowType = BoxWindow | HammingWindow | HannWindow | BlackmanWindow | FlatTopWindow deriving (Read, Show)

data Options = Options {
    optFrameSize :: Maybe Length,
    windowSize :: Length,
    hopSizeO :: HopSize,
    windowType :: WindowType,
    destFile :: String,
    sources :: [(String, Filter (ResourceT IO))]
}

frameSize :: Options -> Length
frameSize opts = maybe (windowSize opts) id $ optFrameSize opts

windowFor :: Options -> Window
windowFor opts = windowFun (windowType opts) (windowSize opts)

windowFun :: WindowType -> Length -> Window
windowFun BoxWindow = boxWindow
windowFun HammingWindow = hammingWindow
windowFun HannWindow = hannWindow
windowFun BlackmanWindow = blackmanWindow
windowFun FlatTopWindow = flatTopWindow

vocoderParamsFor :: Options -> VocoderParams
vocoderParamsFor opts = vocoderParams (frameSize opts) (hopSizeO opts) (windowFor opts)

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

sourceP :: Monad m => Parser (String, Filter m)
sourceP = (,) 
    <$> argument str (metavar "SRC")
    <*> filtersP

filtersP :: Monad m => Parser (Filter m)
filtersP = (\l -> if null l then idFilter else foldr1 composeFilters l) <$> many filterP

filterP :: Monad m => Parser (Filter m)
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
      <|> (pitchShiftInterpolate <$> option auto
             ( long "pitchShiftInterpolate"
            <> metavar "COEFF"
            <> help "Interpolative pitch-shift"))
      <|> (playSpeed <$> option (toRational <$> (auto :: ReadM Double))
             ( long "playSpeed"
            <> metavar "COEFF"
            <> help "Change speed by coefficient"))

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
       <> help "Size of STFT window")
    <*> option auto
        ( long "hopSize"
       <> metavar "SIZE"
       <> value 32
       <> showDefault
       <> help "STFT hop size")
    <*> option auto
        ( long "windowType"
       <> metavar "TYPE"
       <> value BlackmanWindow
       <> showDefault
       <> help "Type of STFT window")
    <*> argument str (metavar "DST")
    <*> some sourceP

myFormat :: Format
myFormat = Format {headerFormat = HeaderFormatWav, sampleFormat = SampleFormatPcm16, endianFormat = EndianFile}

main :: IO ()
main = execParser opts >>= process 
    where
    opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Process audio file"
           <> header "Phase vocoder audio processing")

process :: Options -> IO ()
process opts = do
    let params = vocoderParamsFor opts
    srcs :: [AudioSource (ResourceT IO) Double] <- forM (sources opts) $ \(n, f) -> processA params f <$> sourceSnd n
    runResourceT $ sinkSnd (destFile opts) myFormat $ foldl1 concatenate srcs

