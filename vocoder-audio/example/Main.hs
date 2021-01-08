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
import Control.Monad.Trans.Resource

data WindowType = BoxWindow | HammingWindow | HannWindow | BlackmanWindow | FlatTopWindow deriving (Read, Show)

data Options = Options {
    sourceFile :: String,
    destFile :: String,
    optFrameSize :: Maybe Length,
    windowSize :: Length,
    hopSizeO :: HopSize,
    windowType :: WindowType,
    filterOpt :: Filter (ResourceT IO)
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

auto2 :: (Read a, Read b) => ReadM (a, b)
auto2 = maybeReader $ f . splitOn ","
    where
    f [a,b] = (,) <$> readMaybe a  <*> readMaybe b
    f _ = Nothing

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

options :: Parser Options
options = Options
    <$> argument str (metavar "SRC")
    <*> argument str (metavar "DST")
    <*> optional (option auto 
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
    <*> filterP

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
    src :: AudioSource (ResourceT IO) Double <- sourceSnd $ sourceFile opts
    let params = vocoderParams (frameSize opts) (hopSizeO opts) (windowFor opts)
    runResourceT $ sinkSnd (destFile opts) myFormat $ processA params (filterOpt opts) src

