{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
module Main where

import System.Environment
import Options.Applicative
import Data.Semigroup ((<>))
import Vocoder
import Vocoder.Window
import Vocoder.Audio
import Vocoder.Conduit.Filter
import Sound.File.Sndfile
import qualified Data.Conduit.Combinators as DCC
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Control.Monad.Trans.Resource
import Control.Arrow

import qualified Data.Vector.Storable as V

data WindowType = BoxWindow | HammingWindow | HannWindow | BlackmanWindow | FlatTopWindow deriving (Read, Show)

data Options = Options {
    sourceFile :: String,
    destFile :: String,
    optFrameSize :: Maybe Length,
    windowSize :: Length,
    hopSize :: HopSize,
    windowType :: WindowType
}

frameSize opts = maybe (windowSize opts) id $ optFrameSize opts

windowFor opts = windowFun (windowType opts) (windowSize opts)

windowFun BoxWindow = boxWindow
windowFun HammingWindow = hammingWindow
windowFun HannWindow = hannWindow
windowFun BlackmanWindow = blackmanWindow
windowFun FlatTopWindow = flatTopWindow

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

myFormat = Format {headerFormat = HeaderFormatWav, sampleFormat = SampleFormatPcm16, endianFormat = EndianFile}

main = execParser opts >>= process 
    where
    opts = info (options <**> helper)
            ( fullDesc
           <> progDesc "Process audio file"
           <> header "etaoin shrdlu")

process :: Options -> IO ()
process opts = do
    src :: AudioSource _ Double <- sourceSnd $ sourceFile opts
    let params = make_vocoder_params (frameSize opts) (hopSize opts) (windowFor opts)
    let trans = lowpassBrickwall 500
    runResourceT $ sinkSnd (destFile opts) myFormat $ processA params trans src

