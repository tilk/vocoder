{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
module Main where

import System.Environment
import Vocoder
import VocoderAudio
import Sound.File.Sndfile
import qualified Data.Conduit.Combinators as DCC
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Control.Monad.Trans.Resource

import qualified Data.Vector.Storable as V

myFormat = Format {headerFormat = HeaderFormatWav, sampleFormat = SampleFormatPcm16, endianFormat = EndianFile}

main = do
    [srcfile, dstfile] <- getArgs
    src :: AudioSource _ Double <- sourceSnd srcfile
    let trans = DCC.map id
    let window = V.replicate 512 1
    let params = make_vocoder_params 512 32 window
    runResourceT $ sinkSnd dstfile myFormat $ processA params trans src


