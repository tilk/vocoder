{-# LANGUAGE ScopedTypeVariables, PartialTypeSignatures #-}
module Main where

import System.Environment
import Vocoder
import Vocoder.Audio
import Sound.File.Sndfile
import qualified Data.Conduit.Combinators as DCC
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Control.Monad.Trans.Resource
import Control.Arrow

import qualified Data.Vector.Storable as V

myFormat = Format {headerFormat = HeaderFormatWav, sampleFormat = SampleFormatPcm16, endianFormat = EndianFile}

main = do
    [srcfile, dstfile] <- getArgs
    src :: AudioSource _ Double <- sourceSnd srcfile
    let window = V.replicate 512 1
    let params = make_vocoder_params 512 32 window
    let volume_coeff = fromIntegral (hop_size params) / V.sum window
    let trans = DCC.map $ map $ V.map (* volume_coeff) *** id
    runResourceT $ sinkSnd dstfile myFormat $ processA params trans src


