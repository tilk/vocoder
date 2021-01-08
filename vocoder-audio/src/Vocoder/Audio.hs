{-| 
    Module      : Vocoder.Audio
    Description : Frequency-domain filters
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module allows easy frequency-domain processing on audio streams
created in @conduit-audio@.
-}
module Vocoder.Audio where

import Data.Conduit
import Data.Conduit.Audio
import qualified Data.Conduit.Combinators as DCC
import Control.Applicative
import Vocoder
import Vocoder.Conduit
import Vocoder.Conduit.Filter
import Vocoder.Conduit.Frames

-- | Applies a conduit filter to an audio stream.
processA :: Monad m
         => VocoderParams
         -> Filter m
         -> AudioSource m Double 
         -> AudioSource m Double
processA par c src = AudioSource newSource (rate src) (channels src) (frames src) where
    phs = ZipList $ replicate (channels src) $ zeroPhase par
    freqStep = rate src / fromIntegral (vocFrameLength par)
    newSource = source src 
             .| framesOfE (vocInputFrameLength par * channels src) (vocHopSize par * channels src) 
             .| DCC.map (deinterleave $ channels src) 
             .| DCC.map ZipList
             .| (processFramesF par (phs, phs) (runFilter c freqStep) >> return ()) 
             .| DCC.map getZipList
             .| DCC.map interleave
             .| sumFramesE (chunkSize * channels src) (vocHopSize par * channels src) 

