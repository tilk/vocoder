module Vocoder.Audio where

import Data.Conduit
import Data.Conduit.Audio
import qualified Data.Conduit.Combinators as DCC
import Control.Applicative
import Vocoder
import Vocoder.Filter
import Vocoder.Conduit
import Vocoder.Conduit.Utils

processA :: Monad m
         => VocoderParams
         -> (FreqStep -> ConduitT [STFTFrame] [STFTFrame] m ())
         -> AudioSource m Double 
         -> AudioSource m Double
processA par c src = AudioSource newSource (rate src) (channels src) (frames src) where
    phs = ZipList $ replicate (channels src) $ zeroPhase par
    freqStep = rate src / fromIntegral (vocFrameLength par)
    newSource = source src 
             .| framesOfE (vocInputFrameLength par * channels src) (vocHopSize par * channels src) 
             .| DCC.map (deinterleave $ channels src) 
             .| DCC.map ZipList
             .| (processF par (phs, phs) (DCC.map getZipList .| c freqStep .| DCC.map ZipList) >> return ()) 
             .| DCC.map getZipList
             .| DCC.map interleave
             .| sumFramesWithE (chunkSize * channels src) (vocHopSize par * channels src) 

