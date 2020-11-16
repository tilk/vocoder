module Vocoder.Audio where

import Data.Conduit
import Data.Conduit.Audio
import qualified Data.Conduit.Combinators as DCC
import Control.Applicative
import Vocoder
import Vocoder.Conduit
import Vocoder.ConduitUtils
import qualified Data.Vector.Storable as V

processA :: Monad m
         => Vocoder_params
         -> ConduitT [SFT_block] [SFT_block] m ()
         -> AudioSource m Double 
         -> AudioSource m Double
processA par c src = AudioSource newSource (rate src) (channels src) (frames src) where
    phs = ZipList $ replicate (channels src) $ zero_phase par
    newSource = source src 
             .| framesOfE (input_frame_length par * channels src) (hop_size par * channels src) 
             .| DCC.map (deinterleave $ channels src) 
             .| DCC.map ZipList
             .| (processF par (phs, phs) (DCC.map getZipList .| c .| DCC.map ZipList) >> return ()) 
             .| DCC.map getZipList
             .| DCC.map interleave
             .| sumFramesWithE 0 (V.zipWith (+)) (chunkSize * channels src) (hop_size par * channels src) 

