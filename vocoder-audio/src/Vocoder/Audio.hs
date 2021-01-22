{-| 
    Module      : Vocoder.Audio
    Description : Frequency-domain filters
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module allows easy frequency-domain processing on audio streams
created in @conduit-audio@.
-}
module Vocoder.Audio(
    VocoderAudioSource(..),
    concatenateV,
    sourceVocoder,
    sourceVocoderWithPhase,
    processAudio,
    processAudioWithPhase,
    processVocoderAudio
  ) where

import Data.Conduit
import Data.Conduit.Audio
import qualified Data.Conduit.Combinators as DCC
import Control.Applicative
import Control.Monad
import Vocoder
import Vocoder.Conduit
import Vocoder.Conduit.Filter
import Vocoder.Conduit.Frames
import qualified Data.Vector.Storable as V

data VocoderAudioSource m = VocoderAudioSource {
    sourceV :: (Frame, (ZipList Phase, ZipList Phase)) 
            -> ConduitT () Frame m (V.Vector Double, (ZipList Phase, ZipList Phase)),
    rateV :: Rate,
    channelsV :: Channels,
    framesV :: Frames,
    vocoderParamsV :: VocoderParams
}

-- | Applies a conduit filter to an audio stream, producing a vocoder stream.
--   This allows to seamlessly concatenate audio streams for vocoder processing.
processVocoderAudio 
    :: Monad m
    => VocoderParams
    -> Filter m
    -> AudioSource m Double 
    -> VocoderAudioSource m
processVocoderAudio par c src = VocoderAudioSource newSource (rate src) (channels src) (frames src) par where
    freqStep = rate src / fromIntegral (vocFrameLength par)
    newSource (q, ps) = 
        (source src .| genFramesOfE (vocInputFrameLength par * channels src) (vocHopSize par * channels src) q)
        `fuseBoth`
        (DCC.map (ZipList . deinterleave (channels src)) .| (snd <$> processFramesF par ps (runFilter c freqStep)))
        `fuseUpstream`
        DCC.map (interleave . getZipList)

-- | Connects the end of the first vocoder source to the beginning of the second. 
--   The two sources must have the same sample rate, channel count, vocoder hop size
--   and frame length.
concatenateV :: Monad m 
             => VocoderAudioSource m
             -> VocoderAudioSource m
             -> VocoderAudioSource m
concatenateV src1 src2 
    | rateV src1          /= rateV src2         = error "Vocoder.Audio.concatenateV: mismatched rates"
    | channelsV src1      /= channelsV src2     = error "Vocoder.Audio.concatenateV: mismatched channels"
    | vocHopSize par1     /= vocHopSize par2    = error "Vocoder.Audio.concatenateV: mismatched hop size"
    | vocFrameLength par1 /= vocFrameLength par2 = error "Vocoder.Audio.concatenateV: mismatched frame length"
    | otherwise = VocoderAudioSource (sourceV src1 >=> sourceV src2) (rateV src1) (channelsV src1) (framesV src1 + framesV src2) (vocoderParamsV src1)
    where
    par1 = vocoderParamsV src1
    par2 = vocoderParamsV src2

-- | Creates an audio source from a vocoder source.
sourceVocoder :: Monad m 
              => VocoderAudioSource m
              -> AudioSource m Double
sourceVocoder src = sourceVocoderWithPhase (zeroPhase $ vocoderParamsV src) src

-- | Creates an audio source from a vocoder source, with initial phase provided.
sourceVocoderWithPhase 
    :: Monad m 
    => Phase
    -> VocoderAudioSource m
    -> AudioSource m Double
sourceVocoderWithPhase iphs src = AudioSource newSource (rateV src) (channelsV src) (framesV src)
    where
    par = vocoderParamsV src
    phs = ZipList $ replicate (channelsV src) iphs
    newSource = (sourceV src (V.empty, (phs, phs)) >> return ())
             .| sumFramesE (chunkSize * channelsV src) (vocHopSize par * channelsV src)

-- | Applies a conduit filter to an audio stream.
processAudio 
    :: Monad m
    => VocoderParams
    -> Filter m
    -> AudioSource m Double 
    -> AudioSource m Double
processAudio par = processAudioWithPhase par (zeroPhase par)

-- | Applies a conduit filter to an audio stream, with initial phase provided.
processAudioWithPhase
    :: Monad m
    => VocoderParams
    -> Phase
    -> Filter m
    -> AudioSource m Double 
    -> AudioSource m Double
processAudioWithPhase par iphs c src = sourceVocoderWithPhase iphs $ processVocoderAudio par c src



