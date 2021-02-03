{-# LANGUAGE TupleSections #-}
{-| 
    Module      : Vocoder.Filter
    Description : Frequency-domain filters
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module defines some useful frequency-domain filters for use in
the vocoder framework.
-}
module Vocoder.Filter (
    FreqStep,
    Filter,
    composeFilters,
    addFilters,
    idFilter,
    amplitudeFilter,
    linearAmplitudeFilter,
    amplify,
    lowpassBrickwall,
    highpassBrickwall,
    bandpassBrickwall,
    bandstopBrickwall,
    lowpassButterworth,
    highpassButterworth,
    bandpassButterworth,
    bandstopButterworth,
    pitchShiftInterpolate,
    convolution,
    convolutionFilter,
    envelope,
    envelopeFilter,
    randomPhaseFilter
    ) where

import Vocoder
import Vocoder.Window
import Control.Monad
import Control.Monad.IO.Class
import System.Random
import qualified Data.Vector.Storable as V

-- | A frequency step is a coefficient relating physical frequency (in Hz)
--   to FFT bin numbers. It is used to define filters independently of the
--   FFT window size.
type FreqStep = Double

-- | The type of frequency-domain filters. A frequency-domain filter is
--   a function transforming STFT frames which can depend on the
--   frequency step.
type Filter m = FreqStep -> STFTFrame -> m STFTFrame

-- | Sequential composition of filters.
composeFilters :: Monad m => Filter m -> Filter m -> Filter m
composeFilters f1 f2 step = f1 step >=> f2 step

-- | Addition of filters.
addFilters :: Monad m => Filter m -> Filter m -> Filter m
addFilters f1 f2 step fr = addFrames <$> f1 step fr <*> f2 step fr

-- | Identity filter.
idFilter :: Monad m => Filter m
idFilter _ = return

-- | Creates a filter which transforms only amplitudes, leaving phase
--   increments unchanged.
amplitudeFilter :: Monad m => (FreqStep -> Moduli -> Moduli) -> Filter m
amplitudeFilter f step (mag, ph_inc) = return (f step mag, ph_inc)

-- | Creates a filter which transforms amplitudes and zeroes the phase 
--   increments.
amplitudeFilter0 :: Monad m => (FreqStep -> Moduli -> Moduli) -> Filter m
amplitudeFilter0 f step (mag, ph_inc) = return (f step mag, V.replicate (V.length ph_inc) 0)

-- | Creates a filter which scales amplitudes depending on frequency.
linearAmplitudeFilter :: Monad m => (Double -> Double) -> Filter m
linearAmplitudeFilter f = amplitudeFilter $ \step mag -> V.zipWith (*) mag $ V.generate (V.length mag) $ \k -> f (step * fromIntegral k)

-- | Creates an "amplifier" which scales all frequencies.
amplify :: Monad m => Double -> Filter m
amplify k = linearAmplitudeFilter (const k)

-- | Creates a brickwall lowpass filter.
lowpassBrickwall :: Monad m => Double -> Filter m
lowpassBrickwall t = linearAmplitudeFilter $ \x -> if x <= t then 1.0 else 0.0 

-- | Creates a brickwall highpass filter.
highpassBrickwall :: Monad m => Double -> Filter m
highpassBrickwall t = linearAmplitudeFilter $ \x -> if x >= t then 1.0 else 0.0 

-- | Creates a brickwall bandpass filter.
bandpassBrickwall :: Monad m => Double -> Double -> Filter m
bandpassBrickwall t u = linearAmplitudeFilter $ \x -> if x >= t && x <= u then 1.0 else 0.0 

-- | Creates a brickwall bandstop filter.
bandstopBrickwall :: Monad m => Double -> Double -> Filter m
bandstopBrickwall t u = linearAmplitudeFilter $ \x -> if x <= t || x >= u then 1.0 else 0.0 

butterworthGain :: Double -> Double -> Double -> Double
butterworthGain n t x = 1 / sqrt (1 + (x / t)**(2 * n))

-- | Creates an n-th degree Butterworth-style lowpass filter.
lowpassButterworth :: Monad m => Double -> Double -> Filter m
lowpassButterworth n t = linearAmplitudeFilter $ butterworthGain n t

-- | Creates an n-th degree Butterworth-style highpass filter.
highpassButterworth :: Monad m => Double -> Double -> Filter m
highpassButterworth n t = linearAmplitudeFilter $ butterworthGain (-n) t

-- | Creates an n-th degree Butterworth-style bandpass filter.
bandpassButterworth :: Monad m => Double -> Double -> Double -> Filter m
bandpassButterworth n t u = linearAmplitudeFilter $ \x -> butterworthGain n u x * butterworthGain (-n) t x

-- | Creates an n-th degree Butterworth-style bandstop filter.
bandstopButterworth :: Monad m => Double -> Double -> Double -> Filter m
bandstopButterworth n t u = linearAmplitudeFilter $ \x -> butterworthGain (-n) t x + butterworthGain n u x

interpolate :: Double -> V.Vector Double -> V.Vector Double
interpolate n v = V.generate (V.length v) f 
    where
    f x | i + 1 >= V.length v = 0
        | otherwise = (1-k) * v V.! i + k * v V.! (i+1) where
            x' = n * fromIntegral x
            i = floor x'
            k = x' - fromIntegral i

-- | Creates an interpolative pitch-shifting filter.
pitchShiftInterpolate :: Monad m => Double -> Filter m
pitchShiftInterpolate n _ (mag, ph_inc) = return (interpolate n mag, V.map (/n) $ interpolate n ph_inc)

-- | Convolves the amplitude spectrum using a kernel.
convolution :: V.Vector Double -> Moduli -> Moduli
convolution ker mag = V.generate (V.length mag) $ \k -> V.sum $ flip V.imap ker $ \i v -> v * gmag V.! (i + k) / s
    where
    h = V.length ker `div` 2
    gmag = V.replicate h 0 V.++ mag V.++ V.replicate h 0
    s = V.sum ker

-- | Creates a filter which convolves the spectrum using a kernel.
convolutionFilter :: Monad m => V.Vector Double -> Filter m
convolutionFilter ker = amplitudeFilter0 $ \_ -> convolution ker

-- | Calculates the envelope of an amplitude spectrum using convolution.
envelope :: Length -> Moduli -> Moduli
envelope ksize = V.map ((+(-ee)) . exp) . convolution ker . V.map (log . (+ee))
    where
    ee = 2**(-24)
    ker = if ksize <= 3 then boxWindow ksize else blackmanWindow ksize

-- | Creates a filter which replaces the amplitudes with their envelope.
envelopeFilter :: Monad m => Length -> Filter m
envelopeFilter ksize = amplitudeFilter0 $ \_ -> envelope ksize

-- | Sets the phase increments so that the bins have horizontal consistency.
--   This erases the phase information, introducing "phasiness".
randomPhaseFilter :: MonadIO m => Filter m
randomPhaseFilter _ (mag, ph_inc) = (mag, ) <$> V.replicateM (V.length ph_inc) (randomRIO (0, 2*pi))

