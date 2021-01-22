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
    idFilter,
    amplitudeFilter,
    linearAmplitudeFilter,
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
    neutralPhaseFilter
    ) where

import Vocoder
import Vocoder.Window(blackmanWindow)
import qualified Data.Vector.Storable as V

-- | A frequency step is a coefficient relating physical frequency (in Hz)
--   to FFT bin numbers. It is used to define filters independently of the
--   FFT window size.
type FreqStep = Double

-- | The type of frequency-domain filters. A frequency-domain filter is
--   a function transforming STFT frames which can depend on the
--   frequency step.
type Filter = FreqStep -> STFTFrame -> STFTFrame

-- | Sequential composition of filters.
composeFilters :: Filter -> Filter -> Filter
composeFilters f1 f2 step = f2 step . f1 step

-- | Identity filter.
idFilter :: Filter
idFilter _ = id

-- | Creates a filter which transforms only amplitudes, leaving phase
--   increments unchanged.
amplitudeFilter :: (FreqStep -> Moduli -> Moduli) -> Filter
amplitudeFilter f step (mag, ph_inc) = (f step mag, ph_inc)

-- | Creates a filter which scales amplitudes depending on frequency.
linearAmplitudeFilter :: (Double -> Double) -> Filter
linearAmplitudeFilter f = amplitudeFilter $ \step mag -> V.zipWith (*) mag $ V.generate (V.length mag) $ \k -> f (step * fromIntegral k)

-- | Creates a brickwall lowpass filter.
lowpassBrickwall :: Double -> Filter
lowpassBrickwall t = linearAmplitudeFilter $ \x -> if x <= t then 1.0 else 0.0 

-- | Creates a brickwall highpass filter.
highpassBrickwall :: Double -> Filter
highpassBrickwall t = linearAmplitudeFilter $ \x -> if x >= t then 1.0 else 0.0 

-- | Creates a brickwall bandpass filter.
bandpassBrickwall :: Double -> Double -> Filter
bandpassBrickwall t u = linearAmplitudeFilter $ \x -> if x >= t && x <= u then 1.0 else 0.0 

-- | Creates a brickwall bandstop filter.
bandstopBrickwall :: Double -> Double -> Filter
bandstopBrickwall t u = linearAmplitudeFilter $ \x -> if x <= t || x >= u then 1.0 else 0.0 

butterworthGain :: Double -> Double -> Double -> Double
butterworthGain n t x = 1 / sqrt (1 + (x / t)**(2 * n))

-- | Creates an n-th degree Butterworth-style lowpass filter.
lowpassButterworth :: Double -> Double -> Filter
lowpassButterworth n t = linearAmplitudeFilter $ butterworthGain n t

-- | Creates an n-th degree Butterworth-style highpass filter.
highpassButterworth :: Double -> Double -> Filter
highpassButterworth n t = linearAmplitudeFilter $ butterworthGain (-n) t

-- | Creates an n-th degree Butterworth-style bandpass filter.
bandpassButterworth :: Double -> Double -> Double -> Filter
bandpassButterworth n t u = linearAmplitudeFilter $ \x -> butterworthGain n u x * butterworthGain (-n) t x

-- | Creates an n-th degree Butterworth-style bandstop filter.
bandstopButterworth :: Double -> Double -> Double -> Filter
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
pitchShiftInterpolate :: Double -> Filter
pitchShiftInterpolate n _ (mag, ph_inc) = (interpolate n mag, V.map (/n) $ interpolate n ph_inc)

-- | Convolves the amplitude spectrum using a kernel.
convolution :: V.Vector Double -> Moduli -> Moduli
convolution ker mag = V.generate (V.length mag) $ \k -> V.sum $ flip V.imap ker $ \i v -> v * gmag V.! (i + k)
    where
    h = V.length ker `div` 2
    gmag = V.replicate h 0 V.++ mag V.++ V.replicate h 0

-- | Creates a filter which convolves the spectrum using a kernel.
convolutionFilter :: V.Vector Double -> Filter
convolutionFilter ker = amplitudeFilter $ \_ -> convolution ker

-- | Calculates the envelope of an amplitude spectrum using convolution.
envelope :: Length -> Moduli -> Moduli
envelope ksize = V.map ((+(-ee)) . exp) . convolution ker . V.map (log . (+ee))
    where
    ee = 2**(-24)
    ker = blackmanWindow ksize

-- | Creates a filter which replaces the amplitudes with their envelope.
envelopeFilter :: Length -> Filter
envelopeFilter ksize = amplitudeFilter $ \_ -> envelope ksize

-- | Sets the phase increments so that the bins have horizontal consistency.
--   This erases the phase information, introducing "phasiness".
neutralPhaseFilter :: Filter
neutralPhaseFilter _ (mag, ph_inc) = (mag, ph_inc')
    where
    ph_inc' = V.generate (V.length ph_inc) f
    f i = k * fromIntegral i
    k = pi / fromIntegral (V.length ph_inc - 1)

