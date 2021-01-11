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
    amplitudeFilter,
    linearAmplitudeFilter,
    lowpassBrickwall,
    highpassBrickwall,
    bandpassBrickwall,
    bandstopBrickwall,
    lowpassButterworth,
    highpassButterworth,
    bandpassButterworth,
    bandstopButterworth
    ) where

import Vocoder
import qualified Data.Vector.Storable as V

-- | A frequency step is a coefficient relating physical frequency (in Hz)
--   to FFT bin numbers. It is used to define filters independently of the
--   FFT window size.
type FreqStep = Double

-- | The type of frequency-domain filters. A frequency-domain filter is
--   a function transforming STFT frames which can depend on the
--   frequency step.
type Filter = FreqStep -> STFTFrame -> STFTFrame

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

