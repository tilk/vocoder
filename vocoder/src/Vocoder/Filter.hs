module Vocoder.Filter where

import Vocoder
import qualified Data.Vector.Storable as V

type FreqStep = Double
type Filter = FreqStep -> SFT_block -> SFT_block

amplitudeFilter :: (FreqStep -> Moduli -> Moduli) -> Filter
amplitudeFilter f step (mag, ph_inc) = (f step mag, ph_inc)

linearAmplitudeFilter :: (Double -> Double) -> Filter
linearAmplitudeFilter f = amplitudeFilter $ \step mag -> V.zipWith (*) mag $ V.generate (V.length mag) $ \k -> f (step * fromIntegral k)

lowpassBrickwall :: Double -> Filter
lowpassBrickwall t = linearAmplitudeFilter $ \x -> if x <= t then 1.0 else 0.0 

highpassBrickwall :: Double -> Filter
highpassBrickwall t = linearAmplitudeFilter $ \x -> if x >= t then 1.0 else 0.0 

bandpassBrickwall :: Double -> Double -> Filter
bandpassBrickwall t u = linearAmplitudeFilter $ \x -> if x >= t && x <= u then 1.0 else 0.0 

bandstopBrickwall :: Double -> Double -> Filter
bandstopBrickwall t u = linearAmplitudeFilter $ \x -> if x <= t || x >= u then 1.0 else 0.0 


