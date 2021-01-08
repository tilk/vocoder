{-| 
    Module      : Vocoder.Conduit.Filter
    Description : Frequency-domain filters in Conduit
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module defines some useful frequency-domain filters as conduits.
It includes convenience wrappers for filters defined in the vocoder package.
-}
{-# LANGUAGE RankNTypes #-}
module Vocoder.Conduit.Filter(
      Filter,
      runFilter,
      realtimeFilter,
      amplitudeFilter,
      linearAmplitudeFilter,
      lowpassBrickwall,
      highpassBrickwall,
      bandpassBrickwall,
      bandstopBrickwall
    ) where

import Vocoder
import qualified Vocoder.Filter as F
import Data.Conduit
import qualified Data.Conduit.Combinators as DCC

-- | Conduit frequency-domain filter type. A conduit filter extends 
--   basic frequency-domain filters by using a conduit instead of a
--   pure function. This enables time transformation filters.
newtype Filter m = Filter { runFilter :: forall f. Functor f => F.FreqStep -> ConduitT (f STFTFrame) (f STFTFrame) m () }

-- | Use a basic frequency-domain filter as a conduit filter.
realtimeFilter :: Monad m => F.Filter -> Filter m
realtimeFilter f = Filter (\step -> DCC.map $ fmap $ f step)

-- | Creates a conduit filter which transforms only amplitudes, leaving
--   phase increments unchanged.
amplitudeFilter :: Monad m => (F.FreqStep -> Moduli -> Moduli) -> Filter m
amplitudeFilter = realtimeFilter . F.amplitudeFilter

-- | Creates a filter which scales amplitudes depending on frequency.
linearAmplitudeFilter :: Monad m => (Double -> Double) -> Filter m
linearAmplitudeFilter = realtimeFilter . F.linearAmplitudeFilter

-- | Creates a brickwall lowpass filter.
lowpassBrickwall :: Monad m => Double -> Filter m
lowpassBrickwall t = realtimeFilter $ F.lowpassBrickwall t

-- | Creates a brickwall highpass filter.
highpassBrickwall :: Monad m => Double -> Filter m
highpassBrickwall t = realtimeFilter $ F.highpassBrickwall t

-- | Creates a brickwall bandpass filter.
bandpassBrickwall :: Monad m => Double -> Double -> Filter m
bandpassBrickwall t u = realtimeFilter $ F.bandpassBrickwall t u

-- | Creates a brickwall bandstop filter.
bandstopBrickwall :: Monad m => Double -> Double -> Filter m
bandstopBrickwall t u = realtimeFilter $ F.bandstopBrickwall t u

