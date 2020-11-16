module Vocoder.Conduit.Filter where

import Vocoder
import qualified Vocoder.Filter as F
import Data.Conduit
import qualified Data.Conduit.Combinators as DCC

type Filter f m = F.FreqStep -> ConduitT (f SFT_block) (f SFT_block) m ()

realtimeFilter :: (Functor f, Monad m) => F.Filter -> Filter f m
realtimeFilter f step = DCC.map $ fmap $ f step

amplitudeFilter :: (Functor f, Monad m) => (F.FreqStep -> Moduli -> Moduli) -> Filter f m
amplitudeFilter = realtimeFilter . F.amplitudeFilter

linearAmplitudeFilter :: (Functor f, Monad m) => (Double -> Double) -> Filter f m
linearAmplitudeFilter = realtimeFilter . F.linearAmplitudeFilter

lowpassBrickwall :: (Functor f, Monad m) => Double -> Filter f m
lowpassBrickwall t = realtimeFilter $ F.lowpassBrickwall t

highpassBrickwall :: (Functor f, Monad m) => Double -> Filter f m
highpassBrickwall t = realtimeFilter $ F.highpassBrickwall t

bandpassBrickwall :: (Functor f, Monad m) => Double -> Double -> Filter f m
bandpassBrickwall t u = realtimeFilter $ F.bandpassBrickwall t u

bandstopBrickwall :: (Functor f, Monad m) => Double -> Double -> Filter f m
bandstopBrickwall t u = realtimeFilter $ F.bandstopBrickwall t u

