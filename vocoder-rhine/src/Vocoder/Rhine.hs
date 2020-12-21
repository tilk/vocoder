module Vocoder.Rhine where

import FRP.Rhine
import Data.Tuple(swap)
import qualified Data.Vector.Storable as V
import Vocoder

volumeFix :: Monad m => VocoderParams -> MSF m STFTFrame STFTFrame
volumeFix par = arr $ V.map (* volumeCoeff par) *** id

analysis :: Monad m => VocoderParams -> Phase -> MSF m Frame STFTFrame
analysis par = mealy $ \a s -> swap $ analysisStep (hopSize par) (frameLength par) s $ doFFT par a

synthesis :: Monad m => VocoderParams -> Phase -> MSF m STFTFrame Frame
synthesis par = mealy $ \a s -> doIFFT par *** id $ swap $ synthesisStep (hopSize par) s a

