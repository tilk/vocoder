module Vocoder.Rhine where

import FRP.Rhine
import Data.Tuple(swap)
import qualified Data.Vector.Storable as V
import Vocoder

volumeFix :: Monad m => VocoderParams -> MSF m STFTFrame STFTFrame
volumeFix par = arr $ V.map (* volumeCoeff par) *** id

analysis :: Monad m => VocoderParams -> Phase -> MSF m [Frame] [STFTFrame]
analysis par = mealy $ \a s -> swap $ analysisStage par s a

synthesis :: Monad m => VocoderParams -> Phase -> MSF m [STFTFrame] [Frame]
synthesis par = mealy $ \a s -> swap $ synthesisStage par s a

