module Vocoder.Conduit(
      volumeFix,
      analysis,
      synthesis,
      process,
      volumeFixF,
      analysisF,
      synthesisF,
      processF
    ) where

import Data.Conduit
import qualified Data.Conduit.List as DCL
import qualified Data.List.NonEmpty as DLN
import qualified Data.Vector.Storable as V
import Control.Arrow
import Vocoder

volumeFix :: Monad m => VocoderParams -> ConduitT STFTFrame STFTFrame m ()
volumeFix par = DCL.map $ V.map (* volumeCoeff par) *** id

analysis :: Monad m => VocoderParams -> Phase -> ConduitT Frame STFTFrame m Phase
analysis par ph = DCL.mapAccum (flip $ analysisBlock par) ph

synthesis :: Monad m => VocoderParams -> Phase -> ConduitT STFTFrame Frame m Phase
synthesis par ph = DCL.mapAccum (flip $ synthesisBlock par) ph

process :: Monad m => VocoderParams -> (Phase, Phase) -> ConduitT STFTFrame STFTFrame m r -> ConduitT Frame Frame m (r, (Phase, Phase))
process par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysis par p1 `fuseBoth` (volumeFix par .| c) `fuseBoth` synthesis par p2

app_help :: Applicative f => (a -> s -> (s, b)) -> f a -> f s -> (f s, f b)
app_help f a b = DLN.unzip $ fmap (uncurry f) ((,) <$> a <*> b)

volumeFixF :: (Applicative f, Monad m) => VocoderParams -> ConduitT (f STFTFrame) (f STFTFrame) m ()
volumeFixF par = DCL.map $ fmap $ V.map (* volumeCoeff par) *** id

analysisF :: (Applicative f, Monad m) => VocoderParams -> f Phase -> ConduitT (f Frame) (f STFTFrame) m (f Phase)
analysisF par ph = DCL.mapAccum (app_help $ flip $ analysisBlock par) ph

synthesisF :: (Applicative f, Monad m) => VocoderParams -> f Phase -> ConduitT (f STFTFrame) (f Frame) m (f Phase)
synthesisF par ph = DCL.mapAccum (app_help $ flip $ synthesisBlock par) ph

processF :: (Applicative f, Monad m) => VocoderParams -> (f Phase, f Phase) -> ConduitT (f STFTFrame) (f STFTFrame) m r -> ConduitT (f Frame) (f Frame) m (r, (f Phase, f Phase))
processF par (p1, p2) c = (\((p1', r), p2') -> (r, (p1', p2'))) <$> analysisF par p1 `fuseBoth` (volumeFixF par .| c) `fuseBoth` synthesisF par p2

