module Vocoder (
      Moduli,
      Phase,
      PhaseInc,
      Frame,
      Window,
      HopSize,
      Length,
      STFTFrame,
      FFTOutput,
      VocoderParams,
      vocoderParams,
      vocFrameLength,
      vocInputFrameLength,
      vocHopSize,
      vocWindow,
      doFFT,
      doIFFT,
      analysisBlock,
      analysisStep,
      analysisStage,
      synthesisBlock,
      synthesisStep,
      synthesisStage,
      zeroPhase,
      volumeCoeff
    ) where

import Data.List
import Data.Complex
import Data.Fixed
import Data.Tuple
import Control.Arrow
import Numeric.FFT.Vector.Invertible as FFT
import Numeric.FFT.Vector.Plan as FFTp
import qualified Data.Vector.Storable as V

-- | Complex moduli of FFT frames. Represent signal amplitudes.
type Moduli = V.Vector Double

-- | Complex arguments of FFT frames. Represent signal phases.
type Phase = V.Vector Double

-- | Phase increments. Represent the deviation of the phase difference
-- between successive frames from the expected difference for the center
-- frequencies of the FFT bins.
type PhaseInc = V.Vector Double

-- | Time domain frame.
type Frame = V.Vector Double

-- | Sampled STFT window function.
type Window = Frame

-- | Offset between successive STFT frames, in samples.
type HopSize = Int

-- | Size in samples.
type Length = Int

-- | STFT processing unit.
type STFTFrame = (Moduli,PhaseInc)

-- | Frequency domain frame.
type FFTOutput = V.Vector (Complex Double)

-- | Type of FFT plans for real signals.
type FFTPlan = FFTp.Plan Double (Complex Double)

-- | Type of IFFT plans for real signals.
type IFFTPlan = FFTp.Plan (Complex Double) Double

-- | Configuration parameters for the phase vocoder algorithm.
data VocoderParams = VocoderParams{
    -- | FFT plan used in analysis stage.
    vocFFTPlan  :: FFTPlan,
    -- | FFT plan used in synthesis stage.
    vocIFFTPlan :: IFFTPlan,
    -- | STFT hop size.
    vocHopSize :: HopSize,
    -- | Window function used during analysis and synthesis.
    vocWindow :: Window
    -- TODO thread safety?
}

-- | FFT frame length. Can be larger than `vocInputFrameLength` for zero-padding.
vocFrameLength :: VocoderParams -> Length
vocFrameLength par = planInputSize $ vocFFTPlan par

-- | STFT frame length.
vocInputFrameLength :: VocoderParams -> Length
vocInputFrameLength par = V.length $ vocWindow par

-- | Create a vocoder configuration.
vocoderParams :: Length -> HopSize -> Window -> VocoderParams
vocoderParams len hs wnd = VocoderParams (plan dftR2C len) (plan dftC2R len) hs wnd

-- | Apply a window function on a time domain frame.
applyWindow :: Window -> Frame -> Frame
applyWindow = V.zipWith (*)

-- | Change the vector indexing so that the sample at the middle has the number 0.
-- This is done so that the FFT of the window has zero phase, and therefore does not
-- introduce phase shifts in the signal.
rewind :: (V.Storable a) => V.Vector a -> V.Vector a
rewind vec = uncurry (V.++) $ swap $ V.splitAt (V.length vec `div` 2) vec

-- | Zero-pad the signal symmetrically from both sides.
addZeroPadding :: Length
    -> Frame
    -> Frame
addZeroPadding len v
    | diff < 0  = error $ "addZeroPadding: input is " ++ (show diff) ++ " samples longer than target length"
    | diff == 0 = v
    | otherwise = res
    where
    l = V.length v
    diff = len - l
    halfdiff = diff - (diff `div` 2)
    res = (V.++) ((V.++) (V.replicate halfdiff 0) v) (V.replicate (diff-halfdiff) 0)

-- | Perform FFT processing, which includes the actual FFT, rewinding, zero-paddding
-- and windowing.
doFFT :: VocoderParams -> Frame -> FFTOutput
doFFT par =
    FFT.execute (vocFFTPlan par) . rewind . addZeroPadding (vocFrameLength par) . applyWindow (vocWindow par)

-- | Perform analysis on a sequence of frames. This consists of FFT processing
-- and performing analysis on frequency domain frames.
analysisStage :: Traversable t => VocoderParams -> Phase -> t Frame -> (Phase, t STFTFrame)
analysisStage par ph = mapAccumL (analysisBlock par) ph

-- | Perform FFT transform and frequency-domain analysis.
analysisBlock :: VocoderParams -> Phase -> Frame -> (Phase, STFTFrame)
analysisBlock par prev_ph vec = analysisStep (vocHopSize par) (vocFrameLength par) prev_ph (doFFT par vec)

-- | Analyze a frequency domain frame. Phase from a previous frame must be supplied.
-- It returns the phase of the analyzed frame and the result.
analysisStep :: HopSize -> Length -> Phase -> FFTOutput -> (Phase, STFTFrame)
analysisStep h eN prev_ph vec =
    (ph,(mag,ph_inc))
    where
    ph = V.map phase vec
    mag = V.map magnitude vec
    ph_inc = V.imap (calcPhaseInc eN h) $ V.zipWith (-) ph prev_ph

-- | Wraps an angle (in radians) to the range [-pi : pi].
wrap :: Double -> Double
wrap e = (e+pi) `mod'` (2*pi) - pi

calcPhaseInc :: Length -> HopSize -> Int -> Double -> Double
calcPhaseInc eN hop k ph_diff =
    (omega + wrap (ph_diff - omega)) / fromIntegral hop
    where
    omega = (2*pi*fromIntegral k*fromIntegral hop) / fromIntegral eN

-- | Perform synthesis on a sequence of frames. This consists of performing
-- synthesis and IFFT processing.
synthesisStage :: Traversable t => VocoderParams -> Phase -> t STFTFrame -> (Phase, t Frame)
synthesisStage par ph frs = mapAccumL (synthesisBlock par) ph frs

-- | Perform frequency-domain synthesis and IFFT transform.
synthesisBlock :: VocoderParams -> Phase -> STFTFrame -> (Phase, Frame)
synthesisBlock par ph fr = (id *** doIFFT par) $ synthesisStep (vocHopSize par) ph fr

-- | Synthesize a frequency domain frame. Phase from the previously synthesized frame
-- must be supplied. It returns the phase of the synthesized frame and the result.
synthesisStep :: HopSize -> Phase -> STFTFrame -> (Phase, FFTOutput)
synthesisStep hop ph (mag, ph_inc) =
    (new_ph, V.zipWith mkPolar mag new_ph)
    where
    new_ph = V.zipWith (+) ph $ V.map (* fromIntegral hop) ph_inc

-- | Perform IFFT processing, which includes the actual IFFT, rewinding, removing padding
-- and windowing.
doIFFT :: VocoderParams -> FFTOutput -> Frame
doIFFT par =
    applyWindow (vocWindow par) . cutCenter (vocInputFrameLength par) . rewind . FFT.execute (vocIFFTPlan par)

-- | Cut the center of a time domain frame, discarding zero padding.
cutCenter :: (V.Storable a) => Length -> V.Vector a -> V.Vector a
cutCenter len vec = V.take len $ V.drop ((V.length vec - len) `div` 2) vec

-- | Zero phase for a given vocoder configuration.
-- Can be used to initialize the synthesis stage.
zeroPhase :: VocoderParams -> Phase
zeroPhase par = V.replicate (planOutputSize $ vocFFTPlan par) 0

-- | An amplitude change coefficient for the processing pipeline.
-- Can be used to ensure that the output has the same volume as the input.
volumeCoeff :: VocoderParams -> Double
volumeCoeff par = fromIntegral (vocHopSize par) / V.sum (V.map (**2) $ vocWindow par)

