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
      frameLength,
      inputFrameLength,
      hopSize,
      fftWindow,
      doFFT,
      doIFFT,
      analysisStep,
      analysisStage,
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
    fftPlan  :: FFTPlan,
    -- | FFT plan used in synthesis stage.
    ifftPlan :: IFFTPlan,
    -- | STFT hop size.
    hopSize :: HopSize,
    -- | Window function used during analysis and synthesis.
    fftWindow :: Window
    -- TODO thread safety?
}

-- | FFT frame length. Can be larger than `inputFrameLength` for zero-padding.
frameLength :: VocoderParams -> Length
frameLength par = planInputSize $ fftPlan par

-- | STFT frame length.
inputFrameLength :: VocoderParams -> Length
inputFrameLength par = V.length $ fftWindow par

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
rewind vec = uncurry (V.++) $ swap $ V.splitAt (floor $ (fromIntegral $ V.length vec) /2) vec

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
    halfdiff = diff - (floor $ fromIntegral diff / 2)
    res = (V.++) ((V.++) (V.replicate halfdiff 0) v) (V.replicate (diff-halfdiff) 0)

-- | Perform FFT processing, which includes the actual FFT, rewinding, zero-paddding
-- and windowing.
doFFT :: VocoderParams -> Frame -> FFTOutput
doFFT par =
    FFT.execute (fftPlan par) . rewind . addZeroPadding (frameLength par) . applyWindow (fftWindow par)

-- | Perform analysis on a sequence of frames. This consists of FFT processing
-- and performing analysis on frequency domain frames.
analysisStage :: VocoderParams -> Phase -> [Frame] -> (Phase, [STFTFrame])
analysisStage par ph = mapAccumL (analysisStep (hopSize par) (frameLength par)) ph .  map (doFFT par)

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
synthesisStage :: VocoderParams -> Phase -> [STFTFrame] -> (Phase,[Frame])
synthesisStage par ph hop_and_block_list =
    (id *** map (doIFFT par)) $ mapAccumL (synthesisStep (hopSize par)) ph hop_and_block_list

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
    applyWindow (fftWindow par) . cutCenter (inputFrameLength par) . rewind . FFT.execute (ifftPlan par)

-- | Cut the center of a time domain frame, discarding zero padding.
cutCenter :: (V.Storable a) => Length -> V.Vector a -> V.Vector a
cutCenter len vec = V.take len $ V.drop (floor $ (fromIntegral $ V.length vec - len)/2) vec

-- | Zero phase for a given vocoder configuration.
-- Can be used to initialize the synthesis stage.
zeroPhase :: VocoderParams -> Phase
zeroPhase par = V.replicate (planOutputSize $ fftPlan par) 0

-- | An amplitude change coefficient for the processing pipeline.
-- Can be used to ensure that the output has the same volume as the input.
volumeCoeff :: VocoderParams -> Double
volumeCoeff par = fromIntegral (hopSize par) / V.sum (V.map (**2) $ fftWindow par)

