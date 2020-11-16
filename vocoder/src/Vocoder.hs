module Vocoder where

import Data.List
import Data.Complex
import Data.Fixed
import Data.Tuple
import Control.Arrow
import Numeric.FFT.Vector.Invertible as FFT
import Numeric.FFT.Vector.Plan as FFTp
import qualified Data.Vector.Storable as V

type Moduli = V.Vector Double
type Phase = V.Vector Double
type Phase_inc = V.Vector Double
type Frame = V.Vector Double
type Window = Frame
type HopSize = Int
type Length = Int
type SFT_block = (Moduli,Phase_inc)
type FFT_output = V.Vector (Complex Double)
type FT_plan = FFTp.Plan Double (Complex Double)
type IFT_plan = FFTp.Plan (Complex Double) Double

data Vocoder_params = Vocoder_params{
    fft_plan  :: FT_plan,
    ifft_plan :: IFT_plan,
    hop_size :: HopSize,
    fft_window :: Window
    -- TODO thread safety?
}

frame_length :: Vocoder_params -> Length
frame_length par = planInputSize $ fft_plan par

input_frame_length :: Vocoder_params -> Length
input_frame_length par = V.length $ fft_window par

make_vocoder_params :: Length -> HopSize -> Window -> Vocoder_params
make_vocoder_params len hs wnd = Vocoder_params (plan dftR2C len) (plan dftC2R len) hs wnd

applyWindow :: Window -> Frame -> Frame
applyWindow = V.zipWith (*)

rewind :: (V.Storable a) => V.Vector a -> V.Vector a
rewind vec = uncurry (V.++) $ swap $ V.splitAt (floor $ (fromIntegral $ V.length vec) /2) vec

smart_add_zeropadding :: Length
    -> Frame
    -> Frame
smart_add_zeropadding len v
    | diff < 0  = error $ "smart_add_zeropadding: input is " ++ (show diff) ++ " samples longer than target length"
    | diff == 0 = v
    | otherwise = res
    where
    l = V.length v
    diff = len - l
    większe_pół = diff - (floor $ fromIntegral diff / 2)
    res = (V.++) ((V.++) (V.replicate większe_pół 0) v) (V.replicate (diff-większe_pół) 0)

smart_fft_with_plan :: Vocoder_params -> Frame -> FFT_output
smart_fft_with_plan par =
    FFT.execute (fft_plan par) . rewind . smart_add_zeropadding (frame_length par) . applyWindow (fft_window par)

analysis_stage :: Vocoder_params -> Phase -> [Frame] -> (Phase, [SFT_block])
analysis_stage par ph = mapAccumL (analysis_step (hop_size par) (frame_length par)) ph .  map (smart_fft_with_plan par)

analysis_step :: HopSize -> Length -> Phase -> FFT_output -> (Phase,SFT_block)
analysis_step h eN prev_ph vec =
    (ph,(mag,ph_inc))
    where
    ph = V.map phase vec
    mag = V.map magnitude vec
    ph_inc = V.imap (calc_phase_inc eN h) $ V.zipWith (-) ph prev_ph

wrap :: Double -> Double
wrap e = (e+pi) `mod'` (2*pi) - pi

calc_phase_inc :: Length -> HopSize -> Int -> Double -> Double
calc_phase_inc eN hop k ph_diff =
    (omega + wrap (ph_diff - omega)) / fromIntegral hop
    where
    omega = (2*pi*fromIntegral k*fromIntegral hop) / fromIntegral eN

synthesis_stage :: Vocoder_params -> Phase -> [SFT_block] -> (Phase,[Frame])
synthesis_stage par ph hop_and_block_list =
    (id *** back_to_time_domain)
    $ mapAccumL (synthesis_step (hop_size par)) ph hop_and_block_list
    where
    back_to_time_domain :: [FFT_output] -> [V.Vector Double]
    back_to_time_domain = map (smart_ifft_with_plan par)

synthesis_step :: HopSize -> Phase -> SFT_block -> (Phase, FFT_output)
synthesis_step hop ph (mag, ph_inc) =
    (new_ph, V.zipWith mkPolar mag new_ph)
    where
    new_ph = V.zipWith (+) ph $ V.map (* fromIntegral hop) ph_inc

smart_ifft_with_plan :: Vocoder_params -> FFT_output -> Frame
smart_ifft_with_plan par =
    applyWindow (fft_window par) . cut_center (frame_length par) . rewind . FFT.execute (ifft_plan par)

cut_center :: (V.Storable a) => Length -> V.Vector a -> V.Vector a
cut_center len vec = V.take len $ V.drop (floor $ (fromIntegral $ V.length vec - len)/2) vec

zero_phase :: Vocoder_params -> Phase
zero_phase par = V.replicate (planOutputSize $ fft_plan par) 0

