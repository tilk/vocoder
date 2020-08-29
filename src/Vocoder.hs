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
    frame_length :: Length,
    fft_window :: Window
    -- TODO thread safety?
}

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

smart_fft_with_plan :: FT_plan -> Window -> Length -> [Frame] -> [FFT_output]
smart_fft_with_plan my_plan win len aus =
    map (FFT.execute my_plan . rewind . smart_add_zeropadding len . applyWindow win) aus

analysis_stage :: Vocoder_params -> Phase -> [(HopSize, Frame)] -> (Phase,[(HopSize, SFT_block)])
analysis_stage par ph li_in =
    mapAccumL (analysis_step (frame_length par)) ph fft_res
    where
    (h_li,au_li) = unzip li_in
    fft_res :: [(HopSize,FFT_output)]
    fft_res = zip h_li $ smart_fft_with_plan (fft_plan par) (fft_window par) (frame_length par) au_li

analysis_step :: Length -> Phase -> (HopSize,FFT_output) -> (Phase,(HopSize,SFT_block))
analysis_step eN prev_ph (h,vec) =
    (ph,(h,(mag,ph_inc)))
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

synthesis_stage :: Vocoder_params -> Phase -> [(HopSize,SFT_block)] -> (Phase,[(HopSize,Frame)])
synthesis_stage param ph hop_and_block_list =
    (id ***back_to_time_domain)
    $ mapAccumL (synthesis_step) ph hop_and_block_list
    where
    back_to_time_domain :: [(HopSize,FFT_output)] -> [(HopSize,V.Vector Double)]
    back_to_time_domain = uncurry zip . (id***smart_ifft_with_plan (ifft_plan param) (fft_window param) (frame_length param)) . unzip

synthesis_step :: Phase -> (HopSize,SFT_block) -> (Phase,(HopSize,FFT_output))
synthesis_step ph (hop,(mag,ph_inc)) =
    (new_ph,(hop,V.zipWith mkPolar mag new_ph))
    where
    new_ph = V.zipWith (+) ph $ V.map (* fromIntegral hop) ph_inc

smart_ifft_with_plan :: IFT_plan -> Window -> Length -> [FFT_output] -> [V.Vector Double]
smart_ifft_with_plan my_plan win len input =
    map (applyWindow win . cut_center len . rewind . FFT.execute my_plan) input

cut_center :: (V.Storable a) => Length -> V.Vector a -> V.Vector a
cut_center len vec = V.take len $ V.drop (floor $ (fromIntegral $ V.length vec - len)/2) vec

