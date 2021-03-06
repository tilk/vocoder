{-| 
    Module      : Vocoder.Window
    Description : Window functions
    Copyright   : (c) Marek Materzok, 2021
    License     : BSD2

This module defines popular window functions for use in the vocoder
framework.
-}
module Vocoder.Window (
    makeWindow,
    boxWindow,
    triangleWindow,
    hammingWindow,
    hannWindow,
    generalizedBlackmanWindow,
    blackmanWindow,
    exactBlackmanWindow,
    lanczosWindow,
    flatTopWindow
    ) where

import Vocoder
import qualified Data.Vector.Storable as V

-- | Creates a window of given length by sampling a function
--   on the interval [0,1].
makeWindow :: (Double -> Double) -> Length -> Window
makeWindow f n = V.generate n $ \k -> f (fromIntegral k / (fromIntegral n - 1))

-- | Creates a box window.
boxWindow :: Length -> Window
boxWindow = makeWindow $ const 1

-- | Creates a triangular window.
triangleWindow :: Length -> Window
triangleWindow = makeWindow $ \x -> 2 * (0.5 - abs (x - 0.5))

-- | Creates a Hamming window.
hammingWindow :: Length -> Window
hammingWindow = makeWindow $ \x -> alpha - beta * cos (2 * pi * x)
    where
    alpha = 25.0/46.0
    beta = 21.0/46.0

-- | Creates a Hann window.
hannWindow :: Length -> Window
hannWindow = makeWindow $ \x -> 0.5 * (1 - cos (2 * pi * x))

-- | Creates a generalized Blackman window for a given alpha value.
generalizedBlackmanWindow :: Double -> Length -> Window
generalizedBlackmanWindow a = makeWindow $ \x -> let p = 2 * pi * x in a0 - a1 * cos p + a2 * cos (2 * p)
    where
    a0 = (1 - a) / 2
    a1 = 0.5
    a2 = a / 2

-- | Creates a Blackman window (with alpha=0.16).
blackmanWindow :: Length -> Window
blackmanWindow = generalizedBlackmanWindow 0.16

-- | Creates an exact Blackman window.
exactBlackmanWindow :: Length -> Window
exactBlackmanWindow = makeWindow $ \x -> let p = 2 * pi * x in a0 - a1 * cos p + a2 * cos (2 * p)
    where
    a0 = 7938.0/18608.0
    a1 = 9240.0/18608.0
    a2 = 1430.0/18608.0

-- | Creates a Lanczos window.
lanczosWindow :: Length -> Window
lanczosWindow = makeWindow $ \x -> sinc $ 2 * x - 1
    where
    sinc 0 = 1
    sinc x = sin (pi*x) / (pi*x)

-- | Creates a flat top window.
flatTopWindow :: Length -> Window
flatTopWindow = makeWindow $ \x -> a0 - a1 * cos (2 * pi * x) + a2 * cos (4 * pi * x) - a3 * cos (6 * pi * x) + a4 * cos (8 * pi * x)
    where
    a0 = 0.21557895
    a1 = 0.41663158
    a2 = 0.277263158
    a3 = 0.083578947
    a4 = 0.006947368

