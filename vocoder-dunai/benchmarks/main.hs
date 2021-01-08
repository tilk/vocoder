{-# LANGUAGE TypeApplications #-}

import Gauge.Main
import qualified Data.Vector.Storable as V
import Data.Functor.Identity (runIdentity)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore(unMSF)
import Vocoder.Dunai

reactimateN :: Monad m => Int -> MSF m () a -> m a
reactimateN 0 msf = fst <$> unMSF msf ()
reactimateN n msf = unMSF msf () >>= reactimateN (n-1) . snd

benchFramesOfS :: Int -> Int -> Int -> Int -> Benchmarkable
benchFramesOfS inputChunkSize chunkSize hopSize size0 = flip whnf size0 $ \size ->
    runIdentity $ reactimateN size
        $ count
      >>> arr (V.replicate @Int inputChunkSize)
      >>> framesOfS chunkSize hopSize
      >>> arr (sum . fmap V.sum)
      >>> accumulateWith (+) 0
    
benchSumFramesS :: Int -> Int -> Int -> Int -> Benchmarkable
benchSumFramesS inputChunkSize chunkSize hopSize size0 = flip whnf size0 $ \size ->
    runIdentity $ reactimateN (size `div` k)
        $ count
      >>> arr (replicate k . V.replicate @Int inputChunkSize)
      >>> sumFramesS chunkSize hopSize
      >>> arr V.sum
      >>> accumulateWith (+) 0
    where k = chunkSize `div` hopSize

main :: IO ()
main = defaultMain
    [ bench "framesOfS" $ benchFramesOfS 128 512 32 size0
    , bench "sumFramesS" $ benchSumFramesS 512 128 32 size0
    ]
    where
    size0 = 1000 :: Int

