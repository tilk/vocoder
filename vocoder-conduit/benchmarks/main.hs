import Gauge.Main
import qualified Data.Vector.Storable as V
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import Vocoder.Conduit.Utils

benchFramesOfE :: Int -> Int -> Int -> Int -> Benchmarkable
benchFramesOfE inputChunkSize chunkSize hopSize size0 = flip whnf size0 $ \size ->
        C.runConduitPure
      $ CC.enumFromTo 1 size
     .| CC.map (\x -> V.replicate inputChunkSize x)
     .| framesOfE chunkSize hopSize
     .| CC.sumE

benchSumFramesWithE :: Int -> Int -> Int -> Int -> Benchmarkable
benchSumFramesWithE inputChunkSize chunkSize hopSize size0 = flip whnf size0 $ \size ->
        C.runConduitPure
      $ CC.enumFromTo 1 size
     .| CC.map (\x -> V.replicate inputChunkSize x)
     .| sumFramesWithE chunkSize hopSize
     .| CC.sumE

main :: IO ()
main = defaultMain 
    [ bench "listFramesOfE" $ benchFramesOfE 100 512 21 size0
    , bench "sumFramesWithE" $ benchSumFramesWithE 512 100 21 size0
    ]
    where
    size0 = 10000 :: Int
    
