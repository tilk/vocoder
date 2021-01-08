import Gauge.Main
import qualified Data.Vector.Storable as V
import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import Vocoder.Conduit.Frames

benchFramesOfE :: Int -> Int -> Int -> Int -> Benchmarkable
benchFramesOfE inputChunkSize chunkSize hopSize size0 = flip whnf size0 $ \size ->
        C.runConduitPure
      $ CC.enumFromTo 1 size
     .| CC.map (V.replicate inputChunkSize)
     .| framesOfE chunkSize hopSize
     .| CC.sumE

benchSumFramesE :: Int -> Int -> Int -> Int -> Benchmarkable
benchSumFramesE inputChunkSize chunkSize hopSize size0 = flip whnf size0 $ \size ->
        C.runConduitPure
      $ CC.enumFromTo 1 size
     .| CC.map (V.replicate inputChunkSize)
     .| sumFramesE chunkSize hopSize
     .| CC.sumE

main :: IO ()
main = defaultMain 
    [ bench "framesOfE" $ benchFramesOfE 100 512 21 size0
    , bench "sumFramesE" $ benchSumFramesE 512 100 21 size0
    ]
    where
    size0 = 1000 :: Int
    

