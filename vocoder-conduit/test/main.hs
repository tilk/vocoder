{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Functor.Identity (Identity)
import Data.Conduit ((.|), ConduitT)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Vocoder.Conduit.Frames

runConduitList :: ConduitT a b Identity () -> [a] -> [b]
runConduitList c l = C.runConduitPure $ CL.sourceList l .| c .| CL.consume

equivToList :: Eq b => ([a] -> [b]) -> ConduitT a b Identity () -> [a] -> Bool
equivToList f c xs = f xs == runConduitList c xs

listFramesOfE :: Int -> Int -> [[a]] -> [[a]]
listFramesOfE chunkSize hopSize input = 
    map (\i -> take chunkSize $ drop i cInput) [0, hopSize .. length cInput - chunkSize] 
    where 
    cInput = concat input

listSumFramesE :: Int -> Int -> [[Int]] -> [[Int]]
listSumFramesE chunkSize hopSize input = map (\i -> take chunkSize $ drop i cOutput) [0, chunkSize .. lastLength]
    where 
    cOutput = foldl1 (zipWith (+)) $ zipWith (\k l -> replicate k 0 ++ l ++ repeat 0) [0, hopSize..] input
    lastLength = maximum $ -1 : zipWith (\k l -> k + length l - 1) [0, hopSize..] input

main :: IO ()
main = hspec $ do
    prop "framesOfE" $ \(Positive chunkSize) (Positive hopSize) -> equivToList (listFramesOfE @Int chunkSize hopSize) (framesOfE chunkSize hopSize)
    prop "sumFramesE" $ \(Positive chunkSize) (Positive hopSize) -> equivToList (listSumFramesE chunkSize hopSize) (sumFramesE chunkSize hopSize) . map getNonEmpty


