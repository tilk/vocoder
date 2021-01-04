{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Functor.Identity (Identity, runIdentity)
import Data.MonadicStreamFunction
import qualified Data.Vector.Storable as V
import Vocoder
import Vocoder.Rhine

shrinkOne :: (a -> [a]) -> [a] -> [[a]]
shrinkOne _   []     = []
shrinkOne shr (x:xs) = [ x':xs | x'  <- shr x ]
                    ++ [ x:xs' | xs' <- shrinkOne shr xs ]

runMSFList :: MSF Identity a b -> [a] -> [b]
runMSFList m l = runIdentity $ embed m l

--equivToList :: Eq b => ([a] -> [b]) -> MSF Identity a b -> [a] -> Bool
--equivToList f c xs = f xs == runMSFList c xs

equivToListA :: ([[Int]] -> [[Int]]) -> MSF Identity (V.Vector Int) [V.Vector Int] -> [[Int]] -> Bool
equivToListA f c xs = f xs == concat (map (map V.toList) . runMSFList c . map V.fromList $ xs)

equivToListB :: ([[Int]] -> [[Int]]) -> MSF Identity [V.Vector Int] (V.Vector Int) -> [[[Int]]] -> Bool
equivToListB f c xs = f (concat xs) == (map V.toList . runMSFList c . map (map V.fromList) $ xs)

listFramesOf :: Int -> Int -> [[Int]] -> [[Int]]
listFramesOf chunkSize hopSize input =
    map (\i -> take chunkSize $ drop i cInput) [0, hopSize .. length cInput - chunkSize]
    where
    cInput = concat input

listSumFrames :: Int -> Int -> [[Int]] -> [[Int]]
listSumFrames chunkSize hopSize input = map (\i -> take chunkSize $ drop i cOutput) [0, chunkSize .. lastLength]
    where
    cOutput = foldl1 (zipWith (+)) $ zipWith (\k l -> replicate k 0 ++ l ++ repeat 0) [0, hopSize..] input
    lastLength = maximum $ -1 : zipWith (\k l -> k + length l - 1) [0, hopSize..] input

genBlocks :: Arbitrary a => Int -> Gen [[a]]
genBlocks blockSize = resize (maximum [5, 1000 `div` blockSize]) $ listOf $ vector blockSize

shrinkBlocks :: [[Int]] -> [[[Int]]]
shrinkBlocks = shrinkList $ shrinkOne shrink

genChunks :: Arbitrary a => Int -> Int -> Gen [[[a]]]
genChunks blockM chunkSize = resize (maximum [5, 1000 `div` blockM `div` chunkSize]) $ listOf $ vectorOf blockM $ vector chunkSize

shrinkChunks :: [[[Int]]] -> [[[[Int]]]]
shrinkChunks = shrinkList $ shrinkOne $ shrinkOne shrink

main :: IO ()
main = hspec $ do
    prop "framesOfS" $ \(Positive (Small chunkM)) (Positive (Small blockM)) (Positive (Small hopSize)) -> 
        let blockSize = blockM * hopSize
            chunkSize = chunkM * hopSize
        in forAllShrink (genBlocks blockSize) shrinkBlocks $ 
            equivToListA (listFramesOf chunkSize hopSize . (replicate (chunkSize - hopSize) 0 :)) (framesOfS chunkSize hopSize)
    prop "sumFramesS" $ \(Positive (Small chunkM)) (Positive (Small blockM)) (Positive (Small hopSize)) -> 
        let blockSize = blockM * hopSize
            chunkSize = chunkM * hopSize
        in forAllShrink (genChunks blockM chunkSize) shrinkChunks $ \l ->
            equivToListB (take (length l) . listSumFrames blockSize hopSize) (sumFramesS blockSize hopSize) l


