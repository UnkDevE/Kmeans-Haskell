module Kmeans 
(
    kmeans
)
where

import System.Random
import Data.Vector (minimum)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V

import Data.Array.Repa (foldS, slice, All, Any, Array, DIM1, DIM2, U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as R.Shape

kmeans :: Array U DIM2 RGB8 -> Int -> IO [Array U DIM2 RGB8]
kmeans pixels clusters = do
    clusters <- guessClusters clusters pixels
    return $ iteration (iterateKmeans (map mean clusters) pixels) pixels

iterateKmeans :: [RGB8] -> Array U DIM2 RGB8 -> [RGB8]
iterateKmeans centroids pixels
    | newCentroids == centroids = centroids
    | otherwise = iterateKmeans newCentroids pixels
    where 
        newCentroids = map mean $ iteration centroids pixels 

guessClusters :: Int -> Array U DIM2 RGB8 -> IO [Array U DIM2 RGB8]
guessClusters clusters pixels = do
    gen <- getStdGen
    let rs = map (\s -> take clusters $ randomRs (0, s) gen) $ R.Shape.listOfShape $ R.extent pixels
    let colours = map (\(x,y) -> pixels R.! (Z :. x :. y)) $ zip (rs !! 0) (rs !! 1)
    return $ iteration colours pixels 
    
iteration :: [RGB8] -> Array U DIM2 RGB8 -> [Array U DIM2 RGB8]
iteration centroids pixels = 
    map (R.computeUnboxedS . R.map (\(pixel, (mid, dist)) -> if dist < mid then pixel else zeroPixel))
        (map ( R.computeUnboxedS . R.zipWith (,) pixels . R.zipWith (,) midDist) 
            $ pairwiseDistance centroids pixels)
    where 
        midDist = midpointDistance centroids pixels
   
meanList :: [RGB8] -> RGB8
meanList xs = 
        (first c / s, secnd c / s, thd c / s)
    where 
        c = foldr1 (\acc new -> (first acc + first new, 
                secnd acc + secnd new, thd acc + thd new)) xs 
        s = fromIntegral $ length xs
    
pairwiseDistance :: [RGB8] -> Array U DIM2 RGB8 -> [Array U DIM2 Word8]
pairwiseDistance centroids pixels = 
    map (\c -> R.computeUnboxedS $ R.map (distance c) pixels) centroids

midpointDistance :: [RGB8] -> Array U DIM2 RGB8 -> Array U DIM2 Word8
midpointDistance centroids pixels = 
    R.computeUnboxedS $ R.map (distance m) pixels 
    where m = meanList centroids 

mean :: Array U DIM2 RGB8 -> RGB8
mean arr = 
        (first c / s , secnd c / s, thd c / s)
    where
       c = (V.foldl (\a n -> (first a + first n, secnd a + secnd n, thd a + thd n)) 
                zeroPixel $ R.toUnboxed arr) 
       s = fromIntegral $ R.size $ R.extent arr 

zeroPixel :: RGB8
zeroPixel = (0::Word8, 0::Word8, 0::Word8)

distance :: RGB8 -> RGB8 -> Word8
distance op np = sqrt 
    $ foldr (\fn acc -> acc + ((fn op) - (fn np)) ** 2) (0) [first, secnd, thd]

first (x, _, _) = x
secnd (_, x, _) = x
thd (_, _, x) = x 

type RGB8 = (Word8, Word8, Word8)
