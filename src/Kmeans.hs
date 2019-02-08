module Kmeans 
(
    kmeans,
    RGB8(..)
)
where

import System.Random
import Data.Vector (minimum)
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V

import Data.Array.Repa (foldS, slice, All, Any, Array, DIM1, DIM2, U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as R.Shape
import Debug.Trace

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
    let shape = R.Shape.listOfShape $ R.extent pixels 
        in do 
        getStdGen >>= 
            \gen -> let xs = take clusters $ randomRs (0, shape !! 0) gen 
                in do 
                getStdGen >>= 
                    \gen1 -> let ys = take clusters $ randomRs (0, shape !! 1) gen1 
                        in do 
                            let colours = (map (\(x,y) -> pixels R.! (Z :. x :. y)) $ zip xs ys)
                                in do return $ iteration colours pixels


    
iteration :: [RGB8] -> Array U DIM2 RGB8 -> [Array U DIM2 RGB8]
iteration centroids pixels = 
        map (getCluster zippedPixels) [0..((length centroids) - 1)]
    where 
         zippedPixels = R.computeUnboxedS $ 
            R.zipWith (,) pixels $ pairwiseDistance centroids pixels

getCluster :: Array U DIM2 (RGB8, Int) -> Int -> Array U DIM2 RGB8
getCluster zippixs n = 
    R.computeUnboxedS $ R.map (\(pixel, cluster) -> 
        if cluster == n then 
            pixel
        else
            zeroPixel
    ) zippixs
   
pairwiseDistance :: [RGB8] -> Array U DIM2 RGB8 -> Array U DIM2 Int
pairwiseDistance centroids pixels = 
   R.computeUnboxedS $ R.map (\pixel -> 
        fst $ foldr1 (\(center, dist) (ncenter, ndist) -> 
                if ndist < dist then 
                    (ncenter, ndist)
                else
                    (center, dist)
                )
            $ zip [0..] $ map ((flip distance) pixel) centroids) pixels 

mean :: Array U DIM2 RGB8 -> RGB8
mean arr = 
        (truncate (first c / s), 
        truncate (secnd c / s), 
        truncate (thd c / s))
    where
       c = (V.foldl (\a n -> (first a + first n, secnd a + secnd n, thd a + thd n)) 
                zeroPixel $ R.toUnboxed arr) 
       s = fromIntegral $ R.size $ R.extent arr 

zeroPixel :: RGB8
zeroPixel = (0::Word8, 0::Word8, 0::Word8)

distance :: RGB8 -> RGB8 -> Double
distance op np = sqrt 
    $ foldr (\fn acc -> acc + ((fn op) - (fn np)) ** 2) (0) [first, secnd, thd]

first (x, _, _) = fromIntegral x
secnd (_, x, _) = fromIntegral x
thd (_, _, x) = fromIntegral x 

type RGB8 = (Word8, Word8, Word8)
