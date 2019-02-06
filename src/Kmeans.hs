module Kmeans 
(
    kmeans
)
where

import System.Random
import Data.Vector (minimum)

import Permutation (permutate)

import Data.Array.Repa (traverse, foldP, slice, All, Any, Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!), (-^)(..))
import qualified Data.Array.Repa as R

kmeans :: Array U DIM2 RGB8 -> Int -> IO [Array U DIM2 RGB8]
kmeans pixels clusters = do
    clusters <- guessClusters clusters pixels
    return $ iteration (iterateKmeans (map mean clusters) pixels) pixels

iterateKmeans :: [RGB8] -> Array U DIM2 RGB8 -> [RGB8]
iterateKmeans centroids pixels
    | newCentroids == centroids = centroids
    | otherwise = iterateKmeans newCentroids pixels
    where 
        newCentroids = map mean $ iteration centoids pixels 

guessClusters :: Int -> Array U DIM2 RGB8 -> IO [Array U DIM2 RGB8]
guessClusters clusters pixels = do
    gen <- getStdGen
    let rs = map (\s -> take clusters $ randomRs (0, s) gen) 
        $ R.Shape.toList $ extent pixels
    let colours = map (\(x,y) -> x ! (Z :. x :. y)) $ zip rs[0] rs[1]
    return $ iteration colours pixels 
    
iteration :: [RGB8] -> Array U DIM2 RGB8 -> [Array U DIM2 RGB8]
iteration centroids pixels = 
        R.map (\(pixel dist) -> if dist < midpoint then pixel else zeroPixel) 
            $ map (R.zipWith (,) pixels) $ pairwiseDistanceMin centroids pixels
    where 
        midpoint = meanList centroids
   
meanList :: [RGB8] -> RGB8
meanList xs = 
        (fst c / size xs, snd c / size xs, thd c / size xs)
    where 
        c = foldr1 (\acc new -> (fst acc + fst new, 
                snd acc + snd new, thd acc + thd new)) xs 
    
pairwiseDistanceMin :: [RGB8] -> Array U DIM2 RGB8 -> [Array U DIM2 RGB8]
pairwiseDistanceMin centroids pixels = 
    map (\c -> R.map (distance c)) centroids

mean :: (R.Shape sh) => Array U sh RGB8 -> RGB8
mean arr = 
        (fst c / s, snd c / s, thd c / s)
    where
       c = (foldP (\a n -> (fst a + fst n, snd a + snd n, thd a + thd n)) 
                zeroPixel arr) 
       s = R.size $ extent arr 

zeroPixel :: RGB8
zeroPixel  = (0::Pixel8, 0::Pixel8, 0::Pixel8)

distance :: RGB8 -> RGB8 -> Double 
distance op np = sqrt 
    $ foldr (\acc fn -> acc + ((fn op) - (fn np)) ** 2) (0::Double) [fst, snd, thd]

fst (x, _, _) = x
snd (_, x, _) = x
thd (_, _, x) = x 

type RGB8 = (Pixel8, Pixel8, Pixel8)
