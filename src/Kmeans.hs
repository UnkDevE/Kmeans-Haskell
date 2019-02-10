module Kmeans 
(
    kmeans,
    RGB8(..)
)
where

import System.Random
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as V

import Data.Array.Repa (foldS, slice, All, Any, Array, DIM1, DIM2, U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Shape as R.Shape
import Debug.Trace

kmeans :: Array U DIM2 RGB8 -> Int -> IO [Array U DIM2 RGB8]
kmeans pixels clusters = do
    let pix = R.toUnboxed pixels
    clusters <- guessClusters clusters pixels
    let vecs = iteration (iterateKmeans (map mean clusters) pix) pix
    return $ map (\v -> R.fromUnboxed (R.extent pixels) v) vecs

iterateKmeans :: [RGB8] -> V.Vector RGB8 -> [RGB8]
iterateKmeans centroids pixels
    | newCentroids == centroids = trace (show centroids) centroids
    | otherwise = iterateKmeans (trace (show centroids) newCentroids) pixels
    where newCentroids = map mean $ iteration centroids pixels 

guessClusters :: Int -> Array U DIM2 RGB8 -> IO [V.Vector RGB8]
guessClusters clusters pixels = do 
    let shape = R.Shape.listOfShape $ R.extent pixels  
    xs <- randList clusters (0, 255) 
    ys <- randList clusters (0, 255)
    zs <- randList clusters (0, 255)
    let colours = map (\(a,b,c) -> (fromIntegral a, fromIntegral b, fromIntegral c)) $ zip3 xs ys zs
    return $ iteration colours (R.toUnboxed pixels)

randList :: Int -> (Int, Int) -> IO ([Int])
randList 0 range = return []
randList n range = do 
    rand <- randomRIO range 
    rs <- randList (n-1) range 
    return (rand:rs)
    
iteration :: [RGB8] -> V.Vector RGB8 -> [V.Vector RGB8]
iteration centroids pixels = 
        map (getCluster zippedPixels) [0..((length centroids) - 1)]
    where 
         zippedPixels = 
            V.zip pixels $ pairwiseDistance centroids pixels

getCluster :: V.Vector (RGB8, Int) -> Int -> V.Vector RGB8
getCluster zippixs n = 
    V.map (\(pixel, cluster) -> 
        if cluster == n then 
            pixel
        else 
            zeroPixel
     ) zippixs
   
pairwiseDistance :: [RGB8] -> V.Vector RGB8 -> V.Vector Int
pairwiseDistance centroids pixels = 
  V.map (\pixel -> snd $ minimum
    $ zip (map ((flip distance) pixel) centroids) [0..]) pixels

mean :: V.Vector RGB8 -> RGB8
mean arr = 
        (ceiling (first c / s), 
        ceiling (secnd c / s), 
        ceiling (thd c / s))
    where
       c = (V.foldl (\a n -> 
            (first a + (fromIntegral $ first n), secnd a + (fromIntegral $ secnd n), 
                thd a + (fromIntegral $ thd n))) 
           (0,0,0) arr)
       s = fromIntegral $ V.length arr 

zeroPixel :: RGB8
zeroPixel = (0::Word8, 0::Word8, 0::Word8)

distance :: RGB8 -> RGB8 -> Double
distance op np = sqrt 
    $ foldr (\fn acc -> acc + ((fn op) - (fn np)) ^ 2) (0) $ map ((.) fromIntegral) [first, secnd, thd]

first (x, _, _) = x
secnd (_, x, _) = x
thd (_, _, x) = x 

type RGB8 = (Word8, Word8, Word8)
