{-# LANGUAGE RecordWildCards #-}
module Main where

import Kmeans

import System.Environment (getArgs)

import Codec.Picture
import Data.Array.Repa (foldS, slice, All, Any, Array, DIM1, DIM2, U, D, Z (..), (:.)(..))
import qualified Data.Array.Repa as R

-- | Produce delayed Repa array from image with true color pixels.
fromImage :: Image PixelRGB8 -> Array D DIM2 RGB8
fromImage img@Image {..} =
  R.fromFunction
    (Z :. imageWidth :. imageHeight)
    (\(Z :. x :. y) ->
       let (PixelRGB8 r g b) = pixelAt img x y
       in (r, g, b))

-- | Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (r,g,b) = a R.! (Z :. x :. y)
      in PixelRGB8 r g b

main :: IO ()
main = do
    [path, path_] <- getArgs
    eimg <- readImage path 
    case eimg of 
        Left err -> putStrLn "could not open image"
        Right (ImageRGB8 img) -> do
            clusters <- kmeans (R.computeUnboxedS $ fromImage img) 3
            savePngImage path_ $ ImageRGB8 $ toImage (clusters !! 2)
            putStrLn "operation complete"
        Right _ -> do
            putStrLn "wtf is this"

