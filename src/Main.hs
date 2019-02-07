module Main where

import Kmeans

import Codec.Picture

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
      let (r,g,b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b

main :: IO ()
main = do
    [path, _] <- getArgs
    eimg <- readImage path 
    case eimg of 
        Left err <- putStrLn "could not open image"
        Right (ImageRGB8 img) -> do
            clusters <- kmeans (fromImage img) 16
            map (\(c, i) -> savePngImage (show i) $ toImage c) $ zip clusters [1..]
            putStrLn "operation complete"
