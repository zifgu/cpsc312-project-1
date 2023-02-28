module Main (main, combineImages) where

import Image
import ImageReadWrite
import qualified Graphics.Image as HIP
import System.Directory

-- ****************************************************************
-- Command line tool
-- ****************************************************************

-- readImageFromString pathOrUrl    tries to read an input image from path, or if it is not a local path, URL; returns either an error string or the image
readImageFromString :: String -> IO (Either String RGBImage)
readImageFromString pathOrUrl = do
    isPath <- doesFileExist pathOrUrl
    putStr $ "Reading '" ++ pathOrUrl
    if isPath
    then do
        putStrLn "' (file)..."
        result <- readImageFromFile pathOrUrl
        return result
    else do
        putStrLn "' (URL)..."
        result <- readImageFromUrl pathOrUrl
        return result

-- getAllImages str                 tries to read all images in strs, and returns either an array with all of them or Nothing
getAllImages :: [String] -> IO (Maybe [RGBImage])
getAllImages strs = getAllImagesHelper strs []
    where
        getAllImagesHelper :: [String] -> [RGBImage] -> IO (Maybe [RGBImage])
        getAllImagesHelper [] images = do
            return $ Just images
        getAllImagesHelper (str:t) images = do
            result <- readImageFromString str
            case result of
                (Left err) -> do
                    putStrLn $ "Error reading '" ++ str ++ "':"
                    putStrLn err
                    return Nothing
                (Right image) -> do
                    putStrLn "Successfully read.\n"
                    getAllImagesHelper t (images ++ [image])

-- checkSameSize ims                checks whether all images in ims have the same width and height
checkSameSize :: [RGBImage] -> Bool
checkSameSize [] = True
checkSameSize [im] = True
checkSameSize (a:b:c) = sameSize && checkSameSize (b:c)
    where sameSize = (HIP.dims a) == (HIP.dims b)

-- Command-line tool for image blending.
--      im1Str - string (path or URL) for image 1
--      im2Str - string (path or URL) for image 2
--      maskStr - string (path or URL) for mask image
--      sigma - sigma parameter of image blending
--      iterations - iterations parameter of image blending
-- All three input images must be the same size.
-- Larger sigma/iterations take longer to run but should produce better-blended results.
combineImages :: String -> String -> String -> Double -> Int -> IO ()
combineImages im1Str im2Str maskStr sigma iterations = do
    images <- getAllImages [im1Str,im2Str,maskStr]
    case images of
        Nothing -> putStrLn "Please try again."
        (Just ims@[im1, im2, mask]) -> do
            if checkSameSize ims
            then do 
                let combinedImage = imageBlend im1 im2 mask sigma iterations
                putStrLn "Now blending! Please wait for the result (may take several minutes or more)..."
                HIP.displayImage combinedImage
            else
                putStrLn "Error: images must be the same size. Please try again."

-- Dummy main action. Don't call this.
main :: IO ()
main = do return ()