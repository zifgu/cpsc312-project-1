{-# LANGUAGE FlexibleContexts #-}

module Main (main, combineImages) where

import Image
import ImageReadWrite
import Graphics.Image
import System.Directory

readImageFromString :: String -> IO (Either String RGBImage)
readImageFromString pathOrUrl = do
    isPath <- doesFileExist pathOrUrl
    putStr $ "Reading " ++ pathOrUrl
    if isPath
    then do
        putStrLn " (file):"
        result <- readImageFromFile pathOrUrl
        return (Right result)
    else do
        putStrLn " (URL):"
        result <- readImageFromUrl pathOrUrl
        return result

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
                    putStrLn "Successfully read."
                    getAllImagesHelper t (images ++ [image])

combineImages :: String -> String -> String -> Double -> Int -> IO ()
combineImages im1Str im2Str maskStr sigma iterations = do
    images <- getAllImages [im1Str,im2Str,maskStr]
    case images of
        Nothing -> putStrLn "Sorry, please try again."
        (Just [im1, im2, mask]) -> do
            putStrLn "Insert the name of the combined image."
            combinedImageName <- getLine
            let combinedImagePath = combinedImageName ++ ".png"

            let combinedImage = imageBlend im1 im2 mask sigma iterations

            putStrLn "Done blending! Saving result..."
            displayImage combinedImage

            putStrLn "Congratulations on merging your images! To see the result go to runtime-images folder."

main :: IO ()
main = do return ()