import Image
import ImageReadWrite

printImageStats :: Image -> IO ()
printImageStats image = do
    let (Image w h _) = image
    putStrLn ("Width: " ++ show w)
    putStrLn ("Height: " ++ show h)
    putStrLn ("Pixel (0, 0): " ++ show (get image 0 0 0) ++ "," ++ show (get image 0 0 1) ++ "," ++ show (get image 0 0 2))
    putStrLn ("Pixel (0, 100): " ++ show (get image 0 100 0) ++ "," ++ show (get image 0 100 1) ++ "," ++ show (get image 0 100 2))
    putStrLn ("Pixel (60, 20): " ++ show (get image 60 20 0) ++ "," ++ show (get image 60 20 1) ++ "," ++ show (get image 60 20 2))
    putStrLn ("Pixel (100, 100): " ++ show (get image 100 100 0) ++ "," ++ show (get image 100 100 1) ++ "," ++ show (get image 100 100 2))

-- Expected user flow: Input string per line for url/pathway which outputs an image.
-- Users who wish to do this multiple times will need to type go again.

getInputImages :: [String] -> IO (Maybe [Image])
getInputImages prompts = getInputImagesHelper prompts []
    where
        getInputImagesHelper :: [String] -> [Image] -> IO (Maybe [Image])
        getInputImagesHelper [] images = do
            return $ Just images
        getInputImagesHelper (prompt:t) images = do
            putStrLn prompt
            imageName <- getLine
            result <- readImage imageName
            case result of
                (Left err) -> do
                    putStrLn $ "Failed to read image: " ++ err
                    return Nothing
                (Right image) -> getInputImagesHelper t (images ++ [image])

go :: IO ()
go = do
    let prompts = ["Insert the 1st image URL or filepath.", "Insert the 2nd image URL or filepath.", "Insert the mask image URL or filepath."]
    images <- getInputImages prompts
    case images of
        Nothing -> putStrLn "Sorry, please try again."
        (Just [im1, im2, mask]) -> do
            putStrLn "Successfully read images."

            printImageStats im1
            printImageStats im2
            printImageStats mask

            putStrLn ("Insert the name of the combined image.")
            combinedImageName <- getLine
            let combinedImagePath = combinedImageName ++ ".png"

            let combinedImage = imageBlend im1 im2 mask 1.0 0

            printImageStats combinedImage
            putStrLn "Done blending! Saving result..."

            writeToPng combinedImage combinedImagePath

            putStrLn ("Congratulations on merging your images! To see the result go to runtime-images folder.")
