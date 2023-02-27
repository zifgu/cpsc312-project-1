import Image
import ImageRead
import Control.Exception

testFromFile :: String -> IO ()
testFromFile filepath = do
    result <- readImageFromFile filepath
    case result of
        (Left str) -> putStrLn $ "Failed to read file: " ++ str
        (Right img) -> printImageStats img

testFromUrl :: String -> IO ()
testFromUrl url = do
    result <- readImageFromUrl url
    case result of
        (Left str) -> putStrLn $ "Failed to fetch URL: " ++ str
        (Right img) -> printImageStats img

printImageStats :: Image -> IO ()
printImageStats image = do
    let (Image w h _) = image
    putStrLn ("Width: " ++ show w)
    putStrLn ("Height: " ++ show h)
    putStrLn ("Pixel (0, 0): " ++ show (get image 0 0 0) ++ "," ++ show (get image 0 0 1) ++ "," ++ show (get image 0 0 2))
    putStrLn ("Pixel (0, 100): " ++ show (get image 0 100 0) ++ "," ++ show (get image 0 100 1) ++ "," ++ show (get image 0 100 2))
    putStrLn ("Pixel (60, 20): " ++ show (get image 60 20 0) ++ "," ++ show (get image 60 20 1) ++ "," ++ show (get image 60 20 2))
    putStrLn ("Pixel (100, 100): " ++ show (get image 100 100 0) ++ "," ++ show (get image 100 100 1) ++ "," ++ show (get image 100 100 2))


readingImage :: String -> IO ()
readingImage imagename = do
    result <- readImageFromFile imagename
    case result of
        (Left str) -> readImageFromUrl str
        (Right img) -> printImageStats img
    putStrLn ("Read Complete")

-- Expected user flow: Input string per line for url/pathway which outputs an image.
-- Users who wish to do this multiple times will need to type go again.


go :: IO (String, String, String, String) 
go = do
    putStrLn ("Insert the 1st Image URL or filepath.")
    firstName <- getLine
    readingImage firstName
    putStrLn ("Insert the 2nd Image URL or filepath.")
    secondName <- getLine
    readingImage secondName
    putStrLn ("Insert the filter Image URL or filepath.")    
    filterName <- getLine
    readingImage filterName

    putStrLn ("Insert the name of the combined image.")
    combinedImageName <- getLine

    putStrLn ("Congratulations on merging your images! To see the result go to runtime-images folder.")
    putStrLn(" ")
    putStrLn ("   History:")
    putStrLn ("       firstName = " ++ firstName)
    putStrLn ("       secondName = " ++ secondName)     
    putStrLn ("       filterName = " ++ filterName)      
    putStrLn ("       combinedImageName = " ++ combinedImageName)
    putStrLn(" ")
    return (firstName, secondName, filterName, combinedImageName)
      