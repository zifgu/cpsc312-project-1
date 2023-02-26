import Image
import ImageRead

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