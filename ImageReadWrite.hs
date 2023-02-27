module ImageReadWrite
    (
        readImage,
        readImageFromFile,
        readImageFromUrl,
        writeToPng
    )
    where

import Image
import qualified Codec.Picture as JuicyPixels

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as Lazy

import System.Directory
import Control.Exception

readImage :: String -> IO (Either String Image)
readImage pathOrUrl = do
    isPath <- doesFileExist pathOrUrl
    if isPath
    then readImageFromFile pathOrUrl
    else readImageFromUrl pathOrUrl

readImageFromFile :: String -> IO (Either String Image)
readImageFromFile filepath = do 
    maybeIm <- JuicyPixels.readImage filepath
    return $ convertImage maybeIm

readImageFromUrl :: String -> IO (Either String Image)
readImageFromUrl url = do
    manager <- newManager tlsManagerSettings
    maybeResponse <- try $ do 
        request <- parseUrlThrow url                -- This function will throw an error on non-2xx HTTP status codes, which is what we want
        response <- httpLbs request manager
        return response
        :: IO (Either HttpException (Response Lazy.ByteString))
    return $ case maybeResponse of
        (Left err) -> (Left (show err))
        (Right response) ->
            let
                bytes = Lazy.toStrict (responseBody response)
                maybeIm = JuicyPixels.decodeImage bytes
            in convertImage maybeIm

convertImage :: Either String JuicyPixels.DynamicImage -> Either String Image
convertImage maybeIm =
    case maybeIm of
        (Left err) -> (Left err)
        (Right img) ->
            let rgbImg = JuicyPixels.convertRGB8 img
            in Right (toImage rgbImg)

toImage :: JuicyPixels.Image JuicyPixels.PixelRGB8 -> Image
toImage im = Image w h f
    where
        (JuicyPixels.Image w h _) = im
        f row col c = getChannel (JuicyPixels.pixelAt im col row) c -- TODO: reverse x and y; row = y, col = x

getChannel :: JuicyPixels.PixelRGB8 -> Int -> Double
getChannel pix c = val
    where
        (JuicyPixels.PixelRGB8 r g b) = pix
        val = from8Bit (if c <= 0 then r else if c == 1 then g else b)

-- Convert an integer in [0, 255] to a double in [0, 1]
from8Bit :: Integral a => a -> Double
from8Bit v = (fromIntegral v) / 255

-- Convert a double in [0, 1] to an integer in [0, 255]
to8Bit :: Double -> JuicyPixels.Pixel8
to8Bit v = round (v * 255)

toDynIm :: Image -> JuicyPixels.Image JuicyPixels.PixelRGB8
toDynIm image@(Image w h _) = JuicyPixels.generateImage func w h
    where
        func x y = JuicyPixels.PixelRGB8 (to8Bit (get image y x 0)) (to8Bit (get image y x 1)) (to8Bit (get image y x 2))

writeToPng :: Image -> String -> IO ()
writeToPng image path = JuicyPixels.writePng path (toDynIm image)
