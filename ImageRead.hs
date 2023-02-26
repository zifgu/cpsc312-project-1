module ImageRead
    (
        readImageFromFile,
        readImageFromUrl,
    )
    where

import Image
import qualified Codec.Picture as JuicyPixels

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as Lazy

import Control.Exception

readImageFromFile :: String -> IO (Either String Image)
readImageFromFile filepath = do 
    maybeIm <- JuicyPixels.readImage filepath
    return $ resolveImage maybeIm

readImageFromUrl :: String -> IO (Either String Image)
readImageFromUrl url = do
    manager <- newManager tlsManagerSettings
    request <- parseUrlThrow url                -- This function will throw an error on non-2xx HTTP status codes, which is what we want
    maybeResponse <- try (httpLbs request manager) :: IO (Either HttpException (Response Lazy.ByteString))
    return $ case maybeResponse of
        (Left err) -> (Left (show err))
        (Right response) ->
            let
                bytes = Lazy.toStrict (responseBody response)
                maybeIm = JuicyPixels.decodeImage bytes
            in resolveImage maybeIm

resolveImage :: Either String JuicyPixels.DynamicImage -> Either String Image
resolveImage maybeIm =
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
        val = fromRGB8 (if c <= 0 then r else if c == 1 then g else b)

-- Convert an integer in [0, 255] to a double in [0, 1]
fromRGB8 :: Integral a => a -> Double
fromRGB8 v = (fromIntegral v) / 255
