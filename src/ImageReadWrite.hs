{-# LANGUAGE FlexibleContexts #-}

module ImageReadWrite
    (
        readImageFromFile,
        readImageFromUrl
    ) where

import Image
import qualified Codec.Picture as JuicyPixels
import qualified Graphics.Image as HIP
import qualified Graphics.Image.Interface as HIPInterface

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy as Lazy

import Control.Exception

readImageFromFile :: String -> IO RGBImage
readImageFromFile filepath = do 
    img <- HIP.readImageRGB HIP.VS filepath
    return img

readImageFromUrl :: String -> IO (Either String RGBImage)
readImageFromUrl url = do
    manager <- newManager tlsManagerSettings
    maybeResponse <- try $ do 
        request <- parseUrlThrow url                -- This function will throw an error on non-2xx HTTP status codes, which is what we want
        response <- httpLbs request manager
        return response
        :: IO (Either HttpException (Response Lazy.ByteString))
    return $ case maybeResponse of
        (Left err) -> (Left (show err))
        (Right response) -> convertImage (responseBody response)

convertImage :: Lazy.ByteString -> Either String RGBImage
convertImage lazyBytes = 
    let
        bytes = Lazy.toStrict lazyBytes
        maybeIm = JuicyPixels.decodeImage bytes
        convertToDouble imWord8 = HIP.map (\ px -> HIPInterface.toDouble <$> px) imWord8
    in case maybeIm of
        (Left err) -> Left err
        (Right im) -> Right (convertToDouble (HIP.fromJPImageRGB8 (JuicyPixels.convertRGB8 im)))
