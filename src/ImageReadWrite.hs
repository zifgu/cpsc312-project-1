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

-- ****************************************************************
-- Image reading
-- ****************************************************************

-- readImageFromFile filepath   tries to read the image at filepath
-- May raise an exception - for example, if the file does not exist.
readImageFromFile :: String -> IO RGBImage
readImageFromFile filepath = do 
    img <- HIP.readImageRGB HIP.VS filepath
    return img

-- readImageFromUrl url         tries to fetch the image at url; returns either an error string or the image
--      url - an HTTP or HTTPS URL that points to an image resource
-- May return the error string if the URL does not point to an image, has non-2xx status code, etc.
readImageFromUrl :: String -> IO (Either String RGBImage)
readImageFromUrl url = do
    manager <- newManager tlsManagerSettings
    maybeResponse <- try $ do 
        request <- parseUrlThrow url            -- this configures the request to throw an error on non-2xx HTTP status codes
        response <- httpLbs request manager
        return response
        :: IO (Either HttpException (Response Lazy.ByteString))
    return $ case maybeResponse of
        (Left err) -> (Left (show err))
        (Right response) -> convertImage (responseBody response)

-- convertImage lazyBytes       tries to decode a (lazy) ByteString representing an image's contents to a RGB image; returns either an error string or the image
convertImage :: Lazy.ByteString -> Either String RGBImage
convertImage lazyBytes = 
    let
        bytes = Lazy.toStrict lazyBytes
        maybeIm = JuicyPixels.decodeImage bytes
        convertToDouble imWord8 = HIP.map (\ px -> HIPInterface.toDouble <$> px) imWord8
    in case maybeIm of
        (Left err) -> Left err
        (Right im) -> Right (convertToDouble (HIP.fromJPImageRGB8 (JuicyPixels.convertRGB8 im)))
