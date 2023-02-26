-- CPSC 312 Project 1

module Project ( readthisImage, writethisImage) where

import Graphics.Image.IO

-- To run this program, do:
-- ghci
-- :load Project

--data Image  = arr cs e

-- function for read will take a file path from input then display the picture
--readImage :: Array arr cs e => Image arr cs e -> Image arr cs e (somewhat confused on the representation here)
--readthisImage :: IO() => Image arr cs e 
readthisImage = do
  putStrLn("Put path to image name")
  pathway <- getLine
  picture <- readImageRGB VU pathway
  displayImage picture
  return picture

-- function for write (will save based off pathname input)
-- writethisImage :: IO Image
writethisImage = do 
		putStrLn("Put path for new image name")
		pathway <- getLine
		putStrnLn("Put original image path")
		originalpath <-getLine
		myimage <- readImageRGB VU originalpath
		writeImage pathway $ myimage

		--below uses superimpose which layers 2 images ontop based off coords
		--putStrLn("Put path for source image")
		--pathwaysource <- getLine
		--putStrLn("Put path for top image ")
		--pathwaytopimage <- getLine
		--writeImage pathway $ superimpose (0,0) pathwaytopimage pathwaysource


	

