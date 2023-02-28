{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Image
    (
        RGBImage,
        imageBlend,
    ) where

import qualified Graphics.Image as HIP
import qualified Graphics.Image.Interface as HIPInterface

-- ****************************************************************
-- Data types
-- ****************************************************************

-- RGBImage: the image data type used in all operations.
-- RGBPixel: the type of one pixel of an RGBImage.
-- We only accept RGB images as input.
type RGBImage = HIP.Image HIP.VS HIP.RGB Double
type RGBPixel = HIP.Pixel HIP.RGB Double

-- Filter1D: radius (\ pos -> value)
-- Represents a filter, a kind of 1-dimensional array, used in image filtering.
-- A filter has (2 * radius + 1) elements.
data Filter1D = Filter1D Int (Int -> Double)

-- ****************************************************************
-- Basic image operations
-- ****************************************************************

-- elementwise op im1 im2   returns a new image whose [i,j]th pixel is (op im1[i,j] im2[i,j])
elementwise :: (RGBPixel -> RGBPixel -> RGBPixel) -> RGBImage -> RGBImage -> RGBImage
elementwise op im1 im2 = HIP.zipWith op im1 im2

-- add image1 image2        computes image1 + image2 (elementwise)
add :: RGBImage -> RGBImage -> RGBImage
add = elementwise (+)

-- add image1 image2        computes image1 - image2 (elementwise)
sub :: RGBImage -> RGBImage -> RGBImage
sub = elementwise (-)

-- mul image1 image2        computes image1 * image2 (elementwise)
mul :: RGBImage -> RGBImage -> RGBImage
mul = elementwise (*)

-- oneMinus im              subtracts each pixel in im from 1
oneMinus :: RGBImage -> RGBImage
oneMinus im = HIP.map pixelFunc im
    where pixelFunc px = HIPInterface.liftPx (\x -> 1 - x) px

-- ****************************************************************
-- Convolution
-- ****************************************************************

-- gaussian1d sigma x       evaluates the 1D Gaussian function with standard deviation sigma at x
gaussian1d :: Double -> Int -> Double
gaussian1d sigma x = 
    let xx = fromIntegral x
    in (1 / (2 * pi * sigma)) * exp (-(xx^2 / (2 * sigma)))

-- makeGaussianFilter1D sigma   returns a 1D Gaussian filter with standard deviation sigma
makeGaussianFilter1D :: Double -> Filter1D
makeGaussianFilter1D sigma =
    let r = ceiling (3 * sigma)
        normalizing_factor = sum [gaussian1d sigma x | x <- [-r..r]]
        f x = (gaussian1d sigma x) / normalizing_factor
    in (Filter1D r f)

-- convolveSeparable im filter  convolves im with filter using the separable method
convolveSeparable :: RGBImage -> Filter1D -> RGBImage
convolveSeparable im (Filter1D r f) = convolveI (convolveJ im)
    where
        (w, h) = HIPInterface.dims im
        multiplyPixel px c = HIPInterface.liftPx (*c) px

        -- getReflect getter i j    tries to get the pixel at (i,j), handling out-of-bounds accesses by reflecting across the image border
        getReflect get i j = get (i_access,j_access)
            where 
                i_access = if i < 0 then (-i - 1) else (if i >= w then (w - 1) - (i - w) else i)
                j_access = if j < 0 then (-j - 1) else (if j >= h then (h - 1) - (j - h) else j)
        
        -- convolveI im1    convolves the columns of im1 with filter
        iFunc get (ii, jj) = sum [multiplyPixel (getReflect get (ii + i) jj) (f (-i)) | i <- [-r..r]]
        convolveI im1 = HIPInterface.traverse im1 id iFunc

        -- convolveJ im1    convolves the rows of im1 with filter
        jFunc get (ii, jj) = sum [multiplyPixel (getReflect get ii (jj + j)) (f (-j)) | j <- [-r..r]]        
        convolveJ im1 = HIPInterface.traverse im1 id jFunc

-- ****************************************************************
-- Pyramids
-- ****************************************************************

-- A Gaussian pyramid represents an image as successively more blurred versions of itself. The first level is the original image and the last level is blurriest.
-- constructGaussian im sigma iterations    builds a Gaussian pyramid from im
--      iterations - resulting pyramid has (iterations + 1) levels, including im
--      sigma 0 controls the amount of blur
constructGaussian :: RGBImage -> Double -> Int -> [RGBImage]
constructGaussian im sigma iterations = constructGaussianHelp im (makeGaussianFilter1D sigma) iterations
    where
        constructGaussianHelp :: RGBImage -> Filter1D -> Int -> [RGBImage]
        constructGaussianHelp im1 _ 0 = [im1]
        constructGaussianHelp im1 sigmafilter n =
            let con = convolveSeparable im1 sigmafilter
            in im1 : constructGaussianHelp con sigmafilter (n - 1)

-- A Laplacian pyramid is the difference between successive Gaussian pyramid levels.
-- It has the same number of levels as the Gaussian pyramid; its last level is simply the last level of the Gaussian pyramid.
-- constructLaplacian pyr       builds a Laplacian pyramid from a Gaussian pyramid pyr
constructLaplacian :: [RGBImage] -> [RGBImage]
constructLaplacian [c] = [c]
constructLaplacian (a:b:c) = sub a b : constructLaplacian (b:c)

-- prepareLaplacian             prepares a Laplacian pyramid for display by converting levels to a visible color range
prepareLaplacian :: [RGBImage] -> [RGBImage]
prepareLaplacian [im] = [im]                                -- last level does not need to be converted
prepareLaplacian (h:t) = prepare h : prepareLaplacian t
    where
        prepare im = HIP.map pixelFunc im
        pixelFunc px = HIPInterface.liftPx (\x -> x + 0.5) px

-- showPyramid pyr              displays an image pyramid for debug purposes
showPyramid :: [RGBImage] -> RGBImage
showPyramid [im] = im
showPyramid (h:t) = HIP.leftToRight h (showPyramid t)

-- ****************************************************************
-- Reconstruction
-- ****************************************************************

-- composeLaplacianPyramids lap1 lap2 mask      creates a new Laplacian pyramid by combining each level of lap1 and lap2, weighted by the corresponding level of mask
--      lap1 - Laplacian pyramid of image 1
--      lap2 - Laplacian pyramid of image 2
--      mask - Gaussian pyramid of mask image
composeLaplacianPyramids :: [RGBImage] -> [RGBImage] -> [RGBImage] -> [RGBImage]
composeLaplacianPyramids [] [] [] = []
composeLaplacianPyramids (laph1:lapt1) (laph2:lapt2) (maskh:maskt) = add (mul laph1 maskh) (mul laph2 (oneMinus maskh)):composeLaplacianPyramids lapt1 lapt2 maskt

-- reconstructFromLap pyr       reconstructs an image from its Laplacian pyramid pyr
reconstructFromLap :: [RGBImage] -> RGBImage
reconstructFromLap (h:t) = foldr add h t

-- imageBlend im1 im2 mask sigma iterations     blends im1 and im2, modulated by mask
--      iterations - controls the number of levels of the pyramids we create
--      sigma - controls the amount of blur between each level 
imageBlend :: RGBImage -> RGBImage -> RGBImage -> Double -> Int -> RGBImage
imageBlend im1 im2 mask sigma iterations =
    let
        pyr1 = constructLaplacian (constructGaussian im1 sigma iterations)
        pyr2 = constructLaplacian (constructGaussian im2 sigma iterations)
        pyrMask = constructGaussian mask sigma iterations
        pyrOut = composeLaplacianPyramids pyr1 pyr2 pyrMask
        result = reconstructFromLap pyrOut
    in result