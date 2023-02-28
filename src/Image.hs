{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Image
    (
        RGBImage,
        constructLaplacian,
        constructGaussian,
        showPyramid,
        prepareLaplacian,
        composeLaplacianPyramids,
        reconstructFromLap,
        imageBlend,
    ) where

import qualified Graphics.Image as HIP
import qualified Graphics.Image.Interface as HIPInterface

type RGBImage = HIP.Image HIP.VS HIP.RGB Double

-- Filter: radius (pos -> value)
data Filter1D = Filter1D Int (Int -> Double)

-- Elementwise operations on 2 images
elementwise :: (Double -> Double -> Double) -> RGBImage -> RGBImage -> RGBImage
elementwise op im1 im2 = HIP.zipWith pixelFunc im1 im2
    where pixelFunc px1 px2 = HIPInterface.liftPx2 op px1 px2

add :: RGBImage -> RGBImage -> RGBImage
add = elementwise (+)

sub :: RGBImage -> RGBImage -> RGBImage
sub = elementwise (-)

mul :: RGBImage -> RGBImage -> RGBImage
mul = elementwise (*)

-- Evaluate the 1D Gaussian function with standard deviation sigma at x.
gaussian1d :: Double -> Int -> Double
gaussian1d sigma x = 
    let xx = fromIntegral x
    in (1 / (2 * pi * sigma)) * exp (-(xx^2 / (2 * sigma)))

-- Returns a Gaussian filter with given sigma.
makeGaussianFilter1D :: Double -> Filter1D
makeGaussianFilter1D sigma =
    let r = ceiling (3 * sigma)
        normalizing_factor = sum [gaussian1d sigma x | x <- [-r..r]]
        f x = (gaussian1d sigma x) / normalizing_factor
    in (Filter1D r f)

-- Convolve image with separable filter using the separable method
convolveSeparable :: RGBImage -> Filter1D -> RGBImage
convolveSeparable im (Filter1D r f) = convolveI (convolveJ im)
    where
        (w, h) = HIPInterface.dims im
        multiplyPixel px c = HIPInterface.liftPx (*c) px
        getReflect get i j = get (i_access,j_access)
            where 
                i_access = if i < 0 then (-i - 1) else (if i >= w then (w - 1) - (i - w) else i)
                j_access = if j < 0 then (-j - 1) else (if j >= h then (h - 1) - (j - h) else j)
        iFunc get (ii, jj) = sum [multiplyPixel (getReflect get (ii + i) jj) (f (-i)) | i <- [-r..r]]
        jFunc get (ii, jj) = sum [multiplyPixel (getReflect get ii (jj + j)) (f (-j)) | j <- [-r..r]]
        convolveI im1 = HIPInterface.traverse im1 id iFunc
        convolveJ im1 = HIPInterface.traverse im1 id jFunc

-- Building Gaussian Pyramid
constructGaussian :: RGBImage -> Double -> Int -> [RGBImage]
constructGaussian im sigma iterations = constructGaussianHelp im (makeGaussianFilter1D sigma) iterations

constructGaussianHelp :: RGBImage -> Filter1D -> Int -> [RGBImage]
constructGaussianHelp im sigmafilter 0 = [im]
constructGaussianHelp im sigmafilter iterations = im : constructGaussianHelp con sigmafilter (iterations - 1)
    where con = convolveSeparable im sigmafilter

-- Building Laplacian Pyramid
constructLaplacian :: [RGBImage] -> [RGBImage]
constructLaplacian [c] = [c]
constructLaplacian (a:b:c) = sub a b : constructLaplacian (b:c)

-- Display an image pyramid - for debug purposes
showPyramid :: [RGBImage] -> RGBImage
showPyramid [im] = im
showPyramid (h:t) = HIP.leftToRight h (showPyramid t)

-- Prepare a Laplacian pyramid for display by converting to a visible color range
prepareLaplacian :: [RGBImage] -> [RGBImage]
prepareLaplacian [im] = [im]
prepareLaplacian (h:t) = prepare h : prepareLaplacian t
    where
        prepare im = HIP.map pixelFunc im
        pixelFunc px = HIPInterface.liftPx (\x -> x + 0.5) px

-- Composing Laplacian pyramid
composeLaplacianPyramids :: [RGBImage] -> [RGBImage] -> [RGBImage] -> [RGBImage]
composeLaplacianPyramids [] [] [] = []
composeLaplacianPyramids (laph1:lapt1) (laph2:lapt2) (maskh:maskt) = add (mul laph1 maskh) (mul laph2 (oneMinus maskh)):composeLaplacianPyramids lapt1 lapt2 maskt

-- Subtracts the Image from 1
oneMinus :: RGBImage -> RGBImage
oneMinus im = HIP.map pixelFunc im
    where pixelFunc px = HIPInterface.liftPx (\x -> 1 - x) px

-- Reconstructs an image from a Laplacian pyramid
reconstructFromLap :: [RGBImage] -> RGBImage
reconstructFromLap (h:t) = foldr add h t

-- Blends 2 images
imageBlend :: RGBImage -> RGBImage -> RGBImage -> Double -> Int -> RGBImage
imageBlend im1 im2 mask sigma iterations =
    let
        pyr1 = constructLaplacian (constructGaussian im1 sigma iterations)
        pyr2 = constructLaplacian (constructGaussian im2 sigma iterations)
        pyrMask = constructGaussian mask sigma iterations
        pyrOut = composeLaplacianPyramids pyr1 pyr2 pyrMask
        result = reconstructFromLap pyrOut
    in result