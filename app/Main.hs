{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Graphics.Image as HIP hiding (Filter, convolve, sum)
import Graphics.Image.Interface as HIPInterface hiding (foldr)

elementwise :: (Array arr RGB Double) => (Double -> Double -> Double) -> Image arr RGB Double -> Image arr RGB Double -> Image arr RGB Double
elementwise op im1 im2 = HIP.zipWith pixelFunc im1 im2
    where
        pixelFunc px1 px2 = liftPx2 op px1 px2

add :: (Array arr RGB Double) => Image arr RGB Double -> Image arr RGB Double -> Image arr RGB Double
add = elementwise (+)

sub :: (Array arr RGB Double) => Image arr RGB Double -> Image arr RGB Double -> Image arr RGB Double
sub = elementwise (-)

mul :: (Array arr RGB Double) => Image arr RGB Double -> Image arr RGB Double -> Image arr RGB Double
mul = elementwise (*)

data Filter1D = Filter1D Int (Int -> Double)

-- Evaluate the 2D Gaussian function with standard deviation sigma at x and y.
gaussian2d :: Double -> Int -> Int -> Double
gaussian2d sigma x y =
    let
        d = 2 * sigma^2
        xx = fromIntegral x
        yy = fromIntegral y
    in (1 / (pi * d)) * exp (-((xx^2 + yy^2) / d))

-- Evaluate the 1D Gaussian function with standard deviation sigma at x.
gaussian1d :: Double -> Int -> Double
gaussian1d sigma x = 
    let xx = fromIntegral x
    in (1 / (2 * pi * sigma)) * exp (-(xx^2 / (2 * sigma)))

-- Returns a Gaussian filter with given sigma.
makeGaussianFilter1D :: Double -> Filter1D
makeGaussianFilter1D sigma =
    let r = ceiling (3 * sigma)
        -- TODO: normalization?
        normalizing_factor = sum [gaussian1d sigma x | x <- [-r..r]]
        f = \ x -> (gaussian1d sigma x) / normalizing_factor
    in (Filter1D r f)

convolveSeparable :: (Array arr RGB Double) => Image arr RGB Double -> Filter1D -> Image arr RGB Double
convolveSeparable im fil@(Filter1D r f) = convolveI (convolveJ im)
    where
        (w, h) = dims im
        multiplyPixel px c = liftPx (*c) px
        getReflect get i j = get (i_access,j_access)
            where 
                i_access = if i < 0 then (-i - 1) else (if i >= w then (w - 1) - (i - w) else i)
                j_access = if j < 0 then (-j - 1) else (if j >= h then (h - 1) - (j - h) else j)
        iFunc get (ii, jj) = sum [multiplyPixel (getReflect get (ii + i) jj) (f (-i)) | i <- [-r..r]]
        jFunc get (ii, jj) = sum [multiplyPixel (getReflect get ii (jj + j)) (f (-j)) | j <- [-r..r]]
        convolveI im1 = HIPInterface.traverse im1 id iFunc
        convolveJ im1 = HIPInterface.traverse im1 id jFunc

-- Building Gaussian Pyramid
constructGaussianSeparable :: (Array arr RGB Double) => Image arr RGB Double -> Double -> Int -> [Image arr RGB Double]
constructGaussianSeparable im sigma iterations =  constructGaussianSeparableHelp im (makeGaussianFilter1D sigma) iterations

constructGaussianSeparableHelp :: (Array arr RGB Double) => Image arr RGB Double -> Filter1D -> Int -> [Image arr RGB Double]
constructGaussianSeparableHelp im sigmafilter 0 = [im]
constructGaussianSeparableHelp im sigmafilter iterations = im : constructGaussianSeparableHelp con sigmafilter (iterations - 1)
    where con = convolveSeparable im sigmafilter

-- Building Laplacian Pyramid
constructLaplacian :: (Array arr RGB Double) => [Image arr RGB Double] -> [Image arr RGB Double]
constructLaplacian [c] = [c]
constructLaplacian (a:b:c) = sub a b : constructLaplacian (b:c)

-- Display an image pyramid - debug
showPyramid :: (Array arr RGB Double) => [Image arr RGB Double] -> Image arr RGB Double
showPyramid [im] = im
showPyramid (h:t) = leftToRight h (showPyramid t)

-- Prepare a Laplacian pyramid for display by converting to a visible color range
prepareLaplacian :: (Array arr RGB Double) => [Image arr RGB Double] -> [Image arr RGB Double]
prepareLaplacian [im] = [im]
prepareLaplacian (h:t) = prepare h : prepareLaplacian t
    where
        prepare im = HIP.map pixelFunc im
        pixelFunc px = liftPx (\x -> x + 0.5) px

-- Composing Laplacian pyramid
composeLaplacianPyramids :: (Array arr RGB Double) => [Image arr RGB Double] -> [Image arr RGB Double] -> [Image arr RGB Double] -> [Image arr RGB Double]
composeLaplacianPyramids [] [] [] = []
composeLaplacianPyramids (laph1:lapt1) (laph2:lapt2) (maskh:maskt) = add (mul laph1 maskh) (mul laph2 (oneMinus maskh)):composeLaplacianPyramids lapt1 lapt2 maskt

-- Subtracts the Image from 1
oneMinus :: (Array arr RGB Double) => Image arr RGB Double -> Image arr RGB Double
oneMinus im = HIP.map pixelFunc im
    where pixelFunc px = liftPx (\x -> 1 - x) px

-- Reconstructs an image from a laplacian pyramid
reconstructFromLap :: (Array arr RGB Double) => [Image arr RGB Double] -> Image arr RGB Double
reconstructFromLap (h:t) = foldr add h t

-- Blends 2 images
imageBlend :: (Array arr RGB Double) => Image arr RGB Double -> Image arr RGB Double -> Image arr RGB Double -> Double -> Int -> Image arr RGB Double
imageBlend im1 im2 mask sigma iterations =
    let
        pyr1 = constructLaplacian (constructGaussianSeparable im1 sigma iterations)
        pyr2 = constructLaplacian (constructGaussianSeparable im2 sigma iterations)
        pyrMask = constructGaussianSeparable mask sigma iterations
        pyrOut = composeLaplacianPyramids pyr1 pyr2 pyrMask
        result = reconstructFromLap pyrOut
    in result

main :: IO ()
main = do
    -- This example takes about 3 min on my computer
    image1 <- readImageRGB VU "images/orchid.jpg"
    image2 <- readImageRGB VU "images/violet.jpg"
    imageMask <- readImageRGB VU "images/orchid_mask.bmp"
    let sigma = 3.0
    let iterations = 2
    let pyr1 = constructLaplacian (constructGaussianSeparable image1 sigma iterations)
    displayImage (showPyramid (prepareLaplacian pyr1))
    let pyr2 = constructLaplacian (constructGaussianSeparable image2 sigma iterations)
    displayImage (showPyramid (prepareLaplacian pyr2))
    let pyrMask = constructGaussianSeparable imageMask sigma iterations
    displayImage (showPyramid pyrMask)
    let pyrOut = composeLaplacianPyramids pyr1 pyr2 pyrMask
    displayImage (showPyramid (prepareLaplacian pyrOut))
    let result = reconstructFromLap pyrOut
    displayImage result