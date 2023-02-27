module FunctionImage
    (
        Image (..),
        add,
        sub,
        mul,
        get,
        convolve,
        constructGaussian,
        constructLaplacian,
        composeLaplacianPyramids,
        reconstructFromLap,
        imageBlend,
    )
    where

import Control.Exception

-- Image: width height (row -> col -> channel -> value)
-- NOTE: width = # cols, height = # rows
data Image = Image Int Int (Int -> Int -> Int -> Double)

-- Elementwise operations: input images must be the same size
elementwise :: (Double -> Double -> Double) -> (Image -> Image -> Image)
elementwise op (Image w1 h1 f1) (Image w2 h2 f2)
    | w1 == w2 && h1 == h2      = Image w1 h1 (\ row col c -> op (f1 row col c) (f2 row col c))
    | otherwise                 = error "Images have different sizes"

add :: Image -> Image -> Image
add = elementwise (+)

sub :: Image -> Image -> Image
sub = elementwise (-)

mul :: Image -> Image -> Image
mul = elementwise (*)

-- Image access: 0-based indexing. Indices must be within image width/height/#channels
get :: Image -> Int -> Int -> Int -> Double
get (Image w h f) row col c
    | 0 <= row && row < h && 0 <= col && col < w && 0 <= c && c < 3   = f row col c
    | otherwise                                                       = error "Out of bounds image access"

-- Test code:
-- im1 = Image 10 10 (\ row col c -> fromIntegral row)
-- im2 = Image 10 10 (\ row col c -> 2 * (fromIntegral col))
-- im3 = Image 9 10 (\ row col c -> fromIntegral row)

-- sum = add im1 im2
-- get sum 1 2 0                    -- 5.0

-- diff = sub im1 im2
-- get diff 1 2 0                   -- -3.0

-- prod = mul im1 im2
-- get prod 1 2 0                   -- 4.0

-- bad = add im1 im3
-- get bad 0 0 0                    -- error

-- Filter: radius (x -> y -> value)
-- Filter is centered at index (0, 0). Filter width and height are 2*radius + 1
data Filter = Filter Int (Int -> Int -> Double)

convolve :: Image -> Filter -> Image
convolve (Image w h im) (Filter r f) = Image w h im1
    where
        im1 row col c = sum [get_reflect im (row + i) (col + j) c * f (-i) (-j) | i <- [-r..r], j <- [-r..r]]

        -- Image access - reflects across the edge for out of bounds accesses
        get_reflect :: (Int -> Int -> Int -> Double) -> Int -> Int -> Int -> Double
        get_reflect f row col c = f row_access col_access c
            where
                row_access = if row < 0 then (-row - 1) else (if row >= h then (h - 1) - (row - h) else row)
                col_access = if col < 0 then (-col - 1) else (if col >= w then (w - 1) - (col - w) else col)

-- Test code:
-- Symmetric filter:
-- f1 = Filter 1 (\ row col -> 1/9)
-- im1 = Image 10 5 (\ row col c -> fromIntegral row^2)
-- res1 = convolve im1 f1
-- get res1 1 0 0                               -- 15/9
-- get res1 2 1 1                               -- 42/9

-- Asymmetric filter:
-- f2 = Filter 1 (\ row col -> fromIntegral col / 6)
-- im2 = Image 10 5 (\ row col c -> fromIntegral row)
-- res2 = convolve im2 f2
-- get res2 0 0 0                               -- every pixel should be 0
-- get res2 4 9 0
-- im3 = Image 10 5 (\ row col c -> fromIntegral col)
-- res3 = convolve im3 f2
-- get res3 0 0 1                               -- first column should be -0.5
-- get res3 3 9 2                               -- last column should be -0.5
-- get res3 1 2 0                               -- all others should be -1

-- Evaluate the 2D Gaussian function with standard deviation sigma at x and y.
gaussian2d :: Double -> Int -> Int -> Double
gaussian2d sigma x y =
    let
        d = 2 * sigma^2
        xx = fromIntegral x
        yy = fromIntegral y
    in (1 / (pi * d)) * exp (-((xx^2 + yy^2) / d))

-- Returns a Gaussian filter with given sigma.
make_gaussian_filter :: Double -> Filter
make_gaussian_filter sigma =
    let r = ceiling (3 * sigma)
        normalizing_factor = sum [gaussian2d sigma x y | x <- [-r..r], y <- [-r..r]]
        f = \ x y -> (gaussian2d sigma x y) / normalizing_factor
    in (Filter r f)

-- Building Gaussian Pyramid
constructGaussian :: Image -> Double -> Int -> [Image]
constructGaussian im sigma iterations =  constructGaussianHelp im (make_gaussian_filter sigma) iterations

constructGaussianHelp :: Image -> Filter -> Int -> [Image]
constructGaussianHelp (Image width height im) sigmafilter 0 = [convolve (Image width height im) sigmafilter]
constructGaussianHelp (Image width height im) sigmafilter iterations = con : constructGaussianHelp con sigmafilter (iterations - 1) where
    con = convolve (Image width height im) sigmafilter

-- Building Laplacian Pyramid
constructLaplacian :: [Image] -> [Image]
constructLaplacian [c] = [c]
constructLaplacian (a:b:c) = sub a b : constructLaplacian (b:c)

-- Composing Laplacian pyramid
composeLaplacianPyramids :: [Image] -> [Image] -> [Image] -> [Image]
composeLaplacianPyramids [] [] [] = []
composeLaplacianPyramids (laph1:lapt1) (laph2:lapt2) (maskh:maskt) = add (mul laph1 maskh) (mul laph2 (oneMinus maskh)):composeLaplacianPyramids lapt1 lapt2 maskt

-- Subtracts the Image from 1
oneMinus :: Image -> Image
oneMinus (Image w h im) = sub (Image w h (\ row col c -> 1.0)) (Image w h im)

-- Create an image with all zeroes of the same size as the input image
zeroImage :: Image -> Image
zeroImage (Image w h _) = Image w h (\ row col c -> 0.0)

-- Reconstructs an image from a laplacian pyramid
reconstructFromLap :: [Image] -> Image
reconstructFromLap (h:t) = foldr add (zeroImage h) (h:t)

-- Blends two images
imageBlend :: Image -> Image -> Image -> Double -> Int -> Image
imageBlend im1 im2 mask sigma iterations =
    let
        pyr1 = constructLaplacian (constructGaussian im1 sigma iterations)
        pyr2 = constructLaplacian (constructGaussian im2 sigma iterations)
        pyrMask = constructGaussian mask sigma iterations
        pyrOut = composeLaplacianPyramids pyr1 pyr2 pyrMask
        result = reconstructFromLap pyrOut
    in result

--Testing the Gaussian Pyramid
testGf :: Int -> Int -> Int -> Double
testGf w h c = fromIntegral w

testG :: Image
testG = Image 1 1 testGf

instance Eq Image where
    (Image h1 w1 x) == (Image h2 w2 y) = imhelp (Image h1 w1 x) == imhelp (Image h2 w2 y) && w1 == w2 && h1 == h2

instance Show Image where
    show x = foldr (\ y z -> show y ++ ", " ++ z) "" (imhelp x) 

imhelp :: Image -> [Double]
imhelp (Image w h im) = imhelphelp 0 w h (Image w h im)

imhelphelp :: Int -> Int -> Int -> Image -> [Double]
imhelphelp 2 0 0 (Image wi hi im) = [im 0 0 2]
imhelphelp 2 0 h (Image wi hi im) = im 0 h 2:imhelphelp 0 wi (h-1) (Image wi hi im)
imhelphelp 2 w h (Image wi hi im) = im w h 2:imhelphelp 0 (w-1) h (Image wi hi im)
imhelphelp c w h (Image wi hi im) = im w h c:imhelphelp (c+1) w h (Image wi hi im)

testFilter :: Filter
testFilter = make_gaussian_filter 1.0

testGpart1 :: Image
testGpart1 = convolve testG testFilter
testGpart2 :: Image
testGpart2 = convolve testGpart1 testFilter
testGpart3 :: Image
testGpart3 = convolve testGpart2 testFilter
testGFinal :: [Image]
testGFinal = [testGpart1,testGpart2,testGpart3]

testGComparison = constructGaussian testG 1.0 2
testGResult = testGFinal == testGComparison

-- Testing Laplacian pyramid
testLaplacian = [sub testGpart1 testGpart2, sub testGpart2 testGpart3, testGpart3]
testLaplacianComparison = constructLaplacian testGFinal
testLaplacianResult = testLaplacian == testLaplacianComparison
