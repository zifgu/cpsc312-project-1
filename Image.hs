module Image
    (
        Image (..),
        add,
        sub,
        mul,
        get,
        Filter (..),
        convolve,
    )
    where

import Control.Exception

-- TODO: parameterize image by type?

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

-- Filter: radius (row -> col -> value)
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
