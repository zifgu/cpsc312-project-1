module Image
    (
        Image
    )
    where

-- TODO: parameterize by type?

-- Image: x -> y -> channel -> value
newtype Image = Image (Int -> Int -> Int -> Double)

add :: Image -> Image -> Image
add (Image f1) (Image f2) = Image (\ x y c -> (f1 x y c) + (f2 x y c))

sub :: Image -> Image -> Image
sub (Image f1) (Image f2) = Image (\ x y c -> (f1 x y c) - (f2 x y c))

mul :: Image -> Image -> Image
mul (Image f1) (Image f2) = Image (\ x y c -> (f1 x y c) * (f2 x y c))

get :: Image -> Int -> Int -> Int -> Double
get (Image f) x y c = f x y c

-- Test code:
-- im1 = Image (\ x y c -> fromIntegral x)
-- im2 = Image (\ x y c -> 2 * (fromIntegral y))
-- sum = add im1 im2
-- diff = sub im1 im2
-- prod = mul im1 im2

-- Filter: value function, filter radius
data Filter = Filter (Int -> Int -> Double) Int

convolve :: Image -> Filter -> Image
convolve (Image im) (Filter f r) = Image (im1)
    where im1 x y c = sum [im (x + i) (y + j) c * f (-i) (-j) | i <- [-r..r], j <- [-r..r]]

-- Test code:
-- Symmetric filter:
-- f1 = Filter (\ x y -> 1/9) 1
-- im1 = Image (\ x y c -> fromIntegral x^2)
-- res1 = convolve im1 f1
-- get res1 1 0 0                               -- approximately 15/9
-- get res1 2 1 0                               -- approximately 42/9

-- Asymmetric filter:
-- f2 = Filter (\ x y -> fromIntegral y / 3) 1
-- im2 = Image (\ x y c -> fromIntegral x)
-- res2 = convolve im2 f2
-- get res2 0 0 0                               -- should all be 0
-- im3 = Image (\ x y c -> fromIntegral y)
-- res3 = convolve im3 f2
-- get res3 1 2 0                               -- should all be -2 (approximately)
-- get res3 0 0 1