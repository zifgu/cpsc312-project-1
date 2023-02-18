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
