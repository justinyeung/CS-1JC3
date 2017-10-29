-- Assignment 2 Extra Credit
-- Name: Michael Balas
-- MacID: balasm
-- Date: October 20, 2017

module VectorSpace where

newtype Vector2 a = Vector2 (a, a) deriving (Show, Eq)
newtype Vector3 a = Vector3 (a, a, a) deriving (Show, Eq)
newtype Vector4 a = Vector4 (a, a, a, a) deriving (Show, Eq)

class VectorSpace v where
    -- Implements the zero vector constant
    vecZero         :: (Num a) => v a
    -- Implements the sum function
    vecSum          :: (Num a) => v a -> v a -> v a
    -- Implements the scalar product function 
    vecScalarProd   :: (Num a) => a -> v a -> v a
    -- Implements the magnitude function
    vecMagnitude    :: (Floating a) => v a -> a
    {-- Helper Functions --}
    -- Calculates the difference between two vectors
    vecDifference   :: (Num a) => v a -> v a -> v a
    -- Calculates the distance between two vectors
    vecDistance     :: (Floating a) => v a -> v a -> a

instance VectorSpace Vector2 where
    vecZero = Vector2 (0, 0)
    vecSum (Vector2 (x1, y1)) (Vector2 (x2, y2)) = Vector2 (x1+x2, y1+y2)
    vecScalarProd r (Vector2 (x, y)) = (Vector2 (r*x, r*y))
    vecMagnitude (Vector2 (x, y)) = sqrt((x^2) + (y^2))
    {-- Helper Functions --}
    vecDifference (Vector2 (x1, y1)) (Vector2 (x2, y2)) = vecSum (Vector2 (x1, y1)) (vecScalarProd (-1) (Vector2 (x2, y2)))
    vecDistance (Vector2 (x1, y1)) (Vector2 (x2, y2)) = vecMagnitude (vecDifference (Vector2 (x1, y1)) (Vector2 (x2, y2)))

instance VectorSpace Vector3 where
    vecZero = Vector3 (0, 0, 0)
    vecSum (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = Vector3 (x1+x2, y1+y2, z1+z2)
    vecScalarProd r (Vector3 (x, y, z)) = (Vector3(r*x, r*y, r*z))
    vecMagnitude (Vector3 (x, y, z)) = sqrt((x^2) + (y^2) + (z^2))
    {-- Helper Functions --}
    vecDifference (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = vecSum (Vector3 (x1, y1, z1)) (vecScalarProd (-1) (Vector3 (x2, y2, z2)))
    vecDistance (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)) = vecMagnitude (vecDifference (Vector3 (x1, y1, z1)) (Vector3 (x2, y2, z2)))

instance VectorSpace Vector4 where
    vecZero = Vector4 (0, 0, 0, 0)
    vecSum (Vector4 (w1, x1, y1, z1)) (Vector4 (w2, x2, y2, z2)) = Vector4 (w1+w2, x1+x2, y1+y2, z1+z2)
    vecScalarProd r (Vector4 (w, x, y, z)) = (Vector4(r*w, r*x, r*y, r*z))
    vecMagnitude (Vector4 (w, x, y, z)) = sqrt((w^2) + (x^2) + (y^2) + (z^2))
    {-- Helper Functions --}
    vecDifference (Vector4 (w1, x1, y1, z1)) (Vector4 (w2, x2, y2, z2)) = vecSum (Vector4 (w1, x1, y1, z1)) (vecScalarProd (-1) (Vector4 (w2, x2, y2, z2)))
    vecDistance (Vector4 (w1, x1, y1, z1)) (Vector4 (w2, x2, y2, z2)) = vecMagnitude (vecDifference (Vector4 (w1, x1, y1, z1)) (Vector4 (w2, x2, y2, z2)))

-- Returns a list of distances between a given vector, v, and a list of vectors, vs. 
vecF :: (Floating a, VectorSpace v) => v a -> [v a] -> [a]
vecF v vs = map (vecDistance (v)) vs


{- ----- Test Plans ----- -}
{-
Function: vecScalarProd
Test Case Number: 1
Input: vecScalarProd 5.45 (Vector2(0,0))
Expected Output: Vector2 (0.0,0.0)
Actual Output: Vector2 (0.0,0.0)

Function: vecScalarProd
Test Case Number: 2
Input: vecScalarProd 0 (Vector3(4, 6, 8))
Expected Output: Vector3 (0,0,0)
Actual Output: Vector3 (0,0,0)

Function: vecScalarProd
Test Case Number: 3
Input: vecScalarProd 2 (Vector4(2, 3, 4, 5))
Expected Output: Vector4 (4,6,8,10)
Actual Output: Vector4 (4,6,8,10)

Function: vecScalarProd
Test Case Number: 4
Input: vecScalarProd (-7.3534505) (Vector3(57925.34632, (-64.34957289), 473.348965734))
Expected Output: Vector3 (-425951.16685948,473.19139894276,-3480.7481887512) 
Actual Output: Vector3 (-425951.1668594771,473.19139894275696,-3480.7481887511653)

Function: vecSum
Test Case Number: 1
Input: vecSum (Vector3(1,2,3)) (Vector3(((-1), (-2), (-3))))
Expected Output: Vector3 (0,0,0)
Actual Output: Vector3 (0,0,0)

Function: vecSum
Test Case Number: 2
Input: vecSum (Vector2(0,0)) (Vector2(0,0))
Expected Output: Vector2 (0,0)
Actual Output: Vector2 (0,0)

Function: vecSum
Test Case Number: 3
Input: vecSum (Vector4(20, 40, 60, 80)) (Vector4(3, 5, 7, 8))
Expected Output: Vector4 (23, 45, 67, 88)
Actual Output: Vector4 (23,45,67,88)

Function: vecSum
Test Case Number: 4
Input: vecSum (Vector3(4.893579235, 3.0573409563, (-92.28935723))) (Vector3(32.483573, (-7.53459028), 2.346345720))
Expected Output: Vector3 (37.377152235, (-4.4772493237), (-89.94301151))
Actual Output: Vector3 (37.377152235,-4.4772493237,-89.94301150999999)

Function: vecMagnitude
Test Case Number: 1
Input: vecMagnitude (Vector2(0,0))
Expected Output: 0.0
Actual Output: 0.0

Function: vecMagnitude
Test Case Number: 2
Input: vecMagnitude (Vector3(3, 6, 9))
Expected Output: 11.224972160321824
Actual Output: 11.224972160321824

Function: vecMagnitude
Test Case Number: 3
Input: vecMagnitude (Vector4((-3), (-6), (-9), (-1)))
Expected Output: 11.269427669584
Actual Output: 11.269427669584644

Function: vecMagnitude
Test Case Number: 4
Input: vecMagnitude (Vector3(3.92375428957, (-45.23589725), 49.43692329))
Expected Output: 67.12444884033552
Actual Output: 67.12444884033553

Function: vecF
Test Case Number: 1
Input: vecF (Vector3(1,2,3)) []
Expected Output: []
Actual Output: []

Function: vecF
Test Case Number: 2
Input: vecF (Vector2(0,0)) [Vector2(0,0), Vector2(0,0), Vector2(0,0)]
Expected Output: [0.0,0.0,0.0]
Actual Output: [0.0,0.0,0.0]

Function: vecF
Test Case Number: 3
Input: vecF (Vector4(1,3,9,27)) [Vector4(1, 3, 9, 27), Vector4(0,0,0,0), Vector4(2, 4, 6, 8)]
Expected Output: [0.0, 28.635642, 19.287302]
Actual Output: [0.0,28.635642126552707,19.28730152198591]

Function: vecF
Test Case Number: 4
Input: vecF (Vector3(35.5250294, (-4.23589257), 24.2625235)) [Vector3(346.373456, 23.4363, 36347.252634), Vector3((-32526.37356), 432.6274373, 4.34623738), Vector3(36346.23653, 2.325236, 6.35785378)]
Expected Output: [36324.330731, 32564.835114, 36310.716508]
Actual Output: [36324.33073109596,32564.835113567024,36310.7165077393]

-}






{- ----- Actual Tests ----- -}


-- Determines whether two values can be considered equal (with tolerance equal to 0.0001)
valEquality :: Double -> Double -> Bool
valEquality actual expected = abs(actual - expected) <= 0.0001

-- Determines whether two vectors can be considered equal (with tolerance equal to 0.0001)
vecEquality :: (Ord a, Floating a, VectorSpace v) => v a -> v a -> Bool
vecEquality v1 v2 = abs(vecDistance v1 v2) <= 0.0001

-- Determines whether two lists can be considered equal (with tolderance equal to 0.0001)
listEquality :: [Double] -> [Double] -> Bool
listEquality [] [] = True
listEquality actual expected | (length actual) /= (length expected)          = False
                           | not (valEquality (head actual) (head expected)) = False
                           | otherwise                                       = listEquality (tail actual) (tail expected)


vecScalarProd_1 :: Bool
vecScalarProd_1 = vecEquality actual expected
    where
        actual   = vecScalarProd 5.45 (Vector2(0,0))
        expected = Vector2 (0,0)

vecScalarProd_2 :: Bool
vecScalarProd_2 = vecEquality actual expected
    where
        actual   = vecScalarProd 0 (Vector3(4, 6, 8))
        expected = Vector3 (0,0,0)

vecScalarProd_3 :: Bool
vecScalarProd_3 = vecEquality actual expected
    where
        actual   = vecScalarProd 2 (Vector4(2, 3, 4, 5))
        expected = Vector4(4,6,8,10)

vecScalarProd_4 :: Bool
vecScalarProd_4 = vecEquality actual expected
    where
        actual   = vecScalarProd (-7.3534505) (Vector3(57925.34632, (-64.34957289), 473.348965734))
        expected = Vector3 ((-425951.16685948), 473.19139894276, (-3480.7481887512)) 

vecSum_1 :: Bool
vecSum_1 = vecEquality actual expected
    where
        actual   = vecSum (Vector3(1,2,3)) (Vector3(((-1), (-2), (-3))))
        expected = Vector3(0,0,0)

vecSum_2 :: Bool
vecSum_2 = vecEquality actual expected
    where
        actual   = vecSum (Vector2(0,0)) (Vector2(0,0))
        expected = Vector2(0,0)

vecSum_3 :: Bool
vecSum_3 = vecEquality actual expected
    where
        actual   = vecSum (Vector4(20, 40, 60, 80)) (Vector4(3, 5, 7, 8))
        expected = Vector4(23, 45, 67, 88)

vecSum_4 :: Bool
vecSum_4 = vecEquality actual expected
    where
        actual  = vecSum (Vector3(4.893579235, 3.0573409563, (-92.28935723))) (Vector3(32.483573, (-7.53459028), 2.346345720))
        expected = Vector3 (37.377152235, (-4.4772493237), (-89.94301151))

vecMagnitude_1 :: Bool
vecMagnitude_1 = valEquality actual expected
    where
        actual   = vecMagnitude (Vector2(0,0))
        expected = 0.0

vecMagnitude_2 :: Bool
vecMagnitude_2 = valEquality actual expected
    where
        actual   = vecMagnitude (Vector3(3, 6, 9))
        expected = 11.224972160321824

vecMagnitude_3 :: Bool
vecMagnitude_3 = valEquality actual expected
    where
        actual   = vecMagnitude (Vector4((-3), (-6), (-9), (-1)))
        expected = 11.269427669584

vecMagnitude_4 :: Bool
vecMagnitude_4 = valEquality actual expected
    where
        actual   = vecMagnitude (Vector3(3.92375428957, (-45.23589725), 49.43692329))
        expected = 67.12444884033552

vecF_1 :: Bool
vecF_1 = listEquality actual expected
    where
        v        = (Vector3(1,2,3))
        vs       = []
        actual   = vecF v vs
        expected = []

vecF_2 :: Bool
vecF_2 = listEquality actual expected
    where
        v        = Vector2(0,0)
        vs       = [Vector2(0,0), Vector2(0,0), Vector2(0,0)]
        actual   = vecF v vs
        expected = [0.0, 0.0, 0.0]


vecF_3 :: Bool
vecF_3 = listEquality actual expected
    where
        v        = Vector4(1, 3, 9, 27)
        vs       = [Vector4(1, 3, 9, 27), Vector4(0,0,0,0), Vector4(2, 4, 6, 8)]
        actual   = vecF v vs
        expected = [0.0, 28.635642126552707, 19.28730152198591]

vecF_4 :: Bool
vecF_4 = listEquality actual expected
    where
        v = Vector3(35.5250294, (-4.23589257), 24.2625235)
        vs = [Vector3(346.373456, 23.4363, 36347.252634), Vector3((-32526.37356), 432.6274373, 4.34623738), Vector3(36346.23653, 2.325236, 6.35785378)]
        actual = vecF v vs
        expected = [36324.330731, 32564.835114, 36310.716508]
