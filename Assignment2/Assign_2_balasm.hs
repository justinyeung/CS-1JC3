-- Assignment 2
-- Name: Michael Balas
-- MacID: balasm
-- Date: October 20, 2017

module VectorSpace where

-- Represents a point in 3-dimensional Euclidean space
type Vector = (Double, Double, Double)

-- Implements the zero vector constant
vecZero :: Vector
vecZero = (0, 0, 0)


{- -----  Required Functions ----- -}

-- Implements the scalar product function 
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd r (x, y, z) = (r*x, r*y, r*z)

-- Implements the sum function
vecSum :: Vector -> Vector -> Vector
vecSum (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

-- Implements the magnitude function
vecMagnitude :: Vector -> Double
vecMagnitude (x, y, z) = sqrt((x^2)+(y^2)+(z^2))

-- Returns a list of distances between a given vector, v, and a list of vectors, vs. 
vecF :: Vector -> [Vector] -> [Double]
vecF (x,y,z) vectors = map (vecDistance (x,y,z)) vectors


{- ----- Helper Functions ----- -}

-- Calculates the difference between two vectors
vecDifference :: Vector -> Vector -> Vector
vecDifference (x1, y1, z1) (x2, y2, z2) = vecSum (x1, y1, z1) (vecScalarProd (-1) (x2, y2, z2))

-- Calculates the distance between two vectors
vecDistance :: Vector -> Vector -> Double
vecDistance (x1, y1, z1) (x2, y2, z2) = vecMagnitude (vecDifference (x1, y1, z1) (x2, y2, z2))


{- ----- Test Plans ----- -}
{-
Function: vecScalarProd
Test Case Number: 1
Input: vecScalarProd 5.45 vecZero
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)

Function: vecScalarProd
Test Case Number: 2
Input: vecScalarProd 0 (4,6,8)
Expected Output: (0.0, 0.0, 0.0)
Actual Output: (0.0, 0.0, 0.0)

Function: vecScalarProd
Test Case Number: 3
Input: vecScalarProd 2 (2,3,4)
Expected Output: (4.0,6.0,8.0)
Actual Output: (4.0,6.0,8.0)

Function: vecScalarProd
Test Case Number: 4
Input: vecScalarProd (-7.3534505) (57925.34632, (-64.34957289), 473.348965734)
Expected Output: ((-425951.16685948), 473.19139894276, (-3480.7481887512))
Actual Output: (-425951.1668594771,473.19139894275696,-3480.7481887511653)

Function: vecSum
Test Case Number: 1
Input: vecSum (1,2,3) ((-1), (-2), (-3))
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)

Function: vecSum
Test Case Number: 2
Input: vecSum vecZero vecZero
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)

Function: vecSum
Test Case Number: 3
Input: vecSum (20, 40, 60) (3, 5, 7)
Expected Output: (23.0,45.0,67.0)
Actual Output: (23.0,45.0,67.0)

Function: vecSum
Test Case Number: 4
Input: vecSum (4.893579235, 3.0573409563, (-92.28935723)) (32.483573, (-7.53459028), 2.346345720)
Expected Output: (37.377152235,-4.4772493237,-89.94301151)
Actual Output: (37.377152235,-4.4772493237,-89.94301150999999)

Function: vecMagnitude
Test Case Number: 1
Input: vecMagnitude vecZero
Expected Output: 0.0
Actual Output: 0.0

Function: vecMagnitude
Test Case Number: 2
Input: vecMagnitude (3, 6, 9)
Expected Output: 11.224972160321824
Actual Output: 11.224972160321824

Function: vecMagnitude
Test Case Number: 3
Input: vecMagnitude ((-3), (-6), (-9))
Expected Output: 11.224972160321824
Actual Output: 11.224972160321824

Function: vecMagnitude
Test Case Number: 4
Input: vecMagnitude (3.92375428957, (-45.23589725), 49.43692329)
Expected Output: 67.12444884033552
Actual Output: 67.12444884033553

Function: vecF
Test Case Number: 1
Input: vecF (1,2,3) []
Expected Output: []
Actual Output: []

Function: vecF
Test Case Number: 2
Input: vecF vecZero [vecZero, vecZero, vecZero]
Expected Output: [0.0,0.0,0.0]
Actual Output: [0.0,0.0,0.0]

Function: vecF
Test Case Number: 3
Input: vecF (1,3,9) [(1,3,9),vecZero,(2,4,6)]
Expected Output: [0.0,9.9.53939201417,3.31662479036]
Actual Output: [0.0,9.539392014169456,3.3166247903554]

Function: vecF
Test Case Number: 4
Input: vecF (35.5250294,(-4.23589257),24.2625235) [(346.373456,23.4363,36347.252634),((-32526.37356),432.6274373,4.34623738),(36346.23653,2.325236,6.35785378)]
Expected Output: [36324.3307311, 32564.8351136, 36310.7165077]
Actual Output: [36324.33073109596,32564.835113567024,36310.7165077393]

-}




{- ----- Actual Tests ----- -}

-- Amount of tolerance between values
epsilon = 0.0001

-- Determines whether two values can be considered equal (with tolerance equal to epsilon)
valEquality :: Double -> Double -> Bool
valEquality actual expected = abs(actual - expected) <= epsilon

-- Determines whether two vectors can be considered equal (with tolerance equal to epsilon)
vecEquality :: Vector -> Vector -> Bool
vecEquality (x1, y1, z1) (x2, y2, z2) = valEquality x2 x1
                                     && valEquality y2 y1
                                     && valEquality z2 z1


-- Determines whether two lists can be considered equal (with tolderance equal to epsilon)
listEquality :: [Double] -> [Double] -> Bool
listEquality [] [] = True
listEquality actual expected | (length actual) /= (length expected)          = False
                           | not (valEquality (head actual) (head expected)) = False
                           | otherwise                                       = listEquality (tail actual) (tail expected)

vecScalarProd_1 :: Bool
vecScalarProd_1 = vecEquality actual expected
    where
        actual   = vecScalarProd 5.45 vecZero
        expected = vecZero

vecScalarProd_2 :: Bool
vecScalarProd_2 = vecEquality actual expected
    where
        actual   = vecScalarProd 0 (4, 6, 8)
        expected = vecZero

vecScalarProd_3 :: Bool
vecScalarProd_3 = vecEquality actual expected
    where
        actual   = vecScalarProd 2 (2, 3, 4)
        expected = (4, 6, 8)

vecScalarProd_4 :: Bool
vecScalarProd_4 = vecEquality actual expected
    where
        actual   = vecScalarProd (-7.3534505) (57925.34632, (-64.34957289), 473.348965734)
        expected = ((-425951.16685948), 473.19139894276, (-3480.7481887512))

vecSum_1 :: Bool
vecSum_1 = vecEquality actual expected
    where
        actual   = vecSum (1,2,3) ((-1), (-2), (-3))
        expected = vecZero

vecSum_2 :: Bool
vecSum_2 = vecEquality actual expected
    where
        actual   = vecSum vecZero vecZero
        expected = vecZero

vecSum_3 :: Bool
vecSum_3 = vecEquality actual expected
    where
        actual   = vecSum (20, 40, 60) (3, 5, 7) 
        expected = (23, 45, 67)

vecSum_4 :: Bool
vecSum_4 = vecEquality actual expected
    where
        actual  = vecSum (4.893579235, 3.0573409563, (-92.28935723)) (32.483573, (-7.53459028), 2.346345720)
        expected = (37.377152235, (-4.4772493237), (-89.94301151))

vecMagnitude_1 :: Bool
vecMagnitude_1 = valEquality actual expected
    where
        actual   = vecMagnitude vecZero
        expected = 0.0

vecMagnitude_2 :: Bool
vecMagnitude_2 = valEquality actual expected
    where
        actual   = vecMagnitude (3, 6, 9)
        expected = 11.224972160321824

vecMagnitude_3 :: Bool
vecMagnitude_3 = valEquality actual expected
    where
        actual   = vecMagnitude ((-3), (-6), (-9))
        expected = 11.224972160321824

vecMagnitude_4 :: Bool
vecMagnitude_4 = valEquality actual expected
    where
        actual   = vecMagnitude (3.92375428957, (-45.23589725), 49.43692329)
        expected = 67.12444884033552

vecF_1 :: Bool
vecF_1 = listEquality actual expected
    where
        v        = (1,2,3)
        vs       = []
        actual   = vecF v vs
        expected = []

vecF_2 :: Bool
vecF_2 = listEquality actual expected
    where
        v        = vecZero
        vs       = [vecZero, vecZero, vecZero]
        actual   = vecF v vs
        expected = [0.0, 0.0, 0.0]


vecF_3 :: Bool
vecF_3 = listEquality actual expected
    where
        v        = (1, 3, 9)
        vs       = [(1, 3, 9), vecZero, (2, 4, 6)]
        actual   = vecF v vs
        expected = [0.0, 9.539392, 3.316625]

vecF_4 :: Bool
vecF_4 = listEquality actual expected
    where
        v = (35.5250294, (-4.23589257), 24.2625235)
        vs = [(346.373456, 23.4363, 36347.252634), ((-32526.37356), 432.6274373, 4.34623738), (36346.23653, 2.325236, 6.35785378)]
        actual = vecF v vs
        expected = [36324.330731, 32564.835114, 36310.716508]
