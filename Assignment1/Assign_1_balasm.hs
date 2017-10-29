-- Assignment 1
-- Name: Michael Balas
-- MacID: 400023244
-- Date: September 29, 2017

cubeRoot :: Float -> Float
cubicQ :: Float -> Float -> Float -> Float
cubicR :: Float -> Float -> Float -> Float -> Float
cubicS :: Float -> Float -> Float
cubicT :: Float -> Float -> Float
cubicRealSolution :: Float -> Float -> Float -> Float -> Float

-- Computes the cubic root 
cubeRoot x = if x >= 0 
                  then (x ** (1/3)) 
                  else -((-x) ** (1/3))

-- Computes the value of Q
cubicQ a b c = (3*a*c-(b**2))/(9*(a**2))

-- Computes the value of R
cubicR a b c d = ((9*a*b*c)-(27*(a**2)*(d))-(2*(b**3)))/(54*(a**3))

-- Computes the value of S
cubicS q r = cubeRoot (r + sqrt((q**3)+(r**2)))

-- Computes the value of T
cubicT q r = cubeRoot (r - sqrt((q**3)+(r**2)))

-- Computes the solution
cubicRealSolution a b c d = (cubicS q r) + (cubicT q r) - (b/(3*a))
        where
                q = cubicQ a b c
                r = cubicR a b c d

-- Testing

-- Tolerance value for error
epsilon = 0.001

-- Tests for cases that should return NaN - returns True if NaN, false otherwise
testNaN_1 :: Bool
testNaN_2 :: Bool
testNaN_3 :: Bool

-- Tests for real number solutions when Q^3 + R^2 >= 0 - returns True if solution is correct, false otherwise
testReal_1 :: Bool
testReal_2 :: Bool
testReal_3 :: Bool

testNaN_1 = isNaN (cubicRealSolution 1 0 (-3) 0)
testNaN_2 = isNaN (cubicRealSolution 5 10 (-5) (-10))
testNaN_3 = isNaN (cubicRealSolution 1 3 (-3) (-3))


testReal_1 = (x - x') <= epsilon
        where
                x = cubicRealSolution 67 34 998 24 
                x' = (-0.024067)

testReal_2 = (x - x') <= epsilon
        where
                x = cubicRealSolution 1 3 3 1
                x' = (-1)

testReal_3 = (x - x') <= epsilon
        where
                x = cubicRealSolution 2 (-5) 0 0
                x' = 2.5

