-- Assignment 1 Extra Credit
-- Name: Michael Balas
-- MacID: 400023244
-- Date: September 29, 2017

import Data.Complex

cubeRoot :: RealFloat a => Complex a -> Complex a
cubicQ :: Floating a => a -> a -> a -> a
cubicR :: Floating a => a -> a -> a -> a -> a
cubicComplexS :: RealFloat a => Complex a -> Complex a -> Complex a
cubicComplexT :: RealFloat a => Complex a -> Complex a -> Complex a
cubicRealSolution :: RealFloat a => Complex a -> Complex a -> Complex a -> Complex a -> a

-- Computes the cubic root 
cubeRoot x = 
              if realPart (x) >= 0 
                  then (x ** (1/3)) 
                  else -((-x) ** (1/3))

-- Computes the value of Q
cubicQ a b c = (3*a*c-(b**2))/(9*(a**2))

-- Computes the value of R
cubicR a b c d = ((9*a*b*c)-(27*(a**2)*(d))-(2*(b**3)))/(54*(a**3))

-- Computes the value of S
cubicComplexS q r = cubeRoot (r + (sqrt((q**3)+(r**2))))

-- Computes the value of T
cubicComplexT q r = cubeRoot (r - (sqrt((q**3)+(r**2))))

-- Computes the real approximation of real & complex cubic equations
cubicRealSolution a b c d = realPart ( (cubicComplexS q r) + (cubicComplexT q r) - (b/(3*a)) )
        where
                q = cubicQ a b c
                r = cubicR a b c d


-- Testing

-- Tolerance value for error
epsilon = 0.001

-- Tests for real number solutions when Q^3 + R^2 >= 0 (should give the same solutions as the regular assignment) - returns True if solution is correct, False otherwise
testReal_1 :: Bool
testReal_2 :: Bool
testReal_3 :: Bool

-- Tests for real number approximations when Q^3 + R^2 < 0 - returns True if solution is correct, False otherwise
testComplex_1 :: Bool
testComplex_2 :: Bool
testComplex_3 :: Bool

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


testComplex_1 = (x - x') <= epsilon
        where
                x = cubicRealSolution 1 0 (-3) 0
                x' = 0.0

testComplex_2 = (x - x') <= epsilon
        where
                x = cubicRealSolution 5 10 (-5) (-10)
                x' = 1.0

testComplex_3 = (x - x') <= epsilon
        where
                x = cubicRealSolution 1 3 (-3) (-3)
                x' = (-3.6017)