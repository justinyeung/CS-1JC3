-- Assignment 5
-- Name: Michael Balas
-- MacID: balasm
-- Date: December 2, 2017


module DefiniteIntegral where
import Test.QuickCheck


-- Computes an approximation of a definite integral using the trapezoidal rule with n partitions and g to represent the function f: R -> R
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
                        | a < b     = - definiteIntegral b a g n
                        | a == b    = 0
                        | otherwise = delta * sum [ (g (x-delta) + g x)/2 | x <- [b, b-delta .. a+delta] ]
                where
                    delta = (b - a) / fromIntegral n

-- Computes the area of a circle of radius r using definiteIntegral
circleArea :: Double -> Double
circleArea r
                | r  < 0    = error "Radius must be a positive value"
                | otherwise = 4 * definiteIntegral 0 r (\x -> sqrt (r^2 - x^2)) 100000


-- Computes the volume of a sphere of radius r using definiteIntegral
sphereVolume :: Double -> Double
sphereVolume r 
                | r  < 0    = error "Radius must be a positive value"
                | otherwise = 2 * definiteIntegral 0 r (\x -> pi*(r^2 - x^2)) 100000


{-    -- Test Plans --

Function: definiteIntegral
Test Case Number: 1
Input: definiteIntegral 0 1 (\x -> x) 1
Expected Output: 0.5
Actual Output:   0.5

Function: definiteIntegral
Test Case Number: 2
Input: definiteIntegral 0 25 (\x-> x^3 / (-3)*x) 120
Expected Output: -651117.0181881416
Actual Output:   -651117.0181881451

Function: definiteIntegral
Test Case Number: 3
Input: definiteIntegral (-20) 0 (\x -> -x^2 * (3/24)) 64
Expected Output: -333.3740234375
Actual Output:   -333.3740234375

Function: circleArea
Test Case Number: 1
Input: circleArea 0
Expected Output: 0.0
Actual Output:   0.0

Function: circleArea
Test Case Number: 2
Input: circleArea 5
Expected Output: 78.539816339745    
Actual Output:   78.53981540999946

Function: circleArea
Test Case Number: 3
Input: circleArea 42.36938457
Expected Output: 5639.676386941    
Actual Output:   5639.676320171552

Function: sphereVolume
Test Case Number: 1
Input: sphereVolume 0
Expected Output: 0.0
Actual Output:   0.0

Function: sphereVolume
Test Case Number: 2
Input: sphereVolume 5
Expected Output: 523.5987755983 
Actual Output:   523.5987755845958

Function: sphereVolume
Test Case Number: 3
Input: sphereVolume 42.36938457
Expected Output: 318599.49025153
Actual Output:   318599.49024242134

-}


a ===> b = (not a) || b

-- Additive Property: When integrating a function over two intervals where the upper bound of the first is the same as
-- the lower bound of the second, the integrands can be combined. 
definiteIntegralProp1 :: Double -> Double -> Double -> Integer -> Bool
definiteIntegralProp1 a c b n = ((b > a) && (b < c) && (n > 0)) ===> (abs (x - x') <= epsilon)
                        where
                            x       = definiteIntegral a c g n
                            x'      = (definiteIntegral a b g n) + (definiteIntegral b c g n)
                            g       = (\x -> x)
                            epsilon = 0.001 -- error bound

-- Constant Multiples Property: Coefficients can be distributed out of the integrand and multiplied afterwards
definiteIntegralProp2 :: Double -> Double -> Double -> Integer -> Bool
definiteIntegralProp2 a b coef n = (n > 0) ===> (abs (x - x') <= epsilon)
                        where
                            x       = definiteIntegral a b g n
                            x'      = coef * definiteIntegral a b g' n
                            g       = (\x -> coef*x)
                            g'      = (\x -> x)
                            epsilon = 0.001 -- error bound


-- If the upper and lower boundaries are the same, the area is 0
definiteIntegralProp3 :: Double -> Integer -> Bool
definiteIntegralProp3 a n = (n > 0) ===> (definiteIntegral a a g n == 0)
                        where
                            g = (\x -> x)

{-    -- Quick Check --

Function: definiteIntegral
Property: definiteIntegralProp1 a c b n = ((b > a) && (b < c) && (n > 0)) ===> ((x - x') <= epsilon)
                        where
                            x       = definiteIntegral a c g n
                            x'      = (definiteIntegral a b g n) + (definiteIntegral b c g n)
                            g       = (\x -> x)
                            epsilon = 0.001 
Actual Test Result: Pass

Function: definiteIntegral
Property: definiteIntegralProp2 a b coef n = (n > 0) ===> ((x - x') <= epsilon)
                        where
                            x       = definiteIntegral a b g n
                            x'      = coef * definiteIntegral a b g' n
                            g       = (\x -> coef*x)
                            g'      = (\x -> x)
                            epsilon = 0.001 -- error bound
Actual Test Result: Pass

Function: definiteIntegral
Property: definiteIntegralProp3 a n = (n > 0) ===> (definiteIntegral a a g n == 0)
                        where
                            g = (\x -> x)
Actual Test Result: Pass
-}