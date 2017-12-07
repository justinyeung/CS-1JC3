-- Assignment 3
-- Name: Michael Balas
-- MacID: balasm
-- Date: November 3, 2017

module Polynomial where

data Poly = X
            | Coef Integer
            | Sum Poly Poly
            | Prod Poly Poly
            deriving Show


-- The value of the polynomial with coefficient c is calculated by replacing the indeterimant X with c. 
polyValue :: Poly -> Integer -> Integer
polyValue (Coef a) _   = a
polyValue X c          = c
polyValue (Sum a b) c  = (polyValue a c) + (polyValue b c)
polyValue (Prod a b) c = (polyValue a c) * (polyValue b c) 


-- The degree of the polynomial is the largest exponent appearing in the standard (expanded) form of the polynomial.
polyDegree :: Poly -> Integer
polyDegree (Coef a) = 0
polyDegree X        = 1
polyDegree (Sum a b)
                    | (polyDegree a) >= (polyDegree b) = polyDegree a   -- Check which side of the summation has a greater degree
                    | otherwise                        = polyDegree b
polyDegree (Prod a b) = (polyDegree a) + (polyDegree b)                 -- The degree is additive through multiplication


-- polyDeriv represents the derivative of the polynomial function.
polyDeriv :: Poly -> Poly
polyDeriv (Coef a)   = (Coef 0)
polyDeriv X          = (Coef 1)
polyDeriv (Sum a b)  = Sum (polyDeriv a) (polyDeriv b)                                         -- Take the derivative of each side of the summation seperately
polyDeriv (Prod a b) = Prod (Coef (coefficient1*coefficient2*degree)) (reducePower (Prod a b)) -- derivative=degree*(the coefficients multiplying X)*X^(n-1)
    where
        coefficient1 = polyValue a 1              -- Converts a polynomial to a value ignoring X (hence X=1)
        coefficient2 = polyValue b 1              -- Converts a polynomial to a value ignoring X (hence X=1)
        degree       = polyDegree (Prod a b)      -- The total degree of the expression

-- Helper function for polyDeriv: Subtracts 1 from the exponent of a term (e.g. x^4 => x^3)
reducePower :: Poly -> Poly
reducePower (Prod a b)
            | (polyDegree a == 0) && (polyDegree b == 0) = (Coef 0)         -- Constant * Constant          => 0
            | (polyDegree a /= 0) && (polyDegree b == 0) = (Coef 1)         -- X * Constant                 => 1
            | (polyDegree a == 0) && (polyDegree b == 1) = (Coef 1)         -- Constant * X                 => 1
            | (polyDegree a == 0) && (polyDegree b > 1)  = reducePower b    -- Constant * X^n = X * X^(n-1) => X^(n-1)
            | otherwise                                  = b                -- X^n = X * X^(n-1)            => X^(n-1)

{- --- Test Plans --- 

Function: polyValue
Test Case Number: 1
Input: polyValue (Coef 5) 23
Expected Output: 5
Actual Output: 5

Function: polyValue
Test Case Number: 2
Input: polyValue X 23
Expected Output: 23
Actual Output: 23

Function: polyValue
Test Case Number: 3
Input: polyValue (Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod (Coef 6) X) (Coef 6))) 7
Expected Output: 195
Actual Output: 195

Function: polyDegree
Test Case Number: 1
Input: polyDegree (Coef 42)
Expected Output: 0
Actual Output: 0

Function: polyDegree
Test Case Number: 2
Input: polyDegree X
Expected Output: 1
Actual Output: 1

Function: polyDegree 
Test Case Number: 3
Input: polyDegree (Sum (Coef 7) (Sum (Prod (Coef 9) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 2) (Prod X (Prod X X))) (X))))
Expected Output: 4
Actual Output: 4

Function: polyDeriv
Test Case Number: 1
Input: polyDeriv (Coef 10)
Expected Output: Coef 0
Actual Output: Coef 0

Function: polyDeriv
Test Case Number: 2
Input: polyDeriv X
Expected Output: Coef 1
Actual Output: Coef 1

Function: polyDeriv
Test Case Number: 3
Input: polyDeriv (Sum (Prod (Coef 2) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 5) (Prod X (Prod X X))) (Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod X (Coef 9)) (Coef 7)))))
Expected Output: Sum (Prod (Coef 8) (Prod X (Prod X X))) (Sum (Prod (Coef 15) (Prod X X)) (Sum (Prod (Coef 6) X) (Sum (Prod (Coef 9) (Coef 1)) (Coef 0))))
Actual Output:   Sum (Prod (Coef 8) (Prod X (Prod X X))) (Sum (Prod (Coef 15) (Prod X X)) (Sum (Prod (Coef 6) X) (Sum (Prod (Coef 9) (Coef 1)) (Coef 0))))

-}