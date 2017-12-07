-- Assignment 3 Extra Credit
-- Name: Michael Balas
-- MacID: balasm
-- Date: November 3, 2017

module Polynomial where

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
            deriving Show

-- The value of the polynomial with coefficient c is calculated by replacing the indeterimant X with c. 
polyValue :: Num a => Poly a -> a -> a
polyValue (Coef a) c = a
polyValue X c = c
polyValue (Sum a b) c  = (polyValue a c) + (polyValue b c)
polyValue (Prod a b) c = (polyValue a c) * (polyValue b c) 



-- The degree of the polynomial is the largest exponent appearing in the standard (expanded) form of the polynomial.
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree (Coef a) = 0
polyDegree X = 1
polyDegree (Sum a b)
                    | (polyDegree a) >= (polyDegree b) = polyDegree a       -- Check which side of the summation has a greater degree
                    | otherwise                        = polyDegree b
polyDegree (Prod a b) 
                    | (polyDegree a)==0 && (polyDegree b)==0 = 0
                    | (polyDegree a)==0 && (polyDegree b)/=0 = if (polyValue a 1)==0 then 0 else polyDegree b
                    | (polyDegree a)/=0 && (polyDegree b)==0 = if (polyValue b 1)==0 then 0 else polyDegree a
                    | otherwise                              = (polyDegree a) + (polyDegree b)  -- The degree is additive through multiplication

polyDeriv :: (Num a, Eq a) => Poly a -> Poly a
polyDeriv (Coef a) = (Coef 0)
polyDeriv X = (Coef 1)
polyDeriv (Sum a b) = Sum (polyDeriv a) (polyDeriv b)                       -- Take the derivative of each side of the summation seperately
polyDeriv (Prod a b) = Prod (Coef (coefficient1*coefficient2*degree)) (reducePower (Prod a b))  -- derivative=degree*(the coefficients multiplying X)*X^(n-1)
    where
        coefficient1 = polyValue a 1 -- Convert the polynomial to a value ignoring X (hence X=1)
        coefficient2 = polyValue b 1 -- Convert the polynomial to a value ignoring X (hence X=1)
        degree = fromIntegral(polyDegree (Prod a b))  

-- Helper function for polyDeriv: Subtracts 1 from the exponent of a term (e.g. x^4 => x^3)
reducePower :: (Num a, Eq a) => Poly a -> Poly a
reducePower (Prod a b)
            | (polyDegree a == 0) && (polyDegree b == 0) = (Coef 0)         -- Constant * Constant          => 0
            | (polyDegree a /= 0) && (polyDegree b == 0) = (Coef 1)         -- X * Constant                 => 1
            | (polyDegree a == 0) && (polyDegree b == 1) = (Coef 1)         -- Constant * X                 => 1
            | (polyDegree a == 0) && (polyDegree b > 1) = reducePower b     -- Constant * X^n = X * X^(n-1) => X^(n-1)
            | otherwise                                  = b                -- X^n = X * X^(n-1)            => X^(n-1)

-- Computes (using Newton's method with the seeds) a number such that polyValue p n is approximately 0. 
-- polyNewton p thus solves the polynomial equation p=0.
polyNewton :: (Fractional a, Ord a) => Poly a -> a -> a
polyNewton (Coef a) seed = error "Cannot compute polyNewton on a constant"
polyNewton X seed = 0
polyNewton (Sum a b) seed = let newSeed = seed - ((poly) / (poly'))
                            in if (abs(newSeed - seed) < 1e-9)  -- 1e-9 is the tolerance (epsilon)
                                then newSeed              
                                else polyNewton (Sum a b) newSeed
                            where
                                poly  = polyValue (Sum a b) seed
                                poly' = polyValue (polyDeriv (Sum a b)) seed
polyNewton (Prod a b) seed = let newSeed = seed - ((poly) / (poly'))
                            in if (abs(newSeed - seed) < 1e-9) -- 1e-9 is the tolerance (epsilon)
                                then newSeed
                                else polyNewton (Prod a b) newSeed
                            where
                                poly  = polyValue (Prod a b) seed
                                poly' = polyValue (polyDeriv (Prod a b)) seed



-- Transforms a polynomial into its standard form in order of decreasing (largest power to smallest) order
polyAsList :: (Num a, Eq a) => Poly a -> [a]
polyAsList (Coef a) = [a]
polyAsList X        = [1]
polyAsList (Sum a b) 
                    | (polyDegree a) > (polyDegree b) = polyAsList(a)++x++polyAsList(b)++y  -- Check which side of the summation has a greater degree
                    | otherwise                       = polyAsList(b)++x++polyAsList(a)++y  -- The side with the greater degree goes in the list first
            where
                x = if ((polyDegree a) - (polyDegree b))>1 then replicate (fromIntegral((polyDegree a) - (polyDegree b)-1)) 0 else []  -- Places zeros in areas of missing powers
                y = if ((polyDegree a) - (polyDegree b))>1 then [0] else []                                                            -- Places zeros in areas of missing powers
polyAsList (Prod a b)
                    | (polyDegree a == 0) && (polyDegree b == 0) = [(polyValue a 1)*(polyValue b 1)] -- If both sides are coefficients (not X), return their product
                    | (polyDegree a == 0) && (polyDegree b /= 0) = [polyValue a 1]    -- If the left side is the coefficient (deg=0), return it
                    | (polyDegree a /= 0) && (polyDegree b == 0) = [polyValue b 1]    -- If the right side is the coefficient (deg=0), return it
                    | otherwise                                  = [1]                -- If there is no explicit coefficient (deg/=0), return 1

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

Function: polyValue
Test Case Number: 4
Input: polyValue (Sum (Prod X X) (Sum (Prod (Coef 7) X) (Coef 8))) 3.3
Expected Output: 41.99
Actual Output: 41.989999999999995

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

Function: polyDegree
Test Case Number: 4
Input: polyDegree (Prod (Sum (Coef 5) (Coef (-5))) (Prod X X))
Expected Output: 0
Actual Output: 0

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

Function: polyDeriv
Test Case Number: 4
Input: polyDeriv (Sum (Prod (Coef 3) (Prod X X)) (Sum (Coef 5) (Sum (Prod (Coef 2.0) (Prod X X)) (Sum (Prod (Coef 5.4) (Prod X (Prod X X))) (Sum (Coef 7.3) (Prod (Coef (-4.5)) X))))))
Expected Output: Sum (Prod (Coef 6.0) X) (Sum (Coef 0.0) (Sum (Prod (Coef 4.0) X) (Sum (Prod (Coef 16.2) (Prod X X)) (Sum (Coef 0.0) (Prod (Coef (-4.5)) (Coef 1.0))))))
Actual Output:   Sum (Prod (Coef 6.0) X) (Sum (Coef 0.0) (Sum (Prod (Coef 4.0) X) (Sum (Prod (Coef 16.200000000000003) (Prod X X)) (Sum (Coef 0.0) (Prod (Coef (-4.5)) (Coef 1.0))))))

Function: polyNewton
Test Case Number: 1
Input: polyNewton (Coef 5) 1
Expected Output: 0.0
Actual Output: 0.0

Function: polyNewton
Test Case Number: 2
Input: polyNewton X 1
Expected Output: 0.0
Actual Output: 0.0

Function: polyNewton
Test Case Number: 3
Input: polyNewton (Sum (Prod X X) (Sum (Prod (Coef 2) X) (Coef (-24)))) 1
Expected Output: 4.0
Actual Output: 4.0

Function: polyNewton
Test Case Number: 4
Input: polyNewton (Sum (Prod X (Prod X (Prod X X))) (Sum (Prod (Coef (-5.6)) (Prod X (Prod X X))) (Sum (Prod (Coef (-89.39)) (Prod X X)) (Sum (Prod (Coef 388.074) X) (Coef 1269.324))))) 5
Expected Output: 7.3
Actual Output: 7.299999999999998

Function: polyAsList
Test Case Number: 1
Input: polyAsList (Coef 12)
Expected Output: [12]
Actual Output: [12]

Function: polyAsList
Test Case Number: 2
Input: polyAsList X
Expected Output: [1]
Actual Output: [1]

Function: polyAsList
Test Case Number: 3
Input: polyAsList (Sum (Coef 6) (Sum (Prod (Coef 7) (X)) (Sum (Prod (Coef 4) (Prod X (Prod X X))) (Prod (Coef 2) (Prod X X)))))
Expected Output: (4,2,7,6)
Actual Output:

Function: polyAsList
Test Case Number: 4
Input: polyAsList (Sum (Prod X (Prod X X)) (Prod (Coef 5.6) X))
Expected Output: [1.0,0.0,5.6,0.0]
Actual Output: [1.0,0.0,5.6,0.0]

-}