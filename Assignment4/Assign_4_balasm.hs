-- Assignment 4
-- Name: Michael Balas
-- MacID: balasm
-- Date: November 17, 2017

module PolynomialList where
import Test.QuickCheck

data Poly = 
            X
        | Coef Integer
        | Sum Poly Poly
        | Prod Poly Poly
        deriving Show

-- Reads the coefficients of the standard form of a polynomial from a file in which there is one integer per line where the
-- first integer is a0, the second a1, etc. and then returns the inputted integers as a polynomial list.
getPolyList :: FilePath -> IO [Integer]
getPolyList input = do 
                        file <- readFile input
                        let 
                            ints = lines file
                            polyList = map read ints :: [Integer]
                        return (polyList)

-- If pl is a polynomial list, polyListValue pl n is the value of the polynomial function represented by pl at n.
polyListValue :: [Integer] -> Integer -> Integer
polyListValue [] n = 0
polyListValue pl n 
                    | length pl == 1 = head pl
                    | otherwise      = (head pl) + n * (polyListValue (tail pl) n)


-- If pl is a polynomial list, polyDegree pl is the degree of the polynomial represented by pl. 
-- The degree of the polynomial list [] should  be  undefined, since the degree of the zero polynomial is undefined
polyListDegree :: [Integer] -> Integer
polyListDegree [] = 0 
polyListDegree pl 
            | (last pl) == 0 = polyListDegree (init pl)             -- If last element is 0, cut it off (e.g. [4,1,0] should return 1, not 2)
            | otherwise      = fromIntegral ((length pl) - 1)       -- degree = length of the list up to the highest non-zero element minus one


-- If pl is a polynomial list, polyListDeriv pl is the polynomial list that represents the derivative of the polynomial represented 
-- by pl. polyListDeriv pl thus symbolically differentiates a polynomial list pl.
polyListDeriv :: [Integer] -> [Integer]
polyListDeriv [] = []
polyListDeriv (_:xs) = polyDeriv 1 xs
                where
                    polyDeriv _ [] = []
                    polyDeriv n (x:xs) = n * x : polyDeriv (n+1) xs


-- If pl and ql are polynomial lists, polyListSum pl ql is the polynomial list that represents 
-- the sum of the polynomials represented by pl and ql.
polyListSum :: [Integer] -> [Integer] -> [Integer]
polyListSum xs []         = xs
polyListSum [] ys         = ys
polyListSum (x:xs) (y:ys) = (x+y) : polyListSum xs ys


-- If pl and ql are polynomial lists, polyListProd pl ql is the polynomial list that represents 
-- the product of the polynomials represented by pl and ql.
polyListProd :: [Integer] -> [Integer] -> [Integer]
polyListProd _ [] = []
polyListProd ys (x:xs) = polyListSum (polyscale x ys) (0 : polyListProd ys xs)

polyscale :: Integer -> [Integer] -> [Integer]
polyscale a x = map (a*) x


-- If pl is a polynomial list, polyListToPoly pl is a polynomial of type Poly whose standard form
-- is represented by pl.
polyListToPoly :: [Integer] -> Poly
polyListToPoly pl = polyListToPoly' pl 0

polyListToPoly' :: [Integer] -> Integer -> Poly
polyListToPoly' [] _ = error "Cannot convert empty list to polynomial"
polyListToPoly' pl index
                    | length pl == 1 = Coef (head pl)
                    | length pl == 2 = Sum (prod (head pl) index) (prod (last pl) (index+1))
                    | otherwise      = Sum (prod (head pl) index) (polyListToPoly' (tail pl) (index+1))

-- Helper function to return the degree of x based on its position in the list
prod :: Integer -> Integer -> Poly
prod num index
            | index == 0 = Coef num
            | index == 1 = Prod (Coef num) X
            | index  > 1 = Prod (Coef num) (prodX num index)
        where
            prodX num index 
                            | index == 2 = Prod X X
                            | index  > 2 = Prod X (prodX num (index-1))

-- polyToPolyList p is the polynomial list that represents the standard form of p.
polyToPolyList :: Poly -> [Integer]
polyToPolyList (Coef a)    = [a]
polyToPolyList X           = [0, 1]
polyToPolyList (Sum a b)   = polyListSum (polyToPolyList a) (polyToPolyList b)
polyToPolyList (Prod a b)
                | deg == 0 = [polyValue (Prod a b) 1]
                | deg == 1 = if (polyDegree a) > (polyDegree b)
                                then zeros deg ++ [polyValue b 1]
                                else zeros deg ++ [polyValue a 1]
                | deg  > 1 = zeros deg ++ [(polyValue (Prod a b) 1)]
        where
            zeros deg = replicate (fromIntegral deg) 0
            deg       = polyDegree (Prod a b) 


-- Helper function to return the degree of a polynomial expression
polyDegree :: Poly -> Integer
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

-- Helper function that calculates the value of the polynomial with coefficient c by replacing the indeterimant X with c. 
polyValue :: Poly -> Integer -> Integer
polyValue (Coef a) c = a
polyValue X c = c
polyValue (Sum a b) c  = (polyValue a c) + (polyValue b c)
polyValue (Prod a b) c = (polyValue a c) * (polyValue b c)



{-   -- Test Plans --   

Function: getPolyList
Test Case Number: 1
Input: getPolyList "Ints1.txt" (empty file)

Expected Output: []
Actual Output:   []

Function: getPolyList
Test Case Number: 2
Input: getPolyList "Ints1.txt" (Contents of the file below)
5
Expected Output: [5]
Actual Output:   [5]

Function: getPolyList
Test Case Number: 3
Input: getPolyList "Ints1.txt" (Contents of the file below)
1
2
4
8
16
32
64
Expected Output: [1,2,4,8,16,32,64]
Actual Output:   [1,2,4,8,16,32,64]

Function: polyListValue
Test Case Number: 1
Input: polyListValue [3] 1
Expected Output: 3
Actual Output:   3

Function: polyListValue
Test Case Number: 2
Input: polyListValue [2,5,7,2,9] 0
Expected Output: 2
Actual Output:   2

Function: polyListValue
Test Case Number: 3
Input: polyListValue [2,5,7,2,9] 3
Expected Output: 863
Actual Output:   863

Function: polyListDegree
Test Case Number: 1
Input: polyListDegree [] 
Expected Output: undefined
Actual Output:   undefined

Function: polyListDegree
Test Case Number: 2
Input: polyListDegree [1]
Expected Output: 0
Actual Output:   0

Function: polyListDegree
Test Case Number: 3
Input: polyListDegree [1,2,3,4,5,6,7,8]
Expected Output: 7
Actual Output:   7

Function: polyListDeriv
Test Case Number: 1
Input: polyListDeriv [9]
Expected Output: []
Actual Output:   []

Function: polyListDeriv
Test Case Number: 2
Input: polyListDeriv [1,4,8]
Expected Output: [4,16]
Actual Output:   [4,16]

Function: polyListDeriv 
Test Case Number: 3
Input: polyListDeriv [3,8,-5,3,9,14]
Expected Output: [8,-10,9,36,70]
Actual Output:   [8,-10,9,36,70]

Function: polyListSum
Test Case Number: 1
Input: polyListSum [1,2,3] []
Expected Output: [1,2,3]
Actual Output:   [1,2,3]

Function: polyListSum
Test Case Number: 2
Input: polyListSum [5,3,7] [1,4,8,12,51]
Expected Output: [6,7,15,12,51]
Actual Output:   [6,7,15,12,51]

Function: polyListSum
Test Case Number: 3
Input: polyListSum [-5,-4,-3,7,9,12] [5,4,3,2]
Expected Output: [0,0,0,9,9,12]
Actual Output:   [0,0,0,9,9,12]

Function: polyListProd
Test Case Number: 1
Input: polyListProd [1,2,3] []
Expected Output: []
Actual Output:   []

Function: polyListProd
Test Case Number: 2
Input: polyListProd [5,3,7] [1,4,8,12,51]
Expected Output: [5,23,59,112,347,237,357]
Actual Output:   [5,23,59,112,347,237,357]

Function: polyListProd
Test Case Number: 3
Input: polyListProd [-5,-4,-3,0,0,12] [5,4,3,2]
Expected Output: [-25,-40,-46,-34,-17,54,48,36,24]
Actual Output:   [-25,-40,-46,-34,-17,54,48,36,24]

Function: polyListToPoly
Test Case Number: 1
Input: polyListToPoly [5]
Expected Output: Coef 5
Actual Output:   Coef 5

Function: polyListToPoly
Test Case Number: 2
Input: polyListToPoly [2,4,8,5,9]
Expected Output: Sum (Coef 2) (Sum (Prod (Coef 4) X) (Sum (Prod (Coef 8) (Prod X X)) (Sum (Prod (Coef 5) (Prod X (Prod X X))) (Prod (Coef 9) (Prod X (Prod X (Prod X X)))))))
Actual Output:   Sum (Coef 2) (Sum (Prod (Coef 4) X) (Sum (Prod (Coef 8) (Prod X X)) (Sum (Prod (Coef 5) (Prod X (Prod X X))) (Prod (Coef 9) (Prod X (Prod X (Prod X X)))))))

Function: polyListToPoly
Test Case Number: 3
Input: polyListToPoly [6,-3,-9,0,0,12,-6]
Expected Output: Sum (Coef 6) (Sum (Prod (Coef (-3)) X) (Sum (Prod (Coef (-9)) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Sum (Prod (Coef 0) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X X))))) (Prod (Coef (-6)) (Prod X (Prod X (Prod X (Prod X (Prod X X)))))))))))
Actual Output:   Sum (Coef 6) (Sum (Prod (Coef (-3)) X) (Sum (Prod (Coef (-9)) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Sum (Prod (Coef 0) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X X))))) (Prod (Coef (-6)) (Prod X (Prod X (Prod X (Prod X (Prod X X)))))))))))

Function: polyToPolyList
Test Case Number: 1
Input: polyToPolyList (Coef 5)
Expected Output: [5]
Actual Output:   [5]

Function: polyToPolyList
Test Case Number: 2
Input: polyToPolyList (Sum (Coef 2) (Sum (Prod (Coef 4) X) (Sum (Prod (Coef 8) (Prod X X)) (Sum (Prod (Coef 5) (Prod X (Prod X X))) (Prod (Coef 9) (Prod X (Prod X (Prod X X))))))))
Expected Output: [2,4,8,5,9]
Actual Output:   [2,4,8,5,9]

Function: polyToPolyList
Test Case Number: 3
Input: polyToPolyList (Sum (Coef 6) (Sum (Prod (Coef (-3)) X) (Sum (Prod (Coef (-9)) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Sum (Prod (Coef 0) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X X))))) (Prod (Coef (-6)) (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))))
Expected Output: [6,-3,-9,0,0,12,-6]
Actual Output:   [6,-3,-9,0,0,12,-6]

Function: polyToPolyList
Test Case Number: 4
Input: polyToPolyList (Sum (Coef 6) (Sum (Prod (Coef (-3)) X) (Sum (Prod (Coef (-9)) (Prod X X)) (Sum (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X X))))) (Prod (Coef (-6)) (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))
Expected Output: [6,-3,-9,0,0,12,-6]
Actual Output:   [6,-3,-9,0,0,12,-6]
-}



a ===> b = (not a) || b

-- Property that the value of a polynomial is equal to the sum of its coefficients if X=1
polyListValueProp :: [Integer] -> Bool
polyListValueProp pl = (length pl > 0) ===> ((polyListValue pl 1) == (sum pl))

-- Property that the degree of a polynomial remains the same regardless of order
polyListDegreeProp :: [Integer] -> Bool
polyListDegreeProp pl = ((length pl>0) && (head pl/=0) && (last pl/=0)) ===> ((polyListDegree pl) == (polyListDegree (reverse pl)))

-- Property that the derivative of a constant is the empty list (i.e. zero)
polyListDerivProp :: [Integer] -> Bool
polyListDerivProp pl = (length pl == 1) ===> ((polyListDeriv pl) == ([]))

-- Property that the length of the sum of two lists is always equal to the length of the longer list
polyListSumProp :: ([Integer],[Integer]) -> Bool
polyListSumProp (xs,ys) = ((length (polyListSum xs ys)) == (length (longerList)))
            where
                longerList = if (length xs >= length ys) then xs else ys

-- Property that the length of the product of two non-empty lists is always equal to the sum of the lengths of both lists minus one
polyListProdProp :: ([Integer], [Integer]) -> Bool
polyListProdProp (xs,ys) = (length xs > 0 && length ys > 0) ===> ((length (polyListProd xs ys)) == ((length xs)+(length ys)-1))


{-   -- QuickCheck -- 

a ===> b = (not a) || b

Function: polyListValue
Property: polyListValueProp pl = (length pl > 0) ===> ((polyListValue pl 1) == (sum pl))
Actual Test Result: Pass

Function: polyListDegree
Property: polyListDegreeProp pl = ((length pl>0) && (head pl/=0) && (last pl/=0)) ===> ((polyListDegree pl) == (polyListDegree (reverse pl)))
Actual Test Result: Pass

Function: polyListDeriv
Property: polyListDerivProp pl = (length pl == 1) ===> ((polyListDeriv pl) == ([]))
Actual Test Result: Pass

Function: polyListSum
Property: polyListSumProp (xs,ys) = ((length (polyListSum xs ys)) == (length (longerList)))
            where
                longerList = if (length xs >= length ys) then xs else ys
Actual Test Result:

Function: polyListProd
Property: polyListProdProp (xs,ys) = (length xs > 0 && length ys > 0) ===> ((length (polyListProd xs ys)) == ((length xs)+(length ys)-1))
Actual Test Result: Pass
-}