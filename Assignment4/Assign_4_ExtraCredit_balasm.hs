-- Assignment 4 Extra Credit
-- Name: Michael Balas
-- MacID: balasm
-- Date: November 17, 2017


module PolyDiff where
import Test.QuickCheck

data Poly a = 
        X
    | Coef a
    | Sum (Poly a) (Poly a)
    | Prod (Poly a) (Poly a)
--  deriving Show

-- Using polyPrettyPrint, the file defines Poly Integer as an instance of the type class Show.
instance (Show a, Num a, Eq a) => Show (Poly a) where
    show pl = polyPrettyPrint pl


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

-- Transforms a polynomial into its standard form in decreasing order (largest power to smallest)
polyAsList :: (Num a, Eq a) => Poly a -> [a]
polyAsList p = reverse (polyToPolyList p)

-- Helper Function: polyToPolyList p is the polynomial list that represents the standard form of p.
polyToPolyList :: (Num a, Eq a) => Poly a -> [a]
polyToPolyList (Coef a)    = [a]
polyToPolyList X           = [1, 0]
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


-- Helper Function: If pl and ql are polynomial lists, polyListSum pl ql is the polynomial list 
-- that represents the sum of the polynomials represented by pl and ql.
polyListSum :: (Num a, Eq a) => [a] -> [a] -> [a]
polyListSum xs []         = xs
polyListSum [] ys         = ys
polyListSum (x:xs) (y:ys) = (x+y) : polyListSum xs ys


-- takes a string like "1 + 2x + 5x^2" as input and returns the member of the type 
-- Poly Integer that the string represents as output.
polyParse :: String -> Poly Integer
polyParse s  = polyParse' (removeSpaces s)

polyParse' :: String -> Poly Integer
polyParse' []   = undefined
polyParse' cs
            | isNum cs         = tail1 cs
            | otherwise        = expo cs

expo :: String -> Poly Integer
expo (x:[]) = tail1 [x]
expo cs
        | isDigit (head cs) = if afterX == '^'
                                then if afterExponent1 == '+'
                                    then Sum (Prod (coef) (power exponent1)) (tail1 afterPlus)
                                    else Prod (coef) (power (toNum (toSubString (tail (consumeToken (consumeToken cs))))))
                                else if afterX == '+'
                                    then Sum (Prod (coef) (X)) (tail1 (tail (consumeToken (consumeToken cs))))
                                    else Prod (coef) (X)
        | (head cs) == 'x'  = if head (consumeToken cs) == '^'
                                then if afterExponent2 == '+'
                                    then Sum (Prod (X) (power (exponent2-1))) (tail1 (tail (consumeToken (tail (consumeToken cs)))))
                                    else Prod (X) (power (exponent2-1))
                                else Sum X (tail1 (tail (consumeToken cs)))
        | otherwise = tail1 cs
                    where
                        afterX         = if (length (consumeToken (consumeToken cs)) /= 0) 
                                            then head (consumeToken (consumeToken cs)) 
                                            else '!'
                        afterExponent1 = if (length (consumeToken (tail (consumeToken (consumeToken cs)))) /= 0) 
                                            then head (consumeToken(tail (consumeToken (consumeToken cs)))) 
                                            else '!'
                        afterExponent2 = if length (consumeToken (tail ((consumeToken cs)))) /= 0
                                            then (head (consumeToken (tail (consumeToken cs))))
                                            else '!'
                        afterPlus      = tail (consumeToken(tail (consumeToken (consumeToken cs))))
                        coef           = Coef (toNum (toSubString cs))
                        exponent1      = toNum (toSubString (tail (consumeToken (consumeToken (cs)))))
                        exponent2      = toNum (toSubString (tail (consumeToken cs)))


tail1 :: String -> Poly Integer
tail1 cs
        | length (consumeToken cs) == 0 = if isNum cs then Coef (toNum cs) else X
        | isNum cs                      = if isNum (tail (consumeToken cs))
                                            then Sum (Coef (toNum (toSubString cs))) (tail1 (tail (consumeToken cs)))
                                            else Sum (Coef (toNum (toSubString cs))) (expo (tail (consumeToken cs)))
        | otherwise                     = expo cs

consumeToken :: String -> String
consumeToken cs
                | isDigit (head cs) = drop (length (toSubString cs)) cs
                | (head cs) == 'x'  = tail cs

isNum :: String -> Bool
isNum s 
        | length (toSubString s) == length s = True
        | s!!(length (toSubString s)) == '+' = True
        | otherwise                          = False

toNum :: String -> Integer
toNum cs 
        | (length cs > 0 && head cs == '-') = (-1) * (if (length (tail cs) > 0) then (toNum (tail cs)) else 1)
        | length cs > 0                     = (charToInteger (head cs)) * fromIntegral(10 ^ (position)) + (toNum (tail cs))
        | otherwise                         = 0
    where
        position = length cs - 1

toSubString :: String -> String
toSubString [] = []
toSubString s
            | isDigit (head s) = subString ++ [(head s)] ++ (toSubString (tail s))
            | otherwise        = subString
    where
        subString = []


removeSpaces :: String -> String
removeSpaces s = filter (/=' ') s

isDigit :: Char -> Bool
isDigit c 
            | c >= '0' && c <= '9' = True
            | c == '-'             = True
            | otherwise            = False

charToInteger :: Char -> Integer
charToInteger c
            | not (isDigit c) = error "character must be a digit"
            | c == '-' = (-1)
            | c == '0' = 0
            | c == '1' = 1
            | c == '2' = 2
            | c == '3' = 3
            | c == '4' = 4
            | c == '5' = 5
            | c == '6' = 6
            | c == '7' = 7
            | c == '8' = 8
            | c == '9' = 9

power :: Integer -> Poly Integer
power exp
            | exp == (-1) = Coef 1
            | exp == 0    = Coef 1
            | exp == 1    = X
            | exp  > 1    = Prod X (power (exp-1))




-- Reads a string like "1 + 2x + 5x^2" from a file and then returns a member of the type Poly Integer 
-- that the string represents as output.
getPoly :: FilePath -> IO (Poly Integer)
getPoly input = do 
                    file <- readFile input
                    return (polyParse file)


-- Takes a member of the type Poly Integer as input and returns a nicely formatted string presentation of it as output.
polyPrettyPrint :: (Show a, Num a, Eq a) => Poly a -> String
polyPrettyPrint (Coef a)   = show a
polyPrettyPrint X          = "x" 
polyPrettyPrint (Sum a b)  = (polyPrettyPrint a) ++ " + " ++ (polyPrettyPrint b)
polyPrettyPrint (Prod (Coef a) b) = (show a) ++ (polyPrettyPrint b)
polyPrettyPrint (Prod a (Coef b)) = (show b) ++ (polyPrettyPrint a)
polyPrettyPrint (Prod a b)        = "x^" ++ (show numX)
        where 
            numX = polyDegree (Prod a b)





-- takes a member p of the Poly Integer as input and returns a simplified member q of Poly Integer 
-- as output such that p and q represent the same polynomial function.
polySimp :: Poly Integer -> Poly Integer
polySimp (Coef a)   = (Coef a)
polySimp X          = X
polySimp (Sum a b)  = eval (Sum (polySimp a) (polySimp b))
polySimp (Prod a b) = eval (Prod (polySimp a) (polySimp b))

-- Helper function for polySimp - attempts to evaluate any constant expressions.
eval :: Poly Integer -> Poly Integer
eval (Coef a) = (Coef a)
eval X = X
eval (Prod (Coef a) (Coef b)) = Coef (a * b)
eval (Prod (Coef a) b)
                    | a == 0 = Coef 0
                    | a == 1 = b
                    | otherwise = (Prod (Coef a) b)
eval (Prod a (Coef b))
                    | b == 0 = Coef 0
                    | b == 1 = a
                    | otherwise = (Prod a (Coef b))
eval (Prod a b) = (Prod a b)
eval (Sum (Coef a) (Coef b)) = Coef (a + b)
eval (Sum (Coef a) b)
                    | a == 0 = b
                    | otherwise = (Sum (Coef a) b)
eval (Sum a (Coef b))
                    | b == 0 = a
                    | otherwise = (Sum a (Coef b))
eval (Sum a b) = (Sum a b)


-- Reads a string like "1 + 2x + 5x^2" from a file and returns a nicely formatted string that 
-- represents the derivative in simplified form of the polynomial represented by the input string.
getPolyAndDiff :: FilePath -> IO (String)
getPolyAndDiff input = do
                    string              <- readFile input
                    let poly            = polyParse string
                    let polyDeriv'      = polyDeriv poly
                    let polyDerivSimp   = polySimp polyDeriv'
                    let stringDerivSimp = polyPrettyPrint polyDerivSimp
                    return (stringDerivSimp)

{-   -- Test Cases  --  

Outputs are shown using the standard 'deriving Show' and not Poly Integer as an instance
of the type class Show.

Function: polyDeriv
Test Case Number: 1
Input: polyDeriv (Coef 10) 
Expected Output: Coef 0
Actual Output:   Coef 0

Function: polyDeriv
Test Case Number: 2
Input: polyDeriv X
Expected Output: Coef 1
Actual Output:   Coef 1

Function: polyDeriv
Test Case Number: 3
Input: polyDeriv (Sum (Prod (Coef 2) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 5) (Prod X (Prod X X))) (Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod X (Coef 9)) (Coef 7)))))
Expected Output: Sum (Prod (Coef 8) (Prod X (Prod X X))) (Sum (Prod (Coef 15) (Prod X X)) (Sum (Prod (Coef 6) X) (Sum (Prod (Coef 9) (Coef 1)) (Coef 0))))
Actual Output:   Sum (Prod (Coef 8) (Prod X (Prod X X))) (Sum (Prod (Coef 15) (Prod X X)) (Sum (Prod (Coef 6) X) (Sum (Prod (Coef 9) (Coef 1)) (Coef 0))))


Function: polyAsList
Test Case Number: 1
Input: polyAsList (Coef 12)
Expected Output: [12]
Actual Output:   [12]

Function: polyAsList
Test Case Number: 2
Input: polyAsList X
Expected Output: [0,1]
Actual Output:   [0,1]

Function: polyAsList
Test Case Number: 3
Input: polyAsList (Sum (Prod X (Prod X X)) (Prod (Coef 5.6) X))
Expected Output: [1.0,0.0,5.6,0.0]
Actual Output:   [1.0,0.0,5.6,0.0]

Function: polyParse
Test Case Number: 1
Input: polyParse "135"
Expected Output: Coef 135
Actual Output:   Coef 135

Function: polyParse
Test Case Number: 2
Input: polyParse "x"
Expected Output: X
Actual Output:   X


Function: polyParse
Test Case Number: 3
Input: polyParse "1 + -2x + 5x^2"
Expected Output: Sum (Coef 1) (Sum (Prod (Coef (-2)) X) (Prod (Coef 5) (Prod X X)))
Actual Output:   Sum (Coef 1) (Sum (Prod (Coef (-2)) X) (Prod (Coef 5) (Prod X X)))

Function: polyParse
Test Case Number: 4
Input: polyParse "72  +  4x^4  + 32x^11 + 3x + 43 + -13    + 12x^7"
Expected Output: Sum (Coef 72) (Sum (Prod (Coef 4) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 32) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))) (Sum (Prod (Coef 3) X) (Sum (Coef 43) (Sum (Coef (-13)) (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))))
Actual Output:   Sum (Coef 72) (Sum (Prod (Coef 4) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 32) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))) (Sum (Prod (Coef 3) X) (Sum (Coef 43) (Sum (Coef (-13)) (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))))

Function: getPoly
Test Case Number: 1
Input: getPoly "string.txt" (Contents of the file below)
123
Expected Output: Coef 123
Actual Output:   Coef 123

Function: getPoly
Test Case Number: 2
Input: getPoly "string.txt" (Contents of the file below)
x
Expected Output: X
Actual Output:   X

Function: getPoly
Test Case Number: 3
Input: getPoly "string.txt" (Contents of the file below)
1 + 2x + 5x^2
Expected Output: Sum (Coef 1) (Sum (Prod (Coef 2) X) (Prod (Coef 5) (Prod X X)))
Actual Output:   Sum (Coef 1) (Sum (Prod (Coef 2) X) (Prod (Coef 5) (Prod X X)))

Function: getPoly
Test Case Number: 4
Input: getPoly "string.txt" (Contents of the file below)
72  +  4x^4  + 32x^11 + 3x + 43 + 13    + 12x^7
Expected Output: Sum (Coef 72) (Sum (Prod (Coef 4) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 32) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))) (Sum (Prod (Coef 3) X) (Sum (Coef 43) (Sum (Coef 13) (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))))
Actual Output:   Sum (Coef 72) (Sum (Prod (Coef 4) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 32) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))) (Sum (Prod (Coef 3) X) (Sum (Coef 43) (Sum (Coef 13) (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))))

Function: polyPrettyPrint
Test Case Number: 1
Input: polyPrettyPrint (Coef 135)
Expected Output: "135"
Actual Output:   "135"

Function: polyPrettyPrint
Test Case Number: 2
Input: X
Expected Output: "x"
Actual Output:   "x"

Function: polyPrettyPrint
Test Case Number: 3
Input: polyPrettyPrint (Sum (Coef 1) (Sum (Prod (Coef 2) X) (Prod (Coef 5) (Prod X X))))
Expected Output: "1 + 2x + 5x^2"
Actual Output:   "1 + 2x + 5x^2"

Function: polyPrettyPrint
Test Case Number: 4
Input: polyPrettyPrint (Sum (Coef 72) (Sum (Prod (Coef 4) (Prod X (Prod X (Prod X X)))) (Sum (Prod (Coef 32) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X))))))))))) (Sum (Prod (Coef 3) X) (Sum (Coef 43) (Sum (Coef 13) (Prod (Coef 12) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X)))))))))))))
Expected Output: "72 + 4x^4 + 32x^11 + 3x + 43 + 13 + 12x^7"
Actual Output:   "72 + 4x^4 + 32x^11 + 3x + 43 + 13 + 12x^7"

Function: polySimp
Test Case Number: 1
Input: polySimp (Sum (Coef 0) (Coef 92))
Expected Output: (Coef 92)
Actual Output:   (Coef 92)

Function: polySimp
Test Case Number: 2
Input: polySimp (Prod (Coef 0) (Coef 92))
Expected Output: Coef 0
Actual Output:   Coef 0

Function: polySimp
Test Case Number: 3
Input: polySimp (Sum (Prod (Sum (Coef 3) (Coef (-3))) (Prod X X)) (Sum (Prod (Coef 0) (Prod X (Prod X X))) (Sum (Prod (Coef 92) (Coef 0)) (X))))
Expected Output: X
Actual Output:   X

Function: getPolyAndDiff
Test Case Number: 1
Input: getPolyAndDiff "string.txt" (Contents of the file below)
123
Expected Output: "0"
Actual Output:   "0"

Function: getPolyAndDiff
Test Case Number: 2
Input: getPolyAndDiff "string.txt" (Contents of the file below)
x
Expected Output: "1"
Actual Output:   "1"

Function: getPolyAndDiff
Test Case Number: 3
Input: getPolyAndDiff "string.txt" (Contents of the file below)
"1 + 2x + 5x^2"
Expected Output: "2 + 10x"
Actual Output:   "2 + 10x"

Test Case Number: 4
Input: getPolyAndDiff "string.txt" (Contents of the file below)
72  +  4x^4  + 32x^11 + 3x + 43 + 13    + 12x^7
Expected Output: "16x^3 + 352x^10 + 3 + 84x^6"
Actual Output:   "16x^3 + 352x^10 + 3 + 84x^6"

-}

-- If pl is a polynomial list, polyListToPoly pl is a polynomial of type Poly whose standard form
-- is represented by pl.
polyListToPoly :: [Integer] -> Poly Integer
polyListToPoly pl = polyListToPoly' pl 0

polyListToPoly' :: [Integer] -> Integer -> Poly Integer
polyListToPoly' [] _ = error "Cannot convert empty list to polynomial"
polyListToPoly' pl index
                    | length pl == 1 = Coef (head pl)
                    | length pl == 2 = Sum (prod (head pl) index) (prod (last pl) (index+1))
                    | otherwise      = Sum (prod (head pl) index) (polyListToPoly' (tail pl) (index+1))

-- Helper function to return the degree of x based on its position in the list
prod :: Integer -> Integer -> Poly Integer
prod num index
            | index == 0 = Coef num
            | index == 1 = Prod (Coef num) X
            | index  > 1 = Prod (Coef num) (prodX num index)
        where
            prodX num index 
                            | index == 2 = Prod X X
                            | index  > 2 = Prod X (prodX num (index-1))

a ===> b = (not a) || b

-- Property that polyParse of a string containing only one number n is equal to Coef n
polyParseProp :: Integer -> Bool
polyParseProp num = ((polyValue (polyParse (show num)) 1) == (polyValue (Coef num) 1))


-- Property that polyPrettyPrint of Coef n, where n is any number, is equivalent to the string representing n.
polyPrettyPrintProp :: Integer -> Bool 
polyPrettyPrintProp num = ((polyPrettyPrint (Coef num)) == (show num)) 

-- Property that the derivative of a constant is zero
polyDerivProp :: [Integer] -> Bool
polyDerivProp pl = (length pl == 1) ===> ((polyValue (polyDeriv (polyListToPoly pl)) 1) == 0)

-- Property that the simplified polynomial is equal to the original polynomial
polySimpProp :: [Integer] -> Bool
polySimpProp pl = (length pl > 0) ===> ((polyValue (polySimp (polyListToPoly pl)) 1) == (polyValue (polyListToPoly pl) 1))


{-   -- QuickCheck  -- 


a ===> b = (not a) || b

Function: polyParse
Property: polyParseProp num = ((polyValue (polyParse (show num)) 1) == (polyValue (Coef num) 1))
Actual Test Result: Pass

Function: polyPrettyPrint
Property: polyPrettyPrintProp num = ((polyPrettyPrint (Coef num)) == (show num)) 
Actual Test Result: Pass

Function: polyDeriv
Property: polyDerivProp pl = (length pl == 1) ===> ((polyValue (polyDeriv (polyListToPoly pl)) 1) == 0)
Actual Test Result: Pass

Function: polySimp
Property: polySimpProp pl = (length pl > 0) ===> ((polyValue (polySimp (polyListToPoly pl)) 1) == (polyValue (polyListToPoly pl) 1))
Actual Test Result: Pass


-}