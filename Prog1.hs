{- ##################################
 FNU SOUMYA MUDIYAPPA
 Homework 1.
 ################################## -}
module Prog1 where

--  function that returns whether a given int is greater than or equal to zero
isZeroOrGreater :: Int -> Bool
isZeroOrGreater x
      | (x >= 0)  = True
      | otherwise = False

-- a function that computes the volume of a sphere given its radius
sphereVolume :: Float -> Float
sphereVolume x = (4/3)*(3.14)*(x * x * x)

-- a function that calculates the ceiling of an float, but returns it as a float
ceilingDecimal :: Float -> Float
ceilingDecimal x = fromInteger(ceiling x)

-- function  to return the average of three integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z  = fromInteger((x + y + z))/ 3

-- function that returns True if no two of the four arguments are equal, and False otherwise.
fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent a b c d
  | (a /= b ) && (a /= c) && (a /= d) && (b /= c) && (b /= d) && (c /= d) = True
  | otherwise = False

-- function sum' that  uses recursion to compute the sum of all numbers from 1 to n, where n is greater than or equal to 1.
sum' :: Integer -> Integer
sum' x
  | x <= 1 = x 
  | x >  1 = x + sum'(x - 1)

-- function abssum that uses recursion to compute the sum of the absolute values from m to n, where m is less than or equal to n. 
abssum :: Integer -> Integer -> Integer
abssum m n
  | m == n = (abs n) 
  | m < n = (abs m) + abssum ( m + 1) (n)

-- function  exponent' that recursively computes the result of raising some base number, b, to some exponent, e
exponent' :: Integer -> Integer -> Integer
exponent' b p
  | p == 0 = 1
  | p >= 1 = b * exponent'(b)(p - 1)
