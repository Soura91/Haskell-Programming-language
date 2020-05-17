
{- ##################################
 FNU SOUMYA MUDIYAPPA
 Homework 0.
 ################################## -}
module Prog0 where

-- 1) a function that returns whether a given integer number is between -10
-- and 10 (exclusive)
-- give input in braces for negative numbers
isSingleDigit :: Integer -> Bool
isSingleDigit x = (x < 10 ) && ( x > -10)
-- 2) a function that returns whether a given character is a vowel
isVowel :: Char -> Bool
isVowel x = (x == 'a') || (x == 'e')|| (x == 'i')|| (x == 'o')|| (x == 'u')||(x == 'A') || (x == 'E')|| (x == 'I')|| (x == 'O')|| (x == 'U')

--3)a function absIsDivisibleByThree that returns whether the absolute value of some
-- integer number is evenly divisible by three. 
absIsDivisibleByThree :: Integer -> Bool
absIsDivisibleByThree x = (mod (abs x) 3 == 0)

-- 4)function middle that returns the 2nd greatest of three given integer arguments
middle :: Integer -> Integer -> Integer -> Integer
middle x y z 
      | (x <= y) && (x <= z)  = min (max x y) z
      | (x >= y) && (x >= z)  = max (min x y) z
      | (x >= y) && (x <= z)  = min (max x y) z 
      | otherwise             = max (min x y) z

-- 5)a function nor that computes the NOR gate of two boolean arguments
nor :: Bool -> Bool -> Bool
nor x y
      | not(x || y) = True
      | otherwise   = False

-- 6) a function perimeter that computes the perimeter of a rectangle given its sides. If either
-- argument is less than or equal to zero, the function should return the value 0.
perimeter :: Integer -> Integer -> Integer
perimeter x y
      | (x > 0) && (y > 0) = 2*(x +y)
      | otherwise           = 0

--7) a function letterGrade that returns the equivalent letter grade for a given numerical
-- integer grade, per the syllabus for this course
letterGrade :: Integer -> String
letterGrade x
      | (x >= 93) = "A"
      | (x >= 90) = "A-"
      | (x >= 87) = "B+"
      | (x >= 83) = "B"
      | (x >= 80) = "B-"
      | (x >= 77) = "C+"
      | (x >= 73) = "C"
      | (x >= 70) = "C-"
      | otherwise = "F"
