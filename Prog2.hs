{- ##################################
 FNU SOUMYA MUDIYAPPA.
 Homework 2.
 ################################## -}
module Prog2 where

import Data.Char

-- function that uses recursion to compute the sum of the ASCII values in a String. 
asciisum :: String -> Integer
asciisum [] = 0
asciisum (x : xs) = toInteger(fromEnum x) +  asciisum xs

-- function integerSqrt that returns the integer square root of a positive integer n
integerSqrt :: Integer -> Integer
integerSqrt x = floor (sqrt (fromIntegral x))

-- function matches that picks out all instances of an integer n from a list
matches :: Integer -> [Integer] -> [Integer]
matches x [] = []
matches x xs | (head xs) == x = x:(matches x (tail xs))
              | (head xs) /= x = matches x (tail xs)

-- function element that returns True if an element is a member of a list, False otherwise. 
element :: Integer -> [Integer] -> Bool
element x xs
    | ((matches x xs) /= []) = True
    | otherwise              = False

--  function elemAt that returns the item of the list, where the first item is index 0. 
elemAt :: Int -> [Int] -> Int
elemAt 0 (x:xs)  = x
elemAt n (x:xs)  = elemAt (n - 1) xs

-- a function and' that uses recursion to return the conjunction of a list of boolean values.
and' :: [Bool] -> Bool
and' [] = True
and' (x : xs) =  x && (and' xs) 

-- a function iSort' that uses insertion sort to sort a list of pairs 
iSort' :: [(Int,String)] -> [(Int,String)]
ins :: (Int,String) -> [(Int,String)] -> [(Int,String)]
ins x [] =[x]
ins x (y : ys)
  | fst x > fst y = y : ins x ys
  | fst x <= fst y = x : y : ys
iSort' [] = []
iSort' (x:xs) = ins x (iSort' xs)

--  a function upperFirstTwoLetters that uppercases the first two letters in a string but and leaves everything else as is

upperFirstTwoLetters :: String -> String
upperFirstTwoLetters (x:y:z) = charToString(toUpper x) ++ charToString(toUpper y) ++ z

charToString :: Char -> String
charToString c = [c]
