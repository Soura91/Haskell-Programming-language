{- ##################################
 FNU SOUMYA MUDIYAPPA
 Homework 3.
 ################################## -}
module Prog3 where

import Data.Char
-- Q1 function  which  returns as a pair, the split of a string with an even number of characters into its two halves.
halve :: String -> (String, String)
halve x = splitAt (div (length x) 2 ) x

-- Q2 function init' that has the same behavior as init 
init' :: [a] -> [a]     
init' [] = error "This is a empty list"
init' [y] = [ ]
init' (y:ys) = y : init' ys

-- Q3 function numTimes that returns the number of times that an element occurs in the list
numTimes :: Int -> [Int] -> Int
numTimes x [] = 0
numTimes x xs | (head xs) == x = 1 + numTimes x (tail xs)
              | (head xs) /= x = numTimes x (tail xs)

-- Q4 a function lowerOddLetters that lowercases the first, third, fifth letter of a string
lowerOddLetters :: String -> String
lowerOddLetters [] = []
lowerOddLetters (x : []) = [toLower x]
lowerOddLetters (x : xs : ys) = toLower x : xs : lowerOddLetters ys

-- Q5 a function nestedParens that takes a string argument and returns true if it is a nesting of zero or more pairs of parentheses
nestedParens :: String -> Bool
nestedParens [] = True
nestedParens (x:xs)
    |((length (x:xs) `mod` 2) /= 0) = False
    |(x == '(') && (last xs == ')') = nestedParens (init xs)
    |otherwise = False

-- Q6 a function triads that generates a list of integer triples
triads :: Int -> [(Int,Int,Int)]
triads n = [(x,y,z)| x <-[1..n], y <-[1..n], z <- [1..n], ((x*x)+ (y*y) == (z*z))]

-- Q7 a function replicate' which, only using library functions, has the same behavior as replicate
replicate' :: Int -> Char -> String
replicate' x xs  = [ y | y <- [xs..xs], i<-[1..x], i <= x]

-- Q8 a recursive function replicate'' that has the same behavior as replicate and replicate' 
replicate'' :: Int -> Char -> String
replicate'' 0 xs = []
replicate'' n xs = xs : replicate'' (n-1) xs 
