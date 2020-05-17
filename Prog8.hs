{- ##################################
 FNU SOUMYA MUDIYAPPA.
 Homework 8.
 ################################## -}

module Prog8 where

--Q1 a function all' that returns whether all elements of a list satisfy a predicate.
all' :: (a -> Bool) -> [a] -> Bool
all' p =  and . map p

--Q2 a function takeWhile' that selects elements from a list while they satisfy a predicate. 
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p = foldr (\x ys -> if p x then x:ys else []) []

--Q3  a function concat' that mimics the behavior of the built-in function concat .
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

--Q4 a function mapNums -- using recursion -- that takes a list of functions and applies them all to the same argument, producing a list of results. 
mapNums :: [(a -> b)] -> a -> [b]
mapNums [] y = []
mapNums (f:fs) y = f y : mapNums fs y

--Q5 a function multN that takes an integer and returns a function that takes an argument and returns its argument multiplied by n
multN' :: Int -> (Int -> Int)
multN' n = g
  where 
  g y = n * y

--Q6 a function nonDigit that returns whether a character is not a member of the list [0..9] .
nonDigit :: Char -> Bool
nonDigit = (\x -> not(x `elem` ['0'..'9']))

--Q7 a function  that reverses a list of strings, and then returns the number of letters in each string as a list. 
doubleOp :: [String] -> [Int]
doubleOp  =  reverse . map length

--Q8 a function mapNums' that takes a list of functions and applies them all to the same argument, producing a list of results. 
mapNums' :: [(a -> b)] -> a -> [b]
mapNums' fs xs = map(\f -> f xs) fs

