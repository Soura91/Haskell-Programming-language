{- ##################################
 FNU SOUMYA MUDIYAPPA
 Homework 4.
 ################################## -}
module Prog4 where

import Data.Char

type Item = String
type Price = Int
type BarCode = Int
type ShoppingCart = [BarCode]
type Database = [(BarCode, Item, Price)]

db1 :: Database
db1 = [ (1234, "Hot Dog", 2),
 (4719, "Fries", 1),
 (3814, "Soda", 1),
 (1112, "Chips", 1),
 (1113, "Apple", 1)]

cart1 :: ShoppingCart
cart1 = [1234, 4719, 3814, 1112, 1113, 1234]

-- Q1  a function that checks whether a barcode is valid, that is, is present in the store's database
isValidBarCode :: BarCode -> Database -> Bool
isValidBarCode b [] = False
isValidBarCode b ((b',i,p):xs)
            | (b == b')  =  True
            | otherwise  =  isValidBarCode b xs

-- Q2  a function that checks whether a database is valid, that is, each bar code in the database is unique
isValidDatabase :: Database -> Bool
isValidDatabase [] = True
isValidDatabase ((b,i,p):xs)
      |(count' b ((b,i,p):xs) == 1) && (isValidDatabase xs) = True
      |(count' b ((b,i,p):xs) > 1) = False
      |otherwise = isValidDatabase xs

count' :: BarCode -> Database -> Int
count' _ [] = 0
count' x ((b',i,p) : xs) 
        | (x == b') = 1 + (count' x xs)
        | otherwise = count' x xs

-- Q3  a function totalBill that takes a shopping cart and totals the cost of the items in it
totalBill :: ShoppingCart -> Database -> Int
totalBill [] [] = 0
totalBill [] _ = 0
totalBill _ [] = 0
totalBill (y:ys) ((b',i,p):xs)
        |(isValidBarCode y ((b',i,p):xs)) = count y (y:ys) * p + totalBill ys xs
        |otherwise = totalBill ys xs

-- Q4 a function that returns the set of all such items that are in the shopping cart more than once.   
duplicateItems :: ShoppingCart -> Database -> [String]
duplicateItems [] [] = []
duplicateItems _ [] = []
duplicateItems [] _ = []
duplicateItems (y:ys) ((b,s,p):xs)
    | (isValidBarCode y ((b,s,p):xs)) && (count y (y:ys) > 1) = s : duplicateItems ys xs
    | otherwise = duplicateItems ys xs

count :: BarCode -> ShoppingCart -> Int
count _ [] = 0
count x (y : ys) 
      | (x == y) = 1 + (count x ys)
      | otherwise = count x ys
 
data LicensePlate = IntPlate Int Int
                    | CharPlate [Char] Int
 
 deriving Show

lp1 :: LicensePlate
lp1 = IntPlate 123 4567

lp2 :: LicensePlate
lp2 = CharPlate "abc" 4567

-- Q5 a function  that takes a license plate and returns whether it is valid or notvalid 
isValidPlate :: LicensePlate -> Bool
isValidPlate (IntPlate x y) = if (length (show x ) == 3) && (length (show y) == 4) then True else False
isValidPlate (CharPlate a b) = if (length a == 3) && (length (show b)  == 4) then True else False
      
-- Q6 a function  that takes a license plate and returns what would be the next license plate in the iteration, that is, add one to it.
nextPlate :: LicensePlate -> LicensePlate
nextPlate (IntPlate x y) = IntPlate x (y+1)
nextPlate (CharPlate a b) = CharPlate a (b+1)

-- Q7 a function  that takes a license plate and sums the first part of the plate.
sumFirstPartPlate :: LicensePlate -> Int
sumFirstPartPlate (IntPlate x y) = sum [ digitToInt a | a <- show x]
sumFirstPartPlate (CharPlate a b) = sum [fromEnum (z) | z <- a]

-- Q8 a function that takes a license plate and returns its string equivalent. 
showPlate :: LicensePlate -> String
showPlate (IntPlate x y) = show x ++ "-" ++ show y
showPlate (CharPlate a b) = a ++ "-" ++ show b
