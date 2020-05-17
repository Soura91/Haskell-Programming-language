  {- ##################################
 FNU SOUMYA MUDIYAPPA.
 Homework 7.
 ################################## -}
module Prog7 where

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | If BExpr Expr Expr

data BExpr = BoolLit Bool
           | Or BExpr BExpr
           | EqualTo Expr Expr
           | LessThan Expr Expr

-- Q1 a function bEval :: BExpr -> Bool that evaluates instances of the above boolean expression.
bEval :: BExpr -> Bool
bEval (BoolLit b) = b
bEval (Or b1 b2) = (bEval b1) || (bEval b2)
bEval (EqualTo e1 e2) = (value e1) == (value e2)
bEval (LessThan e1 e2) = (value e1) < (value e2)

--Q2 a function value that evaluates an expression.
value ::  Expr -> Int
value (Val n) = n
value (Add a b) = (value a ) + (value b)
value (Sub a b) = (value a ) - (value b)
value (Mul a b) = (value a ) * (value b)
value (Div a b) = div (value a ) (value b)
value (If a b c) = if ((bEval a) == True) then (value b) else (value c)

-- Q3 a function sumSqNeg that computes the "sum of squares of negatives". 
sumSqNeg :: [Int] -> Int
sumSqNeg ys = sum(map(^2)[y | y<-ys, y<0])

-- Q4 a function containing (without any higher order functions) that returns whether each element in the first list is also in the second list.
containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing [] _ = True
containing _ [] = False
containing (x:xs)(y:ys) = x `elem` (y:ys) && containing xs (y:ys)

--Q5 Write a function total that applies the function (first argument) to every element in the list (second argument) and sums the result.
total :: (Int -> Int) -> [Int] -> Int
total f xs = sum( map f xs)

double ::Int -> Int
double x = x * 2

--Q6  a function containing' (with higher order functions) that returns whether each element in the first list is also in the second list. 
containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = and (map (`elem` ys) xs)

-- Q7 a function lengths that returns a list of lengths of the given strings. You must use one or more higher-order functions: map, filter, foldr.
lengths :: [String] -> [Int]
lengths xs = map (length) xs

-- Q8 a function max' that returns the largest element of a nonempty list. You must use one or more higher-order functions: map, filter, foldr.
max' :: Ord a => [a] -> a
max' (x:xs) =  foldr (max) x xs

