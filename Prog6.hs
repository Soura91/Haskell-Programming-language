 {- ##################################
 FNU SOUMYA MUDIYAPPA.
 Homework 6.
 ################################## -}
module Prog6 where

data Tree a = Leaf a
    | Node [Tree a]

-- Q1 a function occurs that returns whether a given argument is present in a given tree as a leaf.
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Node []) = False
occurs x (Node [Leaf v]) = x == v
occurs x (Leaf v) = x == v
occurs x (Node (y:ys)) = occurs x y || occurs x (Node ys)

--Q2  a function countLeaves that takes a tree argument and returns the number of leaves in the tree.
countLeaves :: Tree a -> Int
countLeaves (Node []) = 0
countLeaves (Node [(Leaf v)]) = 1
countLeaves (Leaf v) = 1
countLeaves (Node (x:xs)) = (countLeaves x) + (countLeaves (Node xs))

--Q3 a function isBinary that takes a tree argument and returns if the tree is binary: every interior node has exactly two children. 
isBinary :: Tree a -> Bool
isBinary (Leaf x)  = True
isBinary (Node n)
  |(length(n) ==2) = a && binary
  |otherwise = False
  where binary = and[isBinary y | y<-n]
        a = (length(n) == 2)

--Q4 a function pre that returns a preorder traversal of the nodes in the tree.
pre :: Tree a -> [a]
pre (Node[]) = []
pre (Leaf v) = [v]
pre (Node(x:ys)) = (pre x) ++  (pre (Node ys))

--Q5 a function depthK that returns all nodes that are at depth k in the tree.
depthK :: Int -> Tree a -> [a]
depthK n (Node[]) = []
depthK 0 (Leaf v) = [v]
depthK n (Leaf v) = []
depthK n (Node(y:ys)) 
    | n > 0 = (depthK (n-1) y) ++ (depthK n (Node ys))
    | otherwise = []


data Expr = Val Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
-- Q6 a function value1 that evaluates an expression
value1 :: Expr -> Int
value1 (Val n) = n
value1 (Add a b) = (value1 a) + (value1 b)
value1 (Sub a b) = (value1 a) - (value1 b)
value1 (Mul a b) = (value1 a) * (value1 b)
value1 (Div a b) = div (value1 a) (value1 b)

-- Q7 a function value2 that evaluates an expression, but returns Nothing if there is a division by zero scenario
value2 :: Expr -> Maybe Int
value2 (Val n) = Just n
value2 (Add a b) = do x <- value2 a
                      y <- value2 b
                      Just(x+y)
value2 (Sub a b) = do x <- value2 a
                      y <- value2 b
                      Just(x-y)
value2 (Mul a b) = do x <- value2 a
                      y <- value2 b
                      Just(x*y)
value2 (Div a b) = do x <- value2 a
                      y <- value2 b
                      safe_div x y

safe_div      :: Int -> Int -> Maybe Int
safe_div _ 0 =  Nothing
safe_div n m =  Just (n `div` m)                    

-- Q8 Make the Expr type an instance of the Show class. Appropriate define the function show
instance Show Expr where 
    show = showExpr

showExpr :: Expr -> String
showExpr (Val n) = show n
showExpr (Add e1 e2) = "(" ++ (showExpr e1) ++ "+" ++ (showExpr e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ (showExpr e1) ++ "-" ++ (showExpr e2) ++ ")"
showExpr (Mul e1 e2) = "(" ++ (showExpr e1) ++ "*" ++ (showExpr e2) ++ ")"
showExpr (Div e1 e2) = "(" ++ (showExpr e1) ++ "/" ++ (showExpr e2) ++ ")"



