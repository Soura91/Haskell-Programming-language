
{- ##################################
 FNU SOUMYA MUDIYAPPA
 Homework 5.
 ################################## -}
module Prog5 where
 
data Set = Set [Int] 
        | EmptySet
 deriving Show

-- Q1 function member that checks whether the given item is present in the given set.
member :: Int -> Set -> Bool
member a EmptySet = False
member a (Set []) = False
member a (Set xs) 
        | a `elem` xs = True
        | otherwise = False

-- Q2  a function ins that inserts the given item into a set. 
ins :: Int -> Set -> Set
ins a EmptySet = (Set [a])
ins a (Set []) = (Set [a])
ins a (Set xs)
      |(member a (Set xs) == True) = (Set xs)
      | otherwise =  Set ([a] ++ xs)

-- Q3  a function equal that returns whether two sets are equal.
equal :: Set -> Set -> Bool
equal (Set (x:xs)) (Set (y:ys)) 
          |length (xs) /= length (ys) = False
          |otherwise = equal' (Set (x:xs)) (Set (y:ys))

equal' :: Set -> Set -> Bool
equal' (Set [])(Set []) = True
equal' (Set []) _ = True
equal'  _ (Set []) = False
equal' (Set (x:xs)) (Set (y:ys))
      |(member x (Set(y:ys)) == True) = equal' (Set xs) (Set (y:ys))
      | otherwise = False

-- Q4 a function saferemove that removes the given item from a set. 
saferemove :: Int -> Set -> Maybe Set
saferemove x (Set xs)
        | member x (Set xs) == False = Nothing
        | member x (Set xs) == True && length (xs) == 1 = Just EmptySet
        | otherwise = Just(safe x (Set xs)) 

safe :: Int -> Set -> Set
safe x (Set ys) = Set([y | y <- ys, x/=y])

-- Q5 a function countLetters that inputs three Strings from the user on separate lines  and returns the number of letters in each entered String as a list.
countLetters :: IO [Int]
countLetters = do
                  putStrLn "Enter first string:"
                  s1 <- getLine
                  putStrLn "Enter second string:"
                  s2 <- getLine
                  putStrLn "Enter third string:"
                  s3 <- getLine
                  let result = [length s1, length s2, length s3]
                  return result         

-- Q6 a function and' that takes as an argument a Boolean value and performs a conjunction
and' :: Bool -> IO Bool
and' n  = do 
          putStrLn "Enter the string:"
          line1 <- getLine
          let line2 = ((read line1)::Bool)
          return (n && line2)

data Tree1 = Leaf1 Int
           | Node1 Tree1 Int Tree1
   deriving Show
   
t1 :: Tree1
t1 = Node1 (Leaf1 4) (3) (Node1 (Leaf1 (2)) (5) (Leaf1 1))

-- Q7 a function preorder that takes a tree argument and returns as a list an preorder traversal of the tree
preorder :: Tree1 -> [Int]
preorder (Leaf1 x) = [x]
preorder (Node1 left v right) = [v] ++ (preorder left) ++ (preorder right)

-- Q8  a function sumPositives that takes a tree argument and returns the sum of positive integers in the tree
sumPositives :: Tree1 -> Int
sumPositives (Leaf1 x) = if (x > 0) then x else 0
sumPositives (Node1 left v right)
        | (v > 0)  = v + sumPositives left + sumPositives right
        | otherwise = sumPositives left + sumPositives right
