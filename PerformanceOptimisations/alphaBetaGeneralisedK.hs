{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ((>>=), return, pure, (<*>), fmap, sequence, Left, Right)
import Debug.Trace
import GHC.Base (maxInt)

type GK r a = forall b. (a -> (r,b)) -> (r,b)

-- Example with search tree

-- Standard tree definition with labels
type Node a = a
data Tree a = Branch a [Tree a] | Leaf a deriving Show


-- Given a path from the root node, it returns a list of child nodes
getNextNodes :: Eq a => Tree a -> [a] -> [a]
getNextNodes (Leaf x) []            = [x]
getNextNodes (Leaf x) _             = []
getNextNodes (Branch a cs) []       = [a]
getNextNodes (Branch a cs) [x]      = if x == a then getValues cs else []
getNextNodes (Branch a cs) (x:y:xs) = if x /= a then [] else getNextNodes (findNode y cs) (y:xs)

-- Given a node lable and a list of trees, it returns the tree where the root lable matches
findNode :: Eq a => a -> [Tree a] -> Tree a
findNode a ((Leaf x):xs)      | a == x    = Leaf x
                              | otherwise = findNode a xs
findNode a ((Branch x cs):xs) | a == x    = Branch x cs
                              | otherwise = findNode a xs


-- returns all the lables of the root node of a given list of trees
getValues :: [Tree a] -> [a]
getValues []                = []
getValues ((Branch x _):xs) = x : getValues xs
getValues ((Leaf x ):xs)    = x : getValues xs


-- small example on how to construct trees
-- https://upload.wikimedia.org/wikipedia/commons/9/91/AB_pruning.svg
x :: Tree Int
x = Branch 1 [
        Branch 2 [
          Branch 3 [
            Branch 4 [Leaf 5, Leaf 6], 
            Branch 5 [Leaf 7, Leaf 4, Leaf 5]], 
          Branch 6 [
            Branch 7 [Leaf 3]]], 
        Branch 8 [
          Branch 9 [
            Branch 10 [Leaf 6],
            Branch 11 [Leaf 6, Leaf 9]],
          Branch 12 [
            Branch 13 [Leaf 7]]],
        Branch 14 [
          Branch 15 [
            Branch 16 [Leaf 5]],
          Branch 17 [
            Branch 18 [Leaf 9, Leaf 8],
            Branch 19 [Leaf 6]
          ]
        ]]


-- example ilustrating how to utilise selection monad without alpha beta prunig to 
-- calculate the optimal path

-- a path is judget by the value of the last node in the path (i.e the leaf value)
p ::  [Int] -> (Int, [Int])
p [x]    = (x, [x])
p (x:xs) = let (r,v) = p xs in (r,x:v)

sequenceHGK :: [a] -> [[a] -> GK r a] -> GK r [a]
sequenceHGK h [] p     = p []
sequenceHGK h (e:es) p = e h (\x -> sequenceHGK (h ++ [x]) es (\xs -> p (x:xs)))

minWith :: Ord r => [x] -> GK r x
minWith xs f = foldr1 (\(r,x) (s,y) -> if r <= s then (r,x) else (s,y) ) (map f xs)

maxWith :: Ord r => [x] -> GK r x
maxWith xs f = foldr1 (\(r,x) (s,y) -> if r >= s then (r,x) else (s,y) ) (map f xs)

-- application of minimax algorithm to the example tree
exampleComp :: (Int, [Int])
exampleComp = sequenceHGK [] (take 5 all) p
  where
    all :: Ord r => [[Int] -> GK r Int]
    all = (minWith . getNextNodes x) : (maxWith . getNextNodes x) : all


-- Alpha Beta pruning version

p' :: [(Int, Int, Int)] -> (Int, [Int])
p' [(a, b ,x)]    = (x,[x])
p' ((a, b, x):xs) = let (r,v) = p' xs in (r,x:v) 


-- given an alpha and beta values, the current maximum, the remainding list of options and 
-- a property function, we call the property function with our alpha and beta values to 
-- obtain the R value for the current element, then store the bigger element in value,
-- and then check if we exeed the beta value and either stop the search or continue with
-- the updated alpha value
alphaMaxWith' :: Ord r => (r, r) -> (r,y) -> [(r, r, x)] -> ((r, r, x) -> (r,y)) -> (r,y)
alphaMaxWith' (a,b) v [] p               = v
alphaMaxWith' (a,b) (r,y) ((_,_,x):xs) p = let (r',y') = p (a, b, x) in
                                           let value = if r >= r' then (r,y) else (r',y') in 
                                           if r >= b || r' >= b then value else alphaMaxWith' (max r' a, b) value xs p

-- In the first call we set the alpha and beta values to the ones first in our list, do the
-- check if we exeeed the beta value and if not continue our computation with the 
-- alphaMaxWith' function
alphaMaxWith :: Ord r => [(r,r,x)] -> ((r,r,x) -> (r,y)) -> (r,y)
alphaMaxWith ((a,b,x):xs) p = let (r,y) = p(a,b,x) in if r >= b then (r,y) else alphaMaxWith' (max a r,b) (r,y) xs p                                     

-- Same logic as with alphaMaxWith
betaMinWith :: Ord r => [(r,r,x)] -> ((r,r,x) -> (r,y)) -> (r,y)
betaMinWith ((a,b,x):xs) p = let (r,y) = p(a,b,x) in if r <= a then (r,y) else betaMinWith' (a,min b r) (r,y) xs p

betaMinWith' :: Ord r => (r, r) -> (r,y) -> [(r, r, x)] -> ((r, r, x) -> (r,y)) -> (r,y)
betaMinWith' (a,b) v [] p               = v
betaMinWith' (a,b) (r,y) ((_,_,x):xs) p = let (r',y') = p (a,b,x) in
                                          let value = if r <= r' then (r,y) else (r',y') in 
                                          if r <= a || r' <= a then value else betaMinWith' (a, min r' b) value xs p


-- An example on how to utilise the new alpha beta min/max with functions                                  
exampleComp1 :: (Int, [Int])
exampleComp1 = sequenceHGK [] (take 5 all) p'
  where 
        es :: [(Int,Int,Int)] -> GK Int (Int, Int, Int)
        es h = alphaMaxWith (f h)
        es' :: [(Int,Int,Int)] -> GK Int (Int, Int, Int) 
        es' h = betaMinWith (f h)
        all :: [[(Int,Int,Int)] -> GK Int (Int, Int, Int)]
        all = es' : es : all

-- Given a history, f calculates all current options
f :: Bounded r => [(r, r, Int)] -> [(r, r, Int)]
f [] = map (minBound,maxBound,) (getNextNodes x [])
f h  = map (((\(a,_,_) -> a) . last) h,((\(_,b,_) -> b) . last) h,) (getNextNodes x (map (\(_,_,x) -> x) h)) 


-- show similar agument to richard that the program is correct.