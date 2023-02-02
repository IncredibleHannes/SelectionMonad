import Control.Monad.State
import Debug.Trace
import qualified Data.List      as L

import Data.Matrix


type J r x = (x -> r) -> x

hsequence :: [x] -> [[x] -> (x -> r) -> x] -> ([x] -> r) -> [x]
hsequence h [] p       = []
hsequence h (e : es) p = a : as
    where 
        a  = e h (\x -> p (x : hsequence (h ++ [x]) es (p . (x:)))) 
        as = hsequence (h++[a]) es (p . (a:))


type ABState = (Int,Int)

type JS x = (x -> State ABState Int) -> State ABState x

sabsequence :: [x] -> [[x] -> JS x] -> JS [x]
sabsequence h [] p       = return []
sabsequence h (e : es) p = do {
        s <- get;
        a <- e h p';
        put (minBound, maxBound);
        as <- sabsequence (h ++ [a]) es (p . (a:));
        return (a : as)
    }
    where 
        p' x = do {
            xs <- sabsequence (h ++ [x]) es (p . (x:));
            p (x : xs)
        }

argMin xs = abMax (minBound,head (getMoves xs)) (getMoves xs)

abMax :: (Int, x) -> [x] -> JS x
abMax (c, v) [] p = return v;
abMax (c, v) (x:xs) p = do {
    (alpha, beta) <- get;
    cost <- p x;
    put (alpha, min cost beta);
    if cost <= alpha then return v
    else
        if cost > c then abMax (cost, x) xs p else abMax (c,v) xs p
}

argMax xs = abMin (maxBound,head (getMoves xs)) (getMoves xs)

abMin :: (Int, x) -> [x] -> JS x
abMin (c, v) [] p = return v;
abMin (c, v) (x:xs) p = do {
    (alpha, beta) <- get;
    cost <- p x;
    put (max cost alpha, beta);
    if cost >= beta then return v
    else
        if cost < c then abMin (cost, x) xs p else abMin (c,v) xs p 
}


data Tree = Node Int Int [Tree]
    deriving (Show, Eq)

example :: Tree 
example = Node 1 6 [Node 2 3 [Node 5 5 [Node 11 5 [Node 20 5 [], Node 21 6 []], Node 12 4 [Node 22 7 [], Node 23 4 [], Node 24 5 []]], Node 6 3 [Node 13 3 [Node 25 3 []]]], Node 3 6 [Node 7 6 [Node 14 6[Node 26 6 []], Node 15 6 [Node 27 6 [], Node 28 9 []]], Node 8 7 [Node 16 7 [Node 29 7 []]]], Node 4 5 [Node 9 5 [Node 17 5 [Node 30 5 []]], Node 10 8 [Node 18 8 [Node 31 9 [], Node 32 8 []], Node 19 6 [Node 33 6 []]]]] 


getMoves :: [Tree] -> [Tree]
getMoves [] = [example]
getMoves [Node _ _ xs] = xs
getMoves (x:xs) = getMoves xs


treeCost :: [Tree] -> State ABState Int
treeCost [] = return 0
treeCost [Node i c _] = return c
treeCost ((Node i c _):xs) = treeCost xs

treeCost' :: [Tree] -> Int
treeCost' [] =  0
treeCost' [Node i c _] = c
treeCost' ((Node i c _):xs) = treeCost' xs

solution = runState (sabsequence [] [argMax, argMin, argMax, argMin, argMax] treeCost) (minBound, maxBound)
fsolution = formatSolution $ fst solution


minWith :: Ord b => (a -> b) -> [a] -> a
minWith f = foldr1 (smaller f)
  where smaller f x y = if f x <= f y then x else y

maxWith :: Ord b => (a -> b) -> [a] -> a
maxWith f = foldr1 (bigger f)
  where bigger f x y = if f x >= f y then x else y

minW xs f = maxWith f $ getMoves xs
maxW xs f = minWith f $ getMoves xs

solution' = hsequence [] [maxW, minW, maxW, minW, maxW] treeCost'
fsolution' = formatSolution solution'

formatSolution :: [Tree] -> [Int]
formatSolution [] = []
formatSolution ((Node i _ _):xs) = i : formatSolution xs

-------------------------------------------------------------------------------------------------------------------------


gameName = "ConnectThree"

type R = Int
type Move = Int
type Board = Matrix Player
data Player = X | O | N
  deriving (Eq, Show)


wins :: Board -> Player -> Bool
wins = checkWin 1 1
  where
    checkWin :: Int -> Int -> Board -> Player -> Bool
    checkWin 4 3 _ _ = False
    checkWin 5 y b p = ((b ! (5, y) == p) && (row 5 y b p || colum 5 y b p || left 5 y b p || right 5 y b p)) || checkWin 1 (y + 1) b p
    checkWin x y b p = ((b ! (x, y) == p) && (row x y b p || colum x y b p || left x y b p || right x y b p)) || checkWin (x + 1) y b p
    row x y b p      = x < 4 && (b ! (x, y) == p && b ! (x+1, y) == p && b ! (x+2, y) == p)
    colum x y b p    = y == 1 && (b ! (x, y) == p && b ! (x, y+1) == p && b ! (x, y+2) == p)
    left x y b p     = y == 1 && x < 4 && (b ! (x, y) == p && b ! (x+1, y+1) == p && b ! (x+2, y+2) == p)
    right x y b p    = y == 1 && x > 2 && (b ! (x, y) == p && b ! (x-1, y+1) == p && b ! (x-2, y+2) == p)



value :: Board -> Int
value b  | wins b X  = 1
         | wins b O  = -1
         | otherwise = 0

outcome :: Player -> [Move] -> Board -> Board
outcome _ [] b   = b
outcome p (m : ms) b = let nb = insert m p b in
                       if wins nb p then nb else outcome (changePlayer p) ms nb

changePlayer :: Player -> Player
changePlayer X = O
changePlayer O = X

insert :: Move -> Player -> Board -> Board
insert m = insert' (m, 1)
  where insert' (x, y) p b = if b ! (x, y) == N
                             then setElem p (x, y) b
                             else insert' (x, y+1) p b


getPossibleMoves :: [Move] -> [Move]
getPossibleMoves [] = [1..5]
getPossibleMoves xs = filter (\x -> length (L.elemIndices x xs) < 3) [1..5]

p :: [Move] -> R
p ms = value(outcome X ms (matrix 5 3 (const N)))

epsilons :: [[Move] -> (Move -> State ABState R) -> State ABState Move]
epsilons = take 9 all
  where all = epsilonX : epsilonO : all
        epsilonX history = abMin(maxBound,head (getPossibleMoves history)) (getPossibleMoves history)
        epsilonO history = abMax (minBound,head (getPossibleMoves history)) (getPossibleMoves history)

epsilons' :: [[Move] -> J R Move]
epsilons' = take 9 all
  where all = epsilonX : epsilonO : all
        epsilonX history f = maxWith f (getPossibleMoves history)
        epsilonO history f = maxWith f (getPossibleMoves history)

optimalPlay = hsequence [] epsilons' p 

p' x = return $ p x

--optimalPlay' = runState (sabsequence [] epsilons p') (minBound, maxBound)
