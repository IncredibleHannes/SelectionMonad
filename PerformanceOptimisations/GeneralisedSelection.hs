{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Debug.Trace ()
import Control.Arrow ()
import Data.Function (on)
import Data.List
import Prelude hiding ((>>=), return, pure, (<*>), fmap, sequence)

type K r x = forall y. (x -> (r,y)) -> (r,y)

-- min and max with function that work well with the new K
minWith :: Ord r => forall y . [x] -> (x -> (r,y)) -> (r,y)
minWith xs f = foldr1 (\(r,x) (s,y) -> if r < s then (r,x) else (s,y) ) (map f xs)

maxWith :: Ord r => forall y . [x] -> (x -> (r,y)) -> (r,y)
maxWith xs f = foldr1 (\(r,x) (s,y) -> if r > s then (r,x) else (s,y) ) (map f xs)

minWithJ :: Ord b => [a] -> (a -> b) -> a
minWithJ xs f = snd (minimumBy (compare `on` fst) (map (\x -> (f x , x)) xs))

maxWithJ :: Ord b => [a] -> (a -> b) -> a
maxWithJ xs f = snd (maximumBy (compare `on` fst) (map (\x -> (f x , x)) xs))

-- pair and sequence for the new K

pairK :: K r x -> K r y -> K r (x,y)
pairK f g p = f (\x -> g(\y -> p(x,y)))

sequenceK :: [K r x] -> K r [x]
sequenceK [e] p = e (\x -> p [x])
sequenceK (e:es) p = e (\x -> sequenceK es (\xs -> p (x:xs)))

hsequenceK :: [[x] -> K r x] -> [x] -> K r [x]
hsequenceK [e] h p = e h (\x -> p [x])
hsequenceK (e:es) h p = e h (\x -> hsequenceK es (h ++ [x]) (\xs -> p (x:xs)))

-- tests for the new K
n = 70

p :: [Int] -> (Int, [Int])
p x = (sum x, x)

es :: [K Int Int]
es = [e1,e2,e1,e2]
  where 
    e1 = minWith [1..n]
    e2 = maxWith [1..n] 

es' :: [KK Int Int]
es' = [e1,e2,e1,e2]
  where 
    e1 = snd . minWith [1..n]
    e2 = snd . maxWith [1..n]

es'' :: [J Int Int]
es'' = [e1,e2,e1,e2]
  where 
    e1 = minWithJ [1..n]
    e2 = maxWithJ [1..n] 

es''' :: [K Int Int]
es''' = [e1,e2,e1,e2]
  where 
    e1 = kk2k (j2kk (minWithJ [1..n]))
    e2 = kk2k (j2kk (maxWithJ [1..n]))

sequenceJ :: [J r x] -> J r [x]
sequenceJ [] p     = []
sequenceJ (e:es) p = b : bs
    where
        b = e (\a -> p (a : sequenceJ es (p . (a:))))
        bs = sequenceJ es (p . (b:))

type J r x = (x -> r) -> x

kk2j :: KK r x -> J r x
kk2j f p = f (\x -> (p x, x))

j2kk :: J r x -> KK r x
j2kk f p = snd (p (f (fst . p)))

test1 = sequenceK es p                    -- 6.83    
test2 = sequenceKK es' p                  -- 13.90
test3 = kk2k (sequenceKK es') p           -- 13.17
test4 = sequenceJ es'' sum                -- 7.13
test5 = j2kk (sequenceJ es'') p          -- 9.64
test6 = kk2j (sequenceKK es') sum        -- 11.31
test7 = k2kk (sequenceK es) p            -- 7.43           
test8 = kk2j (k2kk (sequenceK es)) sum   -- 5.04
test9 = sequenceK es''' p                    -- 8.74
test10 = (kk2j $ k2kk $ sequenceK es''') sum -- 7.38

{-
j2k :: J r x -> K r x
j2k x = k
  where
    kk :: KK r x = j2kk x
    k :: K r x = kk2k kk

--k2j :: K r x -> J r x
--k2j x = kk2j . k2kk
-}
-- Isomorphism between the new K and Toms Special K

type KK r x = forall y. (x -> (r,y)) -> y

k2kk :: forall r x y z. ((x -> (r,y)) -> (r,y)) -> ((x -> (r,y)) -> y)
k2kk f = snd . f

kk2k :: KK r x -> K r x
kk2k f p =  f (\x -> let (r,y) = p x in (r, (r,y)))

pairKK :: KK r a -> KK r b -> KK r (a, b)
pairKK f g p = f (\x -> g (\y -> let (r, z) = p (x,y) in (r, (r, z))))

sequenceKK :: [KK r a] -> KK r [a]
sequenceKK [] p     = snd $ p []
sequenceKK (e:es) p = e (\x -> sequenceKK es (\xs -> let (r,z) = p (x:xs) in (r,(r,z))))

{--  Proof of the isomorphism

assuming that for 
g :: K r x
forall p :: forall y . (x -> (r,y))
exists x :: x
such that:
g p = p x

Making sure that g is not changing the r value after applying p to its elements

kk2k(k2kk g)
 = kk2k(snd . g)
 = \p -> (snd . g) (\x -> let (r,y) = p x in (r, (r,y)))
 = \p -> snd (g(\x -> let (r,y) = p x in (r, (r,y))))
 = \p -> snd (exist x -> let (r,y) = p x in (r, (r,y))) -- assumption
 = \p -> exists x -> snd $ let (r,y) = p x in (r, (r,y)) -- exists commute
 = \p -> exists x -> let (r,y) = p x in snd (r, (r,y))  -- assumption
 = \p -> g (\x -> let (r,y) = p x in snd (r, (r,y)))
 = \p -> g p
 = g

k2kk(kk2k f)
 = k2kk(\p -> f (\x -> let (r,y) = p x in (r, (r,y))))
 = snd . (\p -> f (\x -> let (r,y) = p x in (r, (r,y))))
 = (\p -> (snd . f) (\x -> let (r,y) = p x in (r, (r,y)))) -- * 
 = (\p -> f (\x -> let (r,y) = p x in (r, snd (r,y))))
 = (\p -> f p)
 = f



* Free Theorem of KK 
g :: forall y. (\x -> (r,y)) -> y 
Given:
f :: a -> b
p :: x -> (r, a)
(f . g) p = g ((id *** f) . p)

** Theorem of K
g :: forall y. (\x -> (r,y)) -> (r,y)
Given:
f :: a -> b
p :: x -> (r, a)
((id *** f) . g) p = g ((id *** f) . p)

** Theorem I need

g :: forall y. (\x -> (r,y)) -> (r,y)
  (snd . g) (\x -> let (r,y) = p x in (r, (r,y)))
= g (\x -> let (r,y) = p x in snd (r, (r,y))) 

But this is not true:
g :: K Int Int
g p = (r+1, x)
    where (r, x) = p 1

g' = kk2k(k2kk g)

p _ = (1,1) 

--}


-- Making K a monad

(>>=) :: K r x -> (x -> K r y) -> K r y
e >>= f = e . flip f 

return :: x -> K r x
return x p = p x 

(<*>) :: K r (x -> y) -> K r x -> K r y
f <*> g = \p -> f (\e -> g (p . e))

pure :: x -> K r x
pure = flip ($)

fmap :: (x -> y) -> K r x -> K r y
fmap f e p = e (p . f)


sequence :: [K r x] -> K r [x]
sequence []     = return []
sequence (e:es) = e >>= (
                  \x -> sequence es >>= 
                  \xs -> return (x:xs))

{--
-- Monad Laws

-- Left identity: 	
return a >>= h
 = (flip ($)) a >>= h
 = (\p -> p a) >>= h
 = \p' -> (\p -> p a) ((flip h) p')
 = \p' -> ((flip h) p') a
 = \p' -> h a p'
 = h a

-- Right identity: 	
m >>= return 
 = \p -> m ((flip return) p)
 = \p -> m ((flip (flip ($))) p)
 = \p -> m (($) p)
 = \p -> m p
 = m 

-- Associativity: 	

e >>= f = \p -> e ((flip f) p)

(m >>= g) >>= h 
 = \p -> (m >>= g) ((flip h) p)
 = \p -> (\p' -> m ((flip g) p')) ((flip h) p)
 = \p -> (m ((flip g) ((flip h) p))) 
 = \p -> m ((\y x -> g x y) ((flip h) p))
 = \p -> m ((\x -> g x ((flip h) p)))
 = \p -> m ((\p' x -> (g x) ((flip h) p')) p)
 = \p -> m ((flip (\x p' -> (g x) ((flip h) p'))) p)
 = \p -> m ((flip (\x -> (\p' -> (g x) ((flip h) p')))) p)
 = \p -> m ((flip (\x -> g x >>= h)) p)
 = m >>= (\x -> g x >>= h)

--}
