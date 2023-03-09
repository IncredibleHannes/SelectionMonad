{-# LANGUAGE ImpredicativeTypes #-}
import Debug.Trace
import Control.Arrow
import Prelude hiding ((>>=), return, pure, (<*>), fmap, sequence)

-- tests with more general J without the generalised result type
type J r x =  (x -> (r,x)) -> (r,x)


-- not working
sequenceJ :: [J r x] ->  J r [x]
sequenceJ [e] p = let (r , x) = e (\x -> let (r, [x]) = p [x] in (r,x)) in (r, [x])
sequenceJ (e:es) p = let (r , x) = e (\x -> let (r, y) = p (y ++ snd (sequenceJ es (\z -> p (y++z)) )) in (r,x)) in (r, x : snd (sequenceJ es (\y -> p (x:y)) ))

pair :: J r x -> J r y -> J r (x,y)
pair f g p = (r, (x,y))
    where
        (r,x) = f (\x -> let (s, y) = g (\y -> let (t,(a,b)) = p (x,y) in (t,b)) in (s,x) )
        (_,y) = g (\y -> let (t,(_,b)) = p (x,y) in (t,b))

test = pairK (minWith [1..4]) (maxWith [2..4]) (\(x,y) -> (x + y, (x,y)))

es' :: [J Int Int]
es' = [minWith [1..4], maxWith [1..4],minWith [1..4], maxWith [1..4]]

test3 = sequenceJ es' (\x -> trace "call" $ (sum x, x))

-- New generalised K that returns the r along with the result

type K r x = forall y. (x -> (r,y)) -> (r,y)

-- min and max with function that work well with the new K
minWith :: Ord r => forall y . [x] -> (x -> (r,y)) -> (r,y)
minWith xs f = foldr1 (\(r,x) (s,y) -> if r < s then (r,x) else (s,y) ) (map f xs)

maxWith :: Ord r => forall y . [x] -> (x -> (r,y)) -> (r,y)
maxWith xs f = foldr1 (\(r,x) (s,y) -> if r > s then (r,x) else (s,y) ) (map f xs)

-- pair and sequence for the new K

pairK :: K r x ->  K r y -> K r (x,y)
pairK f g p = f (\x -> g(\y -> p(x,y)))

sequenceK :: [K r x] -> K r [x]
sequenceK [e] p = e (\x -> p [x])
sequenceK (e:es) p = e (\x -> sequenceK es (\xs -> p (x:xs)))

hsequenceK :: [[x] -> K r x] -> [x] -> K r [x]
hsequenceK [e] h p = e h (\x -> p [x])
hsequenceK (e:es) h p = e h (\x -> hsequenceK es (h ++ [x]) (\xs -> p (x:xs)))

-- tests for the new K

es :: [K Int Int]
es = [minWith [1..4], maxWith [1..4],minWith [1..4], maxWith [1..4]]

test2 = sequence es (\x -> trace "call" $ (sum x, x))

-- Isomorphism between the new K and Toms Special K

type KK r x = forall y. (x -> (r,y)) -> y

k2kk :: K r x -> KK r x
k2kk f = snd . f

kk2k :: KK r x -> K r x
kk2k f = \p -> f (\x -> let (r,y) = p x in (r, (r,y)))


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
e >>= f = \p -> e ((flip f) p)

return :: x -> K r x
return x p = p x 

(<*>) :: K r (x -> y) -> K r x -> K r y
f <*> g = \p -> f (\e -> g (p . e))

pure :: x -> K r x
pure = flip ($)

fmap :: (x -> y) -> K r x -> K r y
fmap f e = \p -> e (p . f)


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
