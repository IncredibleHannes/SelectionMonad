{-# LANGUAGE ImpredicativeTypes #-}
import Debug.Trace
import Control.Arrow

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

test2 = sequenceK es (\x -> trace "call" $ (sum x, x))

-- Isomorphism between the new K and Toms Special K

type KK r x = forall y. (x -> (r,y)) -> y

k2kk :: K r x -> KK r x
k2kk f = snd . f

kk2k :: KK r x -> K r x
kk2k f = \p -> f (\x -> let (r,y) = p x in (r, (r,y)))


{-- Proof of the isomorphism

k2kk(kk2k f)
 = k2kk(\p -> f (\x -> let (r,y) = p x in (r, (r,y))))
 = snd . (\p -> f (\x -> let (r,y) = p x in (r, (r,y))))
 = (\p -> (snd . f) (\x -> let (r,y) = p x in (r, (r,y)))) -- * 
 = (\p -> f (\x -> let (r,y) = p x in snd (r, (r,y))))
 = (\p -> f p)
 = f


kk2k(k2kk g)
 = kk2k(snd . g)
 = \p -> (snd . g) (\x -> let (r,y) = p x in (r, (r,y)))
 = \p -> g (\x -> let (r,y) = p x in snd (r, (r,y))) -- **
 = \p -> g p
 = g

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
--}


{--
would be a nice theorem to have but here is a counter example:
** Theorem of K
g :: forall y. (x -> (r,y)) -> (r,y)
Given:
f :: (r, a) -> (r, b)
p :: x -> (r, a)
(f . g) p = g (f . p)
--}
g' :: K Int Int
g' p = if r == 1 then (r,x) else (r,x)
    where (r, x) = p 1

g'' = kk2k(k2kk g')  
f' x = x+1
p' 1 = (1,1) 

--test'  = f'  (g'' p')  -- > (8,2)
--test'' = g'' ((id *** f') . p') -- > (10,2)


{--
** Theorem of K
g :: forall y. (x -> (r,y)) -> (r,y)
Given:
f :: a -> b
p :: x -> (r, a)
((id *** f) . g) p = g ((id *** f) . p)


-- (snd . g) p = g (snd . p)
--}

snd' :: (r,(r,x)) -> (r,x)
snd' = (id *** snd)
