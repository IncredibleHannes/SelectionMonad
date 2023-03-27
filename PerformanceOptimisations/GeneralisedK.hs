{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ((>>=), return, pure, (<*>), fmap, sequence)

-- known selection monad
type J r x = (x -> r) -> x

bindJ :: J r x -> (x -> J r y) -> J r y
bindJ f g p = g (f (p . flip g p)) p

returnJ :: x -> J r x
returnJ x p = x

-- new generalised K that is isomorphic to J
type K r x = forall y. (x -> (r,y)) -> (r,y)

-- equivalent to (j2kk . kk2k) from ealier
j2k :: J r x -> K r x
j2k f p = p (f (fst . p))

-- equivalent to (kk2j . k2kk) from ealier
k2j :: K r x -> J r x
k2j f p = snd (f (\x -> (p x, x)))


{- Therefore proof should be the same as combining both proofs (K <-> KK iso) and (KK <-> J iso)
So: assuming that for 
g :: K r x
forall p :: forall y . (x -> (r,y))
exists x :: x
such that:
g p = p x

these K's are isomorpic to J
-}

bindK :: K r x -> (x -> K r y) -> K r y
bindK f g = j2k (bindJ (k2j f) (\x -> k2j (g x)))
--equivalent by substitution
--bindK f g = j2k (bindJ (k2j f) (\x -> k2j (g x)))                                                         -- definition of bindJ
--bindK f g = j2k ((\f g p -> g (f (p . flip g p)) p) (k2j f) (\x -> k2j (g x)))                            -- lambda application
--bindK f g = j2k ((\p -> (\x -> k2j (g x)) ((k2j f) (p . flip (\x -> k2j (g x)) p)) p))                    -- lambda application
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (flip (\x -> k2j (g x))) p))) p)                                -- definition of flip
--bindK f g = j2k (\p -> k2j (g (k2j f (p . ((\x y -> (\x -> k2j (g x)) y x) ) p))) p)                      -- lambda application
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (\x y -> k2j (g y) x) p))) p)                                   -- lambda application
-- *************** Alternative route below *******************
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (\x -> k2j (g x) p)))) p)                                       -- def j2k
--bindK f g p = p ( k2j (g (k2j f ((fst . p) . (\x -> k2j (g x) (fst . p))))) (fst . p))                    -- apply lambda
--bindK f g p = p (k2j (g (k2j f (fst . p . (\x -> k2j (g x) (fst . p))))) (fst . p))                       -- def k2j
--bindK f g p = p (k2j (g (k2j f (fst . p . (\x -> snd ((g x) (\x -> ((fst . p) x, x))))))) (fst . p))      -- rewrite lambda
--bindK f g p = p (k2j (g (k2j f (\x -> (fst . p . snd) ((g x) (\x -> ((fst . p) x, x)))  ))) (fst . p))    -- rewrite composition
--bindK f g p = p (k2j (g (k2j f (\x -> fst ((p . snd) ((g x) (\x -> ((fst . p) x, x))))))) (fst . p))      -- theorem 2
--bindK f g p = p (k2j (g (k2j f (\x -> fst (((g x) (\x -> (p . snd) ((fst . p) x, x))))))) (fst . p))      -- simplify
--bindK f g p = p (k2j (g (k2j f (\x -> fst (g x p)))) (fst . p))                                           -- def j2k
--bindK f g p = p (k2j (g (snd (f (\x -> ((\x -> fst (g x p)) x, x))))) (fst . p))                          -- apply lambda
--bindK f g p = p (k2j (g (snd (f (\x -> (fst (g x p), x))))) (fst . p))                                    -- def j2k                     
--bindK f g p = p (snd (g (snd (f (\x -> (fst (g x p), x)))) (\x -> ((fst . p) x, x))))                     -- Theorem 1
--bindK f g p = g (snd (f (\x -> (fst (g x p), x)))) p                                                      -- rewrite g (snd (..)) as lambda
--bindK f g p = (\y -> g (snd y) p) (f (\x -> (fst (g x p), x)))                                            -- Theorem 2
--bindK f g p = f ((\y -> g (snd y) p) . (\x -> (fst (g x p), x)))                                          -- def (.)    
--bindK f g p = f (\x -> (\y -> g (snd y) p) (fst (g x p), x) )                                             -- apply lambda and simplify
--bindK f g p = f (\x -> g x p)


-- *********************** Theorems used in the substitution for bind ***********************
{-
Assumption:
g :: K r x
forall p :: forall y . (x -> (r,y))
exists x :: x
such that:
g p = p x


Free theorem for K
g :: K r x
f :: a -> b
p :: x -> (r, a)
((id *** f) . g) p = g ((id *** f) . p)


Theorem 2
f :: (r,a) -> (r,b)
g :: K r x
p :: x -> (r,a)
f (g p) = g (f . p)

iff (fst . f . p) = fst . p

Proof:
f (g p)
exists x ->                                                                          -- Assumption
= f (p x)                                                                            -- rewrite as tuples 
= ((fst . f . p) x, (snd . f . p) x)                                                 -- Theorem 2 condition
= ((fst . p ) x , (snd . f . p) x)                                                   -- rewrite as let
= let (r, y) = p x in (r, snd (f (r,y)))                                             -- rewrite as ***
= let (r, y) = p x in (id *** (\y -> snd (f (r,y)))) (r, y)                          -- resolve ***
= let (r, y) = p x in (\(a,b) -> (a, (\y -> snd (f (r,y))) b)) (r, y)                -- apply lambda
= let (r, y) = p x in (\(a,b) -> (a, snd (f (r,b)))) (r, y)                          -- expand let
= let r = fst (p x) in let y = snd (p x) in (\(a,b) -> (a, snd (f (r,b)))) (r, y)    -- remove let
= (\(a,b) -> (a, snd (f (fst (p x), b)))) ((fst (p x)), (snd (p x)))                 -- simplify
= (\(a,b) -> (a, snd (f (fst (p x), b)))) p x                                        -- remove patternmatch in lambda
= (\a -> (fst a, snd (f (fst (p x), snd a)))) p x                                    -- replace (p x) with a within lambda
= (\a -> (fst a, snd (f (fst a, snd a)))) p x                                        -- add patternmatch to lamvda
= (\(r,y) -> (r, snd (f (r, y)))) p x                                                -- Assumption
= (\(r,y) -> (r, snd (f (r, y)))) g p                                                -- free Theorem
= g ((\(r,y) -> (r,  snd (f (r,y)))) . p)                                            -- rewrite (.)
= g (\x -> (\(r,y) -> (r,  snd (f (r,y)))) (p x))                                    -- pull (p x) into lambda
= g (\x -> (fst (p x),  snd (f (fst (p x) ,snd (p x)))))                             -- simplify tuple to p x
= g (\x -> (fst (p x),  snd (f (p x))))                                              -- rewrite with (.)
= g (\x -> ((fst . p) x, (snd . f . p) x))                                           -- expand first bit with theorem condition
= g (\x -> ((fst . f . p) x, (snd . f . p) x))                                       -- simplify tuple to (f . p) x
= g (\x -> (f . p) x)                                                                -- remove lambda
= g (f . p)

-}

-- Theorem 1
-- If q does apply p to get the r value but keeps the original value, 
-- and we then use that original value to compute the (r,z) values with p
-- we can call g with p directly
-- p :: x -> (r,y)
-- g :: K r x
-- p (snd (g q)) = g p
--    where q = (\x -> ((fst . p) x, x))

-- proof with Theorem 2
-- (p . snd) (g q) = g (\x -> (p . snd) ((fst . p) x, x))
-- (p . snd) (g q) = g p
-- 
-- iff 
-- (fst . p . snd) (\x -> ((fst . p) x, x))
-- = \y -> (fst ( p (snd ( (\x -> ((fst . p) x, x)) y))))
-- = \y -> (fst(p(snd ((fst . p) y, y) )))
-- = \x -> (fst . p) x
-- = fst . (\x -> ((fst . p) x, x)) 
 

returnK :: x -> K r x
returnK' x = j2k (returnJ x)
--equivalent by substitution
returnK x p = p x


sequence :: [K r x] -> K r [x]
sequence [] p     = p []
sequence (e:es) p = e (\x -> sequence es (\xs -> p (x:xs)))

sequenceH :: [[x] -> K r x] -> [x] -> K r [x]
sequenceH [] h p = p []
sequenceH (e:es) h p = e h (\x -> sequenceH es (h ++ [x]) (\xs -> p (x:xs)))

foldMK :: (b -> a -> K r b) -> b -> [a] -> K r b
foldMK f z0 []     = returnK z0
foldMK f z0 (e:es) = bindK (f z0 e) (\y -> foldMK f y es)

sequenceFK :: [K r x] -> K r [x]
sequenceFK = foldMK f []
    where 
        f :: [x] -> K r x -> K r [x]
        f b a p = a (\x -> p(b ++ [x]))

sequenceBind :: [K r x] -> K r [x]
sequenceBind [] = returnK []
sequenceBind (e:es) = bindK e (\x -> bindK (sequenceBind es) (\xs -> returnK (x:xs)))

--sequenceFKH :: [x] ->  [[x] -> K r x] -> K r [x]
--sequenceFKH = foldMK f
--    where 
--        f :: [x] -> ([x] -> K r x) -> K r [x]
--        f b a p = a b (\x -> p(b ++ [x]))


-- examples

minWith :: Ord r => forall y . [x] -> (x -> (r,y)) -> (r,y)
minWith xs f = foldr1 (\(r,x) (s,y) -> if r < s then (r,x) else (s,y) ) (map f xs)

maxWith :: Ord r => forall y . [x] -> (x -> (r,y)) -> (r,y)
maxWith xs f = foldr1 (\(r,x) (s,y) -> if r > s then (r,x) else (s,y) ) (map f xs)


n = 50

p :: [Int] -> (Int, [Int])
p x = (sum x, x)

es :: [K Int Int]
es = [e1,e2,e1,e2]
  where 
    e1 = minWith [1..n]
    e2 = maxWith [1..n]

test1 = sequence es p 
test2 = sequenceFK es p
test3 = sequenceBind es p 
test4 = sequence' es p 
test5 = sequence'' es p 

-- \f g p -> f (\x -> g x p)

sequence' :: [K r x] -> K r [x]
sequence' [] p     = p []
-- sequence' (e:es) = bindK e (\x -> bindK (sequence' es) (\xs -> returnK (x:xs)))
-- sequence' (e:es) = \p -> e (\x -> (bindK (sequence' es) (\xs -> returnK (x:xs))) p)
-- sequence' (e:es) = \p -> e (\x -> ((\p -> (sequence' es) (\xs -> (returnK (x:xs)) p))) p)
-- sequence' (e:es) = \p -> e (\x -> ((\p -> (sequence' es) (\xs -> p (x:xs)))) p)
-- sequence' (e:es) p = e (\x -> (\p -> sequence' es (\xs -> p (x:xs))) p)

sequence' (e:es) p = e (\x -> sequence' es (\xs -> p (x:xs)))


sequence'' :: [K r x] -> K r [x]
sequence'' [] p     = p []
sequence'' (e:es) p = p (a:as)
    where (_,a) = e (\x -> (fst $ p (x : snd (sequence'' es (\y -> (fst $ p (x:y), y)))), x))
          (_,as) = sequence'' es (\y -> (fst $ p (a:y), y))


pairK :: K r x -> K r y -> K r (x,y)
pairK f g p = f (\x -> g(\y -> p (x,y)))



pairK' :: K r x -> K r y -> K r (x,y)
--pairK' f g p = p (a,b)
--    where (_,a) = f (\x -> (fst $ g (\y ->(fst $ p(x,y), y)), x))
--          (_,b) = g (\y -> (fst $ p (a,y), y))

--pairK' f g p = p (a,b)
--    where (_,a) = f (\x -> (fst $ g (\y -> p(x,y)), x))
--          (_,b) = g (\y -> (fst $ p (a,y), y))

--pairK' f g p = let a = snd $ f (\x -> (fst $ g (\y -> p(x,y)), x)) in p (a,snd $ g (\y -> (fst $ p (a,y), y)))
--pairK' f g p = p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x)), snd $ g (\y -> (fst $ p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x)),y), y)))

--pairK' f g p = curry p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x))) (snd $ g (\y -> (fst $ p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x)),y), y)))

--pairK' f g p = g (curry p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x))))
--pairK' f g p = g (\x -> (curry p) (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x))) x)
--pairK' f g p = g (\y -> p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x)), y) )

pairK' f g p = g (\y -> p (snd $ f (\x -> (fst $ g (\y -> p(x,y)), x)), y) )
--pairK' f g p = g (\y -> p (snd $ f (\x -> (fst $ p (x,y), x)), y) )


--pairK' f g p = g (\y -> p (snd (f (\x -> (fst (p (x,y)) , x))),y))
--pairK' f g p = g (\y -> (\x -> p (x,y)) (snd (f (\x -> (fst (p (x,y)) , x)))))
--pairK' f g p = g (\y -> (\x -> p (x,y)) (snd (f (\x -> ((fst . (\x -> p (x,y))) x, x)))))   -- Theorem 1
--pairK' f g p = g (\y -> f (\x -> p (x,y)))

{--
Theorem 2
f :: (r,a) -> (r,b)
g :: K r x
p :: x -> (r,a)
f (g p) = g (f . p)

iff (fst . f . p) = fst . p

Theorem 1
p (snd (g q)) = g p
    where q = (\x -> ((fst . p) x, x))

--}