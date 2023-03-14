{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
--bindK f g = j2k (bindJ (k2j f) (\x -> k2j (g x)))
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
--bindK f g = f . flip g 


-- ******************* Start alternative route *******************************
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (\x -> k2j (g x) p)))) p)                                                               -- definition k2j
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (\x -> (\f p ->  snd (f (\x -> (p x, x)))) (g x) p)))) p)                               -- lambda application
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (\x -> snd ((g x) (\x -> (p x, x))))))) p)                                              -- definition k2j  
--bindK f g = j2k (\p -> k2j (g ((\f p ->  snd (f (\x -> (p x, x)))) f (p . (\x -> snd ((g x) (\x -> (p x, x))))))) p)              -- lambda application
--bindK f g = j2k (\p -> k2j (g (snd (f (\x -> ( (p . (\x -> snd ((g x) (\x -> (p x, x))))) x, x))))) p)                            -- definition (.)
--bindK f g = j2k (\p -> k2j (g (snd (f (\x -> ( p (snd ((g x) (\x -> (p x, x)))), x))))) p)                                        -- definition k2j    
--bindK f g = j2k (\p -> (\f p ->  snd (f (\x -> (p x, x)))) (g (snd (f (\x -> ( p (snd ((g x) (\x -> (p x, x)))), x))))) p)        -- lambda application
--bindK f g = j2k (\p -> snd (g (snd (f (\x -> ( p (snd ((g x) (\x -> (p x, x)))), x)))) (\x -> (p x, x))))                         -- definition j2k
--bindK f g = (\f p -> p (f (fst . p))) (\p -> snd (g (snd (f (\x -> ( p (snd ((g x) (\x -> (p x, x)))), x)))) (\x -> (p x, x))))   -- lambda application
--bindK f g = \p -> p ((\p -> snd (g (snd (f (\x -> ( p (snd ((g x) (\x -> (p x, x)))), x)))) (\x -> (p x, x)))) (fst . p))         -- removing outer lambda     
--bindK f g p = p ((\p -> snd (g (snd (f (\x -> ( p (snd ((g x) (\x -> (p x, x)))), x)))) (\x -> (p x, x)))) (fst . p))             -- lambda application
--bindK f g p = p (snd (g (snd (f (\x -> ((fst . p) (snd ((g x) (\x -> ((fst . p) x, x)))), x)))) (\x -> ((fst . p) x, x))))        -- rewrite (.)
--bindK f g p = p (snd (g (snd (f (\x -> (fst ((p . snd) ((g x) (\x -> ((fst . p) x, x)))), x)))) (\x -> ((fst . p) x, x))))        -- Theorem 2
--bindK f g p = p (snd (g (snd (f (\x -> (fst ((g x) (p . snd . (\x -> ((fst . p) x, x)))), x)))) (\x -> ((fst . p) x, x))))        -- def (.)
--bindK f g p = p (snd (g (snd (f (\x -> (fst ((g x) (\x -> p (snd ((fst . p) x, x))))  , x)))) (\x -> ((fst . p) x, x))))          -- def snd
--bindK f g p = p (snd (g (snd (f (\x -> (fst ((g x) (\x -> p x)), x)))) (\x -> ((fst . p) x, x))))                                 -- remove lambda
--bindK f g p = p (snd (g (snd (f (\x -> (fst (g x p) , x)))) (\x -> ((fst . p) x, x))))                                            -- rewriting as lambda function
--bindK f g p = p (snd (g (snd (f (\x -> (\(r,y) -> (r,x)) (g x p)))) (\x -> ((fst . p) x, x))))                                    -- Theorem 2
--bindK f g p = p (snd (g (snd (f (\x -> g x ((\(r,y) -> (r,x)) . p)))) (\x -> ((fst . p) x, x))))                                  -- change lambda and def (.)
--bindK f g p = p (snd (g (snd (f (\x -> g x (\y -> (\z -> (fst z, x)) (p y))))) (\x -> ((fst . p) x, x))))                         -- apply lambda                              
--bindK f g p = p (snd (g (snd (f (\x -> g x (\y -> (fst (p y), x))))) (\x -> ((fst . p) x, x))))                                   -- theorem 1
--bindK f g p = g (snd (f(\x -> g x (\y -> (fst (p y), x) )))) p                                                                    -- rewrite with (.)
--bindK f g p = g (snd (f(\x -> g x (\y -> ((fst . p) y, x))))) p                                                                   -- rewrite g (snd (..)) as lambda
--bindK f g p = (\x y -> g (snd y) x) p ( f (\x -> (g x) (\y -> ((fst . p) y, x))))                                                 -- lambda application
--bindK f g p = (\y -> g (snd y) p) (f (\x -> (g x) (\y -> ((fst . p) y, x))))                                                      -- expand innermost lambda                                                     
--bindK f g p = (\y -> g (snd y) p) (f (\x -> (g x) (\y -> let (r,z) = p y in (r,x))))                                              -- todo: find theorem for this!!
--bindK f g p = (\(r,x) -> g x p)   (f (\x -> (g x) (\y -> let (r,z) = p y in (r,x))))                                              -- Theorem 2
--bindK f g p = f ((\(r,x) -> g x p) . (\x -> (g x) (\y -> let (r,z) = p y in (r,x))))                                              -- def (.)
--bindK f g p = f (\x -> (\(r,x) -> g x p) ((\x -> g x (\y -> let (r,z) = p y in (r,x))) x))                                        -- apply lambda
--bindK f g p = f (\x -> (\(r,y) -> g y p) ((g x) (\y -> let (r,z) = p y in (r,x))))                                                -- Theorem 2
--bindK f g p = f (\x -> g x ((\(r,y) -> g y p) . (\y -> let (r,z) = p y in (r,x))))                                                -- def (.)
--bindK f g p = f (\x -> g x ( \y -> (\(r,y) -> g y p) ((\y -> let (r,z) = p y in (r,x))y) ))                                       -- apply lambda
--bindK f g p = f (\x -> g x (\y -> (\(r,y) -> g y p) (let (r,z) = p y in (r,x))))                                                  -- resolve patternmatch
--bindK f g p = f (\x -> g x (\y -> (\z -> g (snd z) p) (let (r,z) = p y in (r,x))))                                                -- apply lambda
--bindK f g p = f (\x -> g x (\y -> g (snd (let (r,z) = p y in (r,x))) p ))                                                         -- remove snd
--bindK f g p = f (\x -> g x (\y -> g x p ))                                                                                        -- Theorem 3
--bindK f g p = f (\x -> g x p)                                                                                                     -- simplify
--bindK f g = f . flip g 


-- *********************** Theorems used in the substitution for bind ***********************
-- Assumption:
-- g :: K r x
-- forall p :: forall y . (x -> (r,y))
-- exists x :: x
-- such that:
-- g p = p x

-- Free theorem for K
-- g :: K r x
-- f :: a -> b
-- p :: x -> (r, a)
-- ((id *** f) . g) p = g ((id *** f) . p)

-- Theorem 1
-- If q does apply p to get the r value but keeps the original value, 
-- and we then use that original value to compute the (r,z) values with p
-- we can call g with p directly
-- p :: x -> (r,y)
-- g :: K r x
-- p (snd (g q)) = g p
--    where q = (\x -> ((fst . p) x, x))

-- (p . snd) (g q) = g (\x -> (p . snd) ((fst . p) x, x))

-- Proof: TODO!

-- Theorem 2
-- f :: (r,a) -> (r,b)
-- g :: K r x
-- p :: x -> (r,a)
-- f (g p) = g (f . p)
-- iff (fst . f . p) = fst . p

-- Should be proovable with the Free theorem for K and our assumption that g is not modifying the r value

-- Theorem Flase!!!! Counter example --{

f' (1,x) = (10,x)
f' x = x 
g' p = if p 1 == (1,1) then (1,1) else p 2
p' 1 = (1,1)
p' 2 = (2,2) 

--}

 


-- Proof: TODO!

-- Theorem 3 
-- g :: K r x
-- p :: x -> (r,a)
-- g (\y -> g p) = g p

-- Proof: TODO!

returnK :: x -> K r x
returnK x = j2k (returnJ x)
--equivalent by substitution
returnK' x p = p x


-- Thoughts about potentially useful theorems: 
-- Theorem 4
-- g :: K r x 
-- f :: x -> y
-- p :: y -> (r,z)
-- g (p . f) = (\(r,x) -> p (f x)) (g (\x -> let (r,z) = p (f x) in (r,x)))

-- theorem 5
-- p :: x -> (r,y)
-- f :: (r,y) -> z
-- g :: K r x 
-- f (g p) = snd g(\x -> let (r,y) = p x in (r,f(r,y))) 

-- theorem 6
-- p :: x -> (r,y)
-- g :: K r x
-- snd $ g (\x -> let (r,y) = p x in (r,(r,y))) = g p 