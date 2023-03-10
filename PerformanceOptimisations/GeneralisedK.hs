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
--bindK' f g = j2k (bindJ (k2j f) (\x -> k2j (g x)))
--equivalent by substitution
--bindK f g = j2k (bindJ (k2j f) (\x -> k2j (g x)))                                                                                 -- definition of bindJ
--bindK f g = j2k ((\f g p -> g (f (p . flip g p)) p) (k2j f) (\x -> k2j (g x)))                                                    -- lambda application
--bindK f g = j2k ((\p -> (\x -> k2j (g x)) ((k2j f) (p . flip (\x -> k2j (g x)) p)) p))                                            -- lambda application
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (flip (\x -> k2j (g x))) p))) p)                                                        -- definition of flip
--bindK f g = j2k (\p -> k2j (g (k2j f (p . ((\x y -> (\x -> k2j (g x)) y x) ) p))) p)                                              -- lambda application
--bindK f g = j2k (\p -> k2j (g (k2j f (p . (\x y -> k2j (g y) x) p))) p)                                                           -- lambda application
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
--bindK f g p = p (snd (g (snd (f (\x -> ( (fst . p) (snd ((g x) (\x -> ((fst . p) x, x)))), x)))) (\x -> ((fst . p) x, x))))

-- all substutztuib and lambda application until this point!
--bindK f g p = p (snd (g (snd (f (\x -> ( (fst . p) (snd ((g x) (\x -> ((fst . p) x, x)))), x)))) (\x -> ((fst . p) x, x))))       -- by test'
--bindK f g p = p (snd ( g(snd (f(\x -> g x (\y -> (fst (p y), x) ))) ) (\x -> ((fst . p) x, x))))                                  -- theorem 1
--bindK f g p = g (snd (f(\x -> g x (\y -> (fst (p y), x) )))) p                                                                    -- rewrite with (.)
--bindK f g p = g (snd (f(\x -> g x (\y -> ((fst . p) y, x))))) p                                                                   -- rewrite g (snd (..)) as lambda
--bindK f g p = (\x y -> g (snd y) x) p ( f (\x -> (g x) (\y -> ((fst . p) y, x))))                                                 -- lambda application
--bindK f g p = (\y -> g (snd y) p) (f (\x -> (g x) (\y -> ((fst . p) y, x))))                                                      -- expand innermost lambda                                                     
--bindK f g p = (\y -> g (snd y) p) (f (\x -> (g x) (\y -> let (r,z) = p y in (r,x))))                                              -- todo: find theorem for this!!
--bindK f g p = f (\x -> (g x) (\y -> let (r,z) = p y in (r,z)))                                                                    -- reduce innermost lambda
--bindK f g p = f (\x -> g x p)                                                                                                     -- definition of flip
bindK f g = f . flip g 

-- Theorem 1
-- If q does apply p to get the r value but keeps the original value, 
-- and we then use that original value to compute the (r,z) values with p
-- we can call g with p directly
-- p :: x -> (r,y)
-- g :: K r x
-- p (snd (g q)) = g p
-- where q = (\x -> ((fst . p) x, x))

-- Proof: TODO!

-- Theorem 2
-- g :: K r x 
-- f :: x -> y
-- p :: y -> (r,z)
-- g (p . f) = (\(r,x) -> p (f x)) (g (\x -> let (r,z) = p (f x) in (r,x)))

x :: K r x -> (x -> K r y) -> K r y
--x = (\(r,x) -> g x p) (f (\x -> g x (\y -> let (r,z) = p y in (r,x))))
x f g p = (\(r,x) -> (g x) p) (f (\x -> g x (\y -> let (r,z) = p y in (r,x))))

-- theorem 3
-- p :: x -> (r,y)
-- f :: (r,y) -> z
-- f (g p) = snd g(\x -> let (r,y) = p x in (r,f(r,y))) 

-- theorem 4
-- p :: x -> (r,y)
-- g :: K r x
-- snd $ g (\x -> let (r,y) = p x in (r,(r,y))) = g p 

-- theorem 5
-- p :: x -> (r,a)
-- f :: K r x
-- g :: K r y
-- 


--( (fst . p) (snd ((g x) (\x -> ((fst . p) x, x))) ), x)

test' :: x -> (forall z . y -> (r,z)) -> (x -> K r y) -> (r,x)
test' x p g = (r,x)
    where 
        r = (fst . p . snd) (g x (\y -> let (r,z) = p y in (r, y)))

-- TODO: proof they are equal
test'' :: x -> (forall z . y -> (r,z)) -> (x -> K r y) -> (r,x)
test'' x p g = g x (\y -> (fst (p y), x))


returnK :: x -> K r x
returnK x = j2k (returnJ x)
--equivalent by substitution
returnK' x p = p x
