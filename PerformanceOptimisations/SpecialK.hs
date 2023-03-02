{-# LANGUAGE RankNTypes #-}

import Debug.Trace
import Control.Monad (ap)
import Data.Function (on)
import Data.List

-- Standard Selection Monad

newtype J r x = MkJ { runJ :: (x -> r) -> x }

instance Functor (J r) where
  fmap f (MkJ t) = MkJ (\p -> f (t (\x -> p (f x))))

instance Applicative (J r) where
  pure x = MkJ (\p -> x)
  (<*>) = ap

instance Monad (J r) where
  return = pure
  m >>= k = MkJ (\p -> (runJ . k) (runJ m (p . flip (runJ . k) p)) p)

-- Special K: Selection Monad with Alternative Result

newtype K r x = MkK { runK :: forall y. (x -> (r, y)) -> y }

k2j :: K r x -> J r x
k2j f = MkJ (\p -> runK f (\x -> (p x, x)))

j2k :: J r x -> K r x
j2k f = MkK (\p -> snd (p (runJ f (\x -> fst (p x)))))

instance Functor (K r) where
  fmap f (MkK t) = MkK (\p -> t (\x -> p (f x)))

instance Applicative (K r) where
  pure x = MkK (\p -> snd (p x))
  (<*>) = ap

instance Monad (K r) where
  return = pure
  m >>= k = MkK (\p -> (runK m) (flip (runK . k) (\y -> let (r, z) = p y in (r, (r, z)))))

minimumWith :: Ord b => [a] -> (a -> b) -> a
minimumWith xs f = minimumBy (compare `on` f) xs

minimumWith :: Ord b => [a] -> (a -> b) -> a
minimumWith xs f = minimumBy (compare `on` f) xs

j1 :: J Int String
j1 = MkJ (minimumWith ["alice","bob","charlotte","douglas"])

j2 :: J Int Int
j2 = MkJ (minimumWith [50,20,70,10])

pairJ :: J r a -> J r b -> J r (a,b)
pairJ f g = MkJ (\p -> let a = runJ f (\x -> trace "p1" $ p (x, runJ g (\y ->  trace "p2" $ p (x,y))))
                           b = runJ g (\y -> trace "p3" $ p (a, y))
                       in (a, b)) 

testJ :: (String, Int)
testJ = runJ (pairJ j1 j2) (\(s, n) ->  abs (length s - n))

minimumWithK :: Ord b => [a] -> (a -> (b, c)) -> c
minimumWithK xs f = snd (minimumBy (compare `on` fst) (map f xs))

k1 :: K Int String
k1 = MkK (minimumWithK ["alice","bob","charlotte","douglas"])

k2 :: K Int Int
k2 = MkK (minimumWithK [50,20,70,10])

pairK :: K r a -> K r b -> K r (a, b)
pairK f g = MkK (\p -> runK f (\x -> runK g (\y -> let (r, z) = p (x,y) in (r, (r, z)))))

testK :: (String, Int)
testK = runJ (k2j $ pairK k1 k2) (\(s, n) -> trace "call" $ abs (length s - n))
