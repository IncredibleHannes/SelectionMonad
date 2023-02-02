import Data.HashMap.Strict as H
import Control.Monad.State
import Debug.Trace

type J r x = (x -> r) -> x

maxWith :: Ord b => (a -> b) -> [a] -> a
maxWith f = foldr1 (bigger f)
  where bigger f x y = if f x >= f y then x else y


sequence' :: [J r x] -> J r [x]
sequence' [] p     = []
sequence' (e:es) p = b : bs
    where
        b = e (\a -> p (a : sequence' es (p . (a:))))
        bs = sequence' es (p . (b:))

sequence'' :: String -> [JS r Char] -> JS r String
sequence'' _ [] p     = return []
sequence'' h (e:es) p = do {
    b <- do {
      e (\a -> do {
            rest <- sequence'' (h ++ [a]) es p;
            s <- get;
            if null h then do {
              put (insert (h ++ [a]) rest s);
              p (h ++ [a] ++ rest)
            } else p (h ++ [a] ++ rest)
      })
    };
    s <- get;
    case H.lookup (h ++ [b]) s of
      (Just a) ->  return [b] ++ a
      Nothing -> do {
        bs <- sequence'' (h ++ [b]) es p;
        return (b : bs)
      }
  }

type MState = HashMap String String 

type JS r x = (x -> State MState r) -> State MState x



type Password = String

password = "secr"

p :: String -> Bool
p "secret"    = True
p [_,_,_,_,_,_] = False

selectChar :: (Char -> Bool) -> Char
selectChar p = maxWith p ['a' .. 'z']

solution = sequence' [selectChar,selectChar,selectChar,selectChar,selectChar,selectChar] p

selectChar' :: (Char -> State MState Bool) -> State MState Char
selectChar' p = helper p ['a'..'z']

helper :: (Char -> State MState Bool) -> [Char] -> State MState Char
helper p [a]    = return a
helper p (a:as) = do p a >>= \result -> if result then return a else helper p as

solution' = runState (sequence'' "" [selectChar',selectChar',selectChar',selectChar',selectChar'] (return . p)) empty
