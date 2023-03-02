import qualified Data.HashTable.IO as H
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

sequence'' :: String -> HashTable String String -> [JS r Char] -> JS r String
sequence'' _ s [] p     = return []
sequence'' h s (e:es) p = do {
    b <- do {
      e (\a -> do {
            rest <- sequence'' (h ++ [a]) s es p;
            --if h == [] then do {
              H.insert s (h ++ [a]) rest;
              p (h ++ [a] ++ rest)
            --} else p (h ++ [a] ++ rest)
      })
    };
    l <- H.lookup s (h ++ [b]);
    case l of
      (Just a) -> return [b] ++ a
      Nothing -> do {
        bs <- sequence'' (h ++ [b]) s es p;
        return (b : bs)
      }
  }

type JS r x = (x -> IO r) -> IO x

type Password = String

password = "secr"

p :: String -> Bool
p "secr"    = True
p [_,_,_,_] = False

selectChar :: (Char -> Bool) -> Char
selectChar p = maxWith p ['a' .. 'z']

solution = sequence' [selectChar,selectChar,selectChar,selectChar] p

selectChar' :: (Char -> IO Bool) -> IO Char
selectChar' p = helper p ['a'..'z']

helper :: (Char -> IO Bool) -> [Char] -> IO Char
helper p [a]    = p a >>= return a
helper p (a:as) = do p a >>= \result -> if result then return a else helper p as

type HashTable k v = H.BasicHashTable k v

solution' = do {
  e <- H.new;
  sequence'' "" e [selectChar',selectChar',selectChar',selectChar'] (return . p)
}
