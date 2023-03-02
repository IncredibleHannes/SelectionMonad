import Control.Monad.State
import Debug.Trace

maxWith :: Ord b => (a -> b) -> [a] -> a
maxWith f = foldr1 (bigger f)
  where bigger f x y = if f x >= f y then x else y

sequence'' :: String -> [JS r Char] -> JS r String
sequence'' _ [] p s    =  (s, [])
sequence'' h (e:es) p s = (s''', b : bs)
    where 
      (s'', b) = e (\a s' -> 
          let (s'', rest) = sequence'' (h ++ [a]) es p s' in
          if null h then p (h ++ [a] ++ rest) (([a], rest) : s'')
          else p (h ++ [a] ++ rest) s''
        ) s
      (s''', bs) = case lookup' [b] s'' of
        (Just (a,b)) -> trace (show (a,b)) (s'', a ++ b)
        Nothing -> sequence'' (h ++ [b]) es p s''

lookup' :: String -> [(String, String)] -> Maybe (String, String)
lookup' _ [] = Nothing
lookup' s ((k,v):xs) = if s == k then Just (k,v) else lookup' s xs

type JS r x = (x -> [(String,String)] -> ([(String,String)], r)) -> [(String,String)] -> ([(String,String)], x)

type Password = String


p :: String -> [(String,String)] -> ([(String,String)], Bool)
p "secre" s   = (s, True)
p _ s = (s, False)
p [_,_,_,_,_,_] s = (s, False)

selectChar :: (Char -> Bool) -> Char
selectChar p = maxWith p ['a' .. 'z']

--solution = sequence' [selectChar,selectChar,selectChar,selectChar,selectChar,selectChar] p

selectChar' :: (Char -> [(String, String)] ->  ([(String,String)], Bool)) -> [(String,String)] -> ([(String,String)], Char)
selectChar' p = helper p ['a'..'z'] 

helper :: (Char -> [(String, String)] -> ([(String,String)], Bool)) -> [Char] -> [(String,String)] -> ([(String,String)], Char)
helper p [a] s    = (s,a)
helper p (a:as) s = if result then (s',a) else helper p as s'
  where
    (s', result) = p a s

solution' = sequence'' "" [selectChar',selectChar',selectChar',selectChar',selectChar'] p []
