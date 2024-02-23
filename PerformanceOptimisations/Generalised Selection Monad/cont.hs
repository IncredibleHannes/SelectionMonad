

type K r a = (a -> r) -> r

sequence' :: [K r a] -> K r [a]
sequence' [] p = p []
sequence' (e:es) p = p (x:xs)
    where 
        x = e (\x -> p [x])
        xs = sequence' es p