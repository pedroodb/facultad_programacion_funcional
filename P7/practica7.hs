--Ej2

pal :: [Char] -> Bool
pal cs = foldr (\(c,c') -> (&&) (c == c')) True (zip cs (reverse cs))

hs :: [[Char]] -> Int
hs = foldr ((+).((\h -> if h then 1 else 0).((=='h').head))) 0

--avgLen :: [[a]] -> Float
--avgLen xs = ((/) s q) where (s,q) = foldr (\x -> \(s',q') -> (s' + (length x), q' + 1)) (0,0) xs

--Ej5

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile p = foldr (\x r -> if p x then x:r else []) []

--dropwhile :: (a -> Bool) -> [a] -> [a]
--dropwhile p 