map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' f [] = []
filter' f (x:xs) = if (f x) then (x:(filter' f xs)) else filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b

foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

--Map defined using foldr'
foldMap' f = foldr' (\x foldedList -> f x : foldedList) []

--Filter defined using foldr'
foldFilter' f = foldr' (\x foldedList -> if f x then x : foldedList else foldedList) []

--unfoldr' :: (b -> Maybe (a, b)) -> (b -> [a])
--unfoldr' f b = case f b of
--                Just (a, b') -> a : unfoldr f b'
--                Nothing -> []

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z xs = foldr' (\x h -> \xs' -> f x (tail xs') (h (tail xs'))) (const z) xs xs