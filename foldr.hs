map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' f [] = []
filter' f (x:xs) = if (f x) then (x:(filter' f xs)) else filter' f xs

foldr' :: (b -> a -> a) -> a -> [b] -> a

foldr' f base [] = base
foldr' f base (x:xs) = f x (foldr' f base xs)

foldMap' f = foldr' (\x foldedList -> f x : foldedList) []
foldFilter' f = foldr' (\x foldedList -> if f x then x : foldedList else foldedList) []

unfoldr' :: (b -> Maybe (a, b)) -> (b -> [a])
unfoldr' f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []