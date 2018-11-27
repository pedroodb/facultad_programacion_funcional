sum'::[Int]->Int
sum' [] = 0
sum' (x:xs) = x + sum(xs)

remainders::[Int]->Int->[Int]
remainders [] n = []
remainders (x:xs) n = ((mod x n):remainders xs n)

squares::[Int]->[Int]
squares [] = []
squares (x:xs) = (x*x):squares xs

pairs::[Int]->[Int]
pairs [] = []
pairs (x:xs) = if (mod x 2) == 0
  then (x:pairs xs)
  else pairs xs
