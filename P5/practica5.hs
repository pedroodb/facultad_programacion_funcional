--Ej2

sum' :: [Int]->Int
sum' [] = 0
sum' (x:xs) = x + sum(xs)

remainders :: [Int]->Int->[Int]
remainders [] n = []
remainders (x:xs) n = ((mod x n):remainders xs n)

squares :: [Int]->[Int]
squares [] = []
squares (x:xs) = (x*x):squares xs

pairs :: [Int]->[Int]
pairs [] = []
pairs (x:xs) = if (mod x 2) == 0
  then (x:pairs xs)
  else pairs xs

--Ej7

ifThenElse_Lam = \x -> x
true_Lam = \x -> \y -> x
false_Lam = \x -> \y -> y
not_Lam = \x -> ifThenElse_Lam x false_Lam true_Lam

or_Lam = \x -> \y -> x true_Lam y
and_Lam = \x -> \y -> x y false_Lam
xor_Lam = \x -> \y -> x (not_Lam y) y --No funciona cuando el primero es falso?
iff_Lam = \x -> \y -> x y true_Lam

--Ej8

pair_Lam = \x -> \y -> \b -> ifThenElse_Lam b x y

fst_Lam p = p true_Lam
snd_Lam p = p false_Lam
