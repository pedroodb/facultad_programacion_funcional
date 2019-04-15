
--Ej5

twice = (\f -> (\x -> f (f x)))
flip = (\f -> (\x -> (\y -> f y x)))
inc = (\x -> x+1)

--Ej6

fix :: ((a -> b) -> a -> b) -> a -> b
fix f x = f (fix f) x

--Ej11

parseDigit d 
  | d == '0' = 0
  | d == '1' = 1
  | d == '2' = 2
  | d == '3' = 3
  | d == '4' = 4
  | d == '5' = 5
  | d == '6' = 6
  | d == '7' = 7
  | d == '8' = 8
  | d == '9' = 9

sumDigit = (+) . parseDigit
