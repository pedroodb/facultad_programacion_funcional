suma (a,b) = a+b

curry' f = (\x -> (\y -> f (x,y)))
uncurry' f = (\(x,y) -> f x y)

fix:: ((a -> b) -> a -> b) -> a -> b
fix f x = f (fix f) x

--Ej5 
twice = (\f -> (\x -> f (f x)))
flip = (\f -> (\x -> (\y -> f y x)))
inc = (\x -> x+1)
