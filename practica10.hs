--Ej1a

--head :: [a] -> a
--tail :: [a] -> [a]
--pred :: Int -> Int

--Ej1b

--undefined
--[]
--undefined
--Los 10 primeros valores para los que el predicado de verdadero

--Ej2

at :: Int -> [a] -> a
at 1 (x:xs) = x
at n (x:xs) | n>1 = at (n-1) xs

--let f n = n^2 : f (n+1) in 
--at 3 (f 3)
--at 3 (9 : (f 4))
--at 2 (f 4)
--at 2 (16 : (f 5))
--at 1 (f 5)
--at 1 (25 : (f 6))
--25