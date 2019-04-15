--Ej1

import Data.List

range :: Int -> Int -> [Int]
range x y = unfoldr (\x' -> if (x' == y) then Nothing else Just(x', x'+1)) x

dividesTo :: Int -> Int -> Bool
dividesTo x y = mod x y == 0

nextDiv :: Int -> Int -> Int
nextDiv x y = head (filter (dividesTo y) (range (x+1) (y+1)))

sumDivs :: Int -> Int
sumDivs x = sum (filter (dividesTo x) (range 1 (x+1)))
