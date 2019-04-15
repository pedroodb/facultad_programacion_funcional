--Ej 1
seven :: Int
seven = 7

sign :: Int -> Bool
sign x = if x>=0 then True else False

sign' :: Int -> Bool
sign' x
  | x>=0 = True
  | x<0 = False

absolute :: Int -> Int
absolute x
  | x>=0 = x
  | x<0 = -x

and' :: Bool -> Bool -> Bool
and' x y
  | x = y
  | otherwise = x

or' :: Bool -> Bool -> Bool
or' x y
  | x = x
  | otherwise = y

not' :: Bool -> Bool
not' x
  | x = False
  | otherwise = True

xor' :: Bool -> Bool -> Bool
xor' x y
  | x == y = True
  | otherwise = False

dividesTo :: Int -> Int -> Bool
dividesTo x y = mod y x == 0

isMultiple :: Int -> Int -> Bool
isMultiple x y = dividesTo y x

isCommonDivisor :: Int -> (Int, Int) -> Bool
isCommonDivisor x (y, z) = and' (dividesTo x y) (dividesTo x z)

isCommonMult :: Int -> (Int, Int) -> Bool
isCommonMult x (y, z) = and' (isMultiple x y) (isMultiple x z)

swap :: (Int, Int) -> (Int, Int)
swap (x, y) = (y, x)

--Ej 2

f x = x

greaterThan :: Int -> Int -> Bool
greaterThan x y = x > y

f' (x, y) = x

--Ej 4

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

--Ej5
{-
  eficiencia
  corrección
  claridad
  modificabilidad
  terminación
  equivalencia
  generalidad
  simplicidad
-}

--Ej6
{-
Transparencia referencial: 
consideración sólo del comportamiento externo de un programa (abstracción de detalles de ejecución).
posibilidad de demostrar propiedades usando las propiedades de las subexpresiones y métodos de deducción lógica.

//where define funciones dentro de otras para qué sean solo visibles en la funcion en la qué se definen
//let in sirve para lo mismo
-}
suma2 x = x+1+1
suma2 x = inc(inc x) where inc z = z+1
suma2 x = let adic=2 in x+adic

--Ej8
--No es posible porque viola la Transparencia Referencial


--Ej 9

esBisiesto :: Int -> Bool
esBisiesto x = let ultimasCifras = x `mod` 100 in ((ultimasCifras `mod` 4==0) && (ultimasCifras/=0)) || (x `mod` 400==0)
