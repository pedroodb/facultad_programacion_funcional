first :: (Int,Int) -> Int
first (x,y) = x

second :: (Int,Int) -> Int
second (x,y) = y

const :: Int -> Int -> Int
const x y = x

compose :: (a -> b) -> (c -> a) -> c -> b
compose f g = (\x -> f (g x))

apply :: (a -> b) -> a -> b
apply f x = f x

subst :: (a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)

pairFunc :: (a -> b, b -> a) -> b -> a -> (b, a)
pairFunc (f1,f2) x y = (f1 (f2 x), f2 (f1 y))

--Ej 8)

data ColorPrimario = Amarillo|Azul|Rojo deriving Eq Show
data ColorSecundario = Naranja|Violeta|Verde

data Punto = Punto2D Int Int | Punto3D Int Int distintas

suma (Punto2D x1 y1) (Punto2D x2 y2) = Punto2D (x1 + x2) (y1 + y2)

-- Ej 10)
--tom x = x x

-- Ej 11)
--smaller??

second' :: a -> a
second' = \x -> x

andThen :: Bool -> Bool -> Bool
andThen = \x -> \y -> x && y

iff :: Bool -> Bool -> Bool
iff x y = if x then not y else y

--Las distintas x son lo mismo??
--alpha x x = x

-- 13) ??
