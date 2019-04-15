--Ej1

--Resuelto en practica 6

--Ej2

data BinTree a = Empty | Bin a (BinTree a) (BinTree a) deriving Show

nodesBin :: BinTree a -> Int
nodesBin Empty = 0
nodesBin (Bin x t1 t2) =  1 + (nodesBin t1) + (nodesBin t2)

heightBin :: BinTree a -> Int
heightBin Empty = 0
heightBin (Bin x t1 t2) = 1 + (max (heightBin t1) (heightBin t2))

mapBin :: (a -> b) -> BinTree a -> BinTree b
mapBin f Empty = Empty
mapBin f (Bin x t1 t2) = Bin (f x) (mapBin f t1) (mapBin f t2)

mirrorBin :: BinTree a -> BinTree a
mirrorBin Empty = Empty
mirrorBin (Bin x t1 t2) = Bin x (mirrorBin t2) (mirrorBin t1)

--Usando foldBin

foldBin :: b -> (a -> b -> b -> b) -> BinTree a -> b
foldBin z f Empty = z
foldBin z f (Bin x t1 t2) = f x (foldBin z f t1) (foldBin z f t2)

nodesBin' :: BinTree a -> Int
nodesBin' = foldBin 0 (\x r1 r2 -> r1 + r2 + 1)

mapBin' :: (a -> b) -> BinTree a -> BinTree b
mapBin' f = foldBin Empty (\x -> Bin (f x))

mirrorBin' :: BinTree a -> BinTree a
mirrorBin' = foldBin Empty (\x -> flip(Bin x))

heightBin' :: BinTree a -> Int
heightBin' = foldBin 0 (\x r1 r2 -> 1 + (max r1 r2))

--Ej3

data GenTree a = Gen a [GenTree a] deriving Show

foldGen :: (a -> [b] -> b) -> GenTree a -> b
foldGen f (Gen x gs) = f x (map (foldGen f) gs)

foldGen' :: (a -> c -> b) -> ([b] -> c) -> GenTree a -> b
foldGen' f g (Gen x gs) = f x (g (map (foldGen' f g) gs))

--Ej4

mapGen :: (a -> b) -> GenTree a -> GenTree b
mapGen f = foldGen' (\x -> Gen (f x)) id

maximum' [] = 0
maximum' (x:xs) = maximum (x:xs)

heightGen :: GenTree a -> Int
heightGen = foldGen' (const (1+)) maximum'

mirrorGen :: GenTree a -> GenTree a
mirrorGen = foldGen' Gen reverse

--Ej5

data GenExp a = Leaf a | Un (GenExp a) | BinG (GenExp a) (GenExp a) deriving Show

foldGenExp :: (a -> b) -> (b -> b) -> (b -> b -> b) -> GenExp a -> b
foldGenExp f g h (Leaf x) = f x
foldGenExp f g h (Un ge) = g (foldGenExp f g h ge)
foldGenExp f g h (BinG ge1 ge2) = h (foldGenExp f g h ge1) (foldGenExp f g h ge2)


data NExp = Num Int | Sum NExp NExp | Sub NExp NExp | Neg NExp deriving Show

foldNExp :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> NExp -> b
foldNExp f g h l (Num x) = f x
foldNExp f g h l (Sum ne1 ne2) = g (foldNExp f g h l ne1) (foldNExp f g h l ne2)
foldNExp f g h l (Sub ne1 ne2) = h (foldNExp f g h l ne1) (foldNExp f g h l ne2)
foldNExp f g h l (Neg ne) = l (foldNExp f g h l ne)

-- Either a b = Left a | Right b no tiene estructura recursiva

data Nat = Zero | Succ Nat

foldNat :: b -> (b -> b) -> Nat -> b
foldNat z f Zero = z
foldNat z f (Succ n) = f (foldNat z f n)