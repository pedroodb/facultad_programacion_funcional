--Ej1

setUnion = (++)
--setUnion' = (or)

--Ej2

data TipTree a = Tip a | Join (TipTree a) (TipTree a) deriving Show

foldTip :: (a -> b) -> (b -> b -> b) -> TipTree a -> b

foldTip z f (Tip x) = z x
foldTip z f (Join t1 t2) = f (foldTip z f t1) (foldTip z f t2)

heightTip :: TipTree a -> Int
heightTip = foldTip (const 1) (\x -> \y -> (max x y) + 1)

leaves :: TipTree a -> Int
leaves = foldTip (const 1) (+)

nodes :: TipTree a -> Int
nodes = foldTip (const 0) ((+).(1+))

walkover :: TipTree a -> [a]
walkover = foldTip (:[]) (++)

mirrorTip :: TipTree a -> TipTree a
mirrorTip = foldTip Tip (flip Join)

mapTip :: (a -> b) -> TipTree a -> TipTree b
mapTip f = foldTip (Tip . f) Join

--Ej4

data Seq a = Nil | Unit a | Cat (Seq a) (Seq a) deriving Show

foldSeq :: b -> (a -> b) -> (b -> b -> b) -> Seq a -> b

foldSeq z f g Nil = z
foldSeq z f g (Unit x) = f x
foldSeq z f g (Cat s1 s2) = g (foldSeq z f g s1) (foldSeq z f g s2)

appSeq :: Seq a -> Seq a -> Seq a
appSeq = Cat

conSeq :: a -> Seq a -> Seq a
conSeq x s = Cat (Unit x) s

lenSeq :: Seq a -> Int
lenSeq = foldSeq 0 (const 1) (+)

headSeq :: Seq a -> a
headSeq = foldSeq undefined id const

-- A partir de aca necesito recursion primitiva, ver de implementar recr

eqSeq :: Eq a => Seq a -> Seq a -> Bool
eqSeq Nil Nil = True
eqSeq (Unit x1) (Unit x2) = (==) x1 x2
eqSeq (Cat s1 s2) (Cat s1' s2') = and [(eqSeq s1 s1'),(eqSeq s2 s2')]
eqSeq s1 s2 = False