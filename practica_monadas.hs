--Ej1

class Package p where
  handle::(a->b)->p a->p b

instance Package Maybe where
  handle f Nothing = Nothing
  handle f (Just a) = Just(f a)

instance Package [] where
  handle = map

instance Package ((->)a) where
  handle f g = \x -> f(g x)

--Ej2

class Box m where
  link :: (a -> m b) -> m a -> m b
  pack :: a -> m a

instance Box Maybe where
  pack = Just
  link f Nothing = Nothing
  link f (Just x) = f x

