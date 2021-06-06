-- intermediate problems from 
-- https://www.schoolofhaskell.com/user/DanBurton/20-intermediate-exercises


-- show Fluffy instances
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry f [] = []
  furry f (x:xs) = (f x):(furry f xs)

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f Nothing = Nothing
  furry f (Just x) = Just (f x)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f x = f . x

-- (a -> b) -> (t -> a) -> (t -> b)

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft (Left (f x)) 
  furry f (EitherLeft (Right x)) = EitherLeft (Right x)
-- (a -> b) -> EitherLeft t a -> EitherLeft t b

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Left x)) = EitherRight (Left x)
  furry f (EitherRight (Right x)) = EitherRight (Right (f x))
-- /show

-- ===============================================================

-- show Additional Misty functions
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

-- Exercise 6
-- Relative Difficulty: 3
-- (use banana and/or unicorn)
furry' :: (Misty m) => (a -> b) -> m a -> m b
furry' f = banana (unicorn . f)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
--apple ma mf = do a <- ma
--                 f <- mf
--                 return (f a)
--
--apple ma mf = ma >>= (\a -> mf >>= (\f -> return (f a) ) )
-- banana is bind but with flipped args
apple ma mf = banana (\a -> banana (\f -> unicorn (f a)) mf ) ma

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs f = 

[m b] -> m [b]

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"
-- /show

main = putStrLn "It typechecks!"

