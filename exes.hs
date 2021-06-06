
-- hs exercise 5.5

data Tree3 a = Node (Tree3 a) (Tree3 a) (Tree3 a) | Leaf a
  deriving Show

-- foldr :: (a -> b -> b) -> b -> t a -> b

foldTree3 :: (b -> b -> b -> b) -> (a -> b) -> Tree3 a -> b
foldTree3 node leaf = go
  where 
    go (Leaf x) = leaf x
    go (Node l m r) = node (go l) (go m) (go r)

--instance Foldable Tree3 where
--  foldr f e (Leaf x) = f x e
--  foldr f e (Node left middle right) = foldr f (foldr f (foldr f e right) middle) left

instance Foldable Tree3 where
  foldr f e = foldTree3 node leaf
    where leaf = (\x -> f x e) 
          node x y z = \a -> f () x
--  foldr f e (Leaf x) = foldTree3 () () (Leaf x)
--  foldr (Node l m r) = foldTree3 () () (Leaf x)  
--node x y z = foldr (foldr (foldr (\a -> f a e) z) y) x

t1 = (Node (Node (Leaf 1) (Leaf 2) (Leaf 3)) (Leaf 17) (Node (Leaf 7) (Leaf 8 ) (Leaf 9)))

t2 = (Node (Node (Leaf 1) (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 5) (Leaf 6)) (Node (Leaf 7) (Leaf 8 ) (Leaf 9)))






