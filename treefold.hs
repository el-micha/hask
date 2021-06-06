

data Tree a = Leaf | Node a (Tree a) (Tree a) (Tree a) 

instance Show a => Show (Tree a) where
  show t = go 0 t
    where go n Leaf = ws n ++ "Leaf\n"
          go n (Node a Leaf Leaf Leaf) = ws n ++ "Node " ++ show a ++ "\n"
          go n (Node a l m r) = ws n ++ "Node " ++ show a ++ "\n" ++ go (n+1) l ++ go (n+1) m ++ go (n+1) r
          ws n = foldl (++) "" ["  " | _ <- [1..n]]
  
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf Leaf
insert x (Node y l m r) 
  | x <  y = Node y (insert x l) m r
  | x == y = Node y l (insert x m) r
  | x >  y = Node y l m (insert x r)

buildTree :: Ord a => [a] -> Tree a
buildTree xs = foldl (flip insert) Leaf xs

t = buildTree [4,3,5,6,3,2]

treeFold :: (a -> b -> b -> b -> b) -> b -> Tree a -> b
treeFold node leaf Leaf = leaf
treeFold node leaf (Node x l m r) = node x (treeFold node leaf l) (treeFold node leaf m) (treeFold node leaf r)

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node a l m r) = (inorder l) ++ (inorder m) ++ (inorder r)

instance Foldable Tree where
  foldr f e Leaf = e
  foldr f e (Node x l m r) = f x (foldr f (foldr f (foldr f e r) m) l)




