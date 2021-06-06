
-- trees 

data Tree t = Leaf | Node t (Tree t) (Tree t)
  deriving Show

insert :: Ord t => t -> Tree t -> Tree t
insert t Leaf = Node t Leaf Leaf
insert t (Node x left right)
  | t < x = Node x (insert t left) right
  | otherwise = Node x left (insert t right)

level :: Int -> Tree t -> [t]
level n Leaf = []
level 0 (Node t _ _) = [t]
level n (Node t left right) = (level (n-1) left) ++ (level (n-1) right)

build :: Ord t => [t] -> Tree t
build xs = foldl (flip insert) Leaf xs

t1 = build [1]
t2 = build [2,1,3]
t3 = build [8,3,5,1,2,6,9,3,4,5]
t4 = build [9,8,4,5,2,3,4,7,12]
t5 = build [2,6,9,3,4,56,87,1,2,8]
t6 = build [5,3,2,6,8]

t7 = build [7,10,3,2,4]
t8 = build [7,10,3,2,8]

t9 = build [2,6,9,3,4,56,87,1,2,8,7,3,2,5,7,8,5,3,2,4,5,7,8,6,54,3,2,3,54,6,87,8,54,3,2,4,5,7]


bft :: Tree t -> [t]
bft tree = helper 0 tree
  where helper n tree
          | null currlevel = currlevel
          | otherwise      = currlevel ++ (helper (n+1) tree)
            where currlevel = level n tree


sortedTree :: Ord t => Tree t -> Bool
--try to fail..

sortedTree Leaf = True
sortedTree (Node t left right) = (checklist [(<t)] left) && (checklist [(>t)] right)
  where checklist xs Leaf = True
        checklist xs (Node t left right)
          | not (all id (xs <*> [t])) = False
          | otherwise = (checklist ((<t):xs) left) && (checklist ((>t):xs) right)




