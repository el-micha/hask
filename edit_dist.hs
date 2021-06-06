-- edit distance

data Edit = Change Char | Copy | Delete | Insert Char
  deriving (Eq, Show)

transform :: String -> String -> [Edit]
transform [] [] = []
transform xs [] = map (\_ -> Delete) xs
transform [] ys = map Insert ys

transform (x:xs) (y:ys)
  | x == y = Copy : (transform xs ys)
  | otherwise = best [Change y : transform xs ys, Delete : transform xs (y:ys), Insert y : transform (x:xs) ys]

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost x' = x
  | otherwise = x'
  where x' = best xs

cost :: [Edit] -> Int
cost = length . filter (/=Copy)

dist x y = cost $ transform x y
-- ==================================================================

Tree a = Leaf | Node a (Tree a) (Tree a)

-- tree manipulations: 
-- change content of Node
-- change structure:
--   delete tree
--   insert tree
--   replace tree with other tree
--   
-- "node deletions, insertions, and replacements that are necessary to transform one tree into another."

-- data Edit = Change Char | Copy | Delete | Insert Char
--   deriving (Eq, Show)

data TEdit = Copy | Change a | Delete | Insert (Tree a) | Replace

-- but... replace = move? 






