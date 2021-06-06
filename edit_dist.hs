
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





