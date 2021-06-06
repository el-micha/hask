f :: String -> IO ()
f out = do
  inp1 <- getLine
  inp2 <- getLine
  if inp1==inp2
    then putStrLn out
    else return ()

map1 f (x:xs) = foldr (\x y -> f x :y ) [] (x:xs)
map2 f (x:xs) = foldr ((:).f) [] (x:xs)

-- A function that prepends a list to its argument
-- dl123 = \x -> [1,2,3] ++ x
type DList a = [a] -> [a]
empty :: DList a
empty = \x -> x

sngl :: a -> DList a
sngl x = \y -> [x] ++ y -- sngl x = \xs -> x:xs

app :: DList a -> DList a -> DList a
app ys zs = \xs -> ys (zs xs)

fromList :: [a] -> DList a
fromList ys = \xs -> ys ++ xs

toList :: DList a -> [a]
toList ys = ys []

-- length as foldr
length1 :: [a] -> Int
length1 xs = foldr (\x y -> y + 1) 0 xs

length2 xs = foldl (\x y -> x + 1) 0 xs