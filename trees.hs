import Data.Maybe
import Data.List

data Tree a = E | Node a (Tree a) (Tree a) deriving Show

singleton x = Node x E E

tinsert x E = singleton x
tinsert x (Node n left right)
  | x < n = Node n (tinsert x left) right
  | otherwise = Node n left (tinsert x right)

insertmany xs tree = foldl (\x y -> tinsert y x) tree xs
list2tree xs = insertmany xs E

treemin (Node n E _) = n
treemin (Node n left _) = treemin left

treemax (Node n _ E) = n
treemax (Node n _ right) = treemax right

treesum E = 0
treesum (Node n left right) = n + (treesum left) + (treesum right)

treepath:: Eq a => a -> Tree a -> Maybe [a]
treepath elem E = Nothing
treepath elem (Node n left right)
  | elem == n = Just [n]
  | isJust l = l >>= (\p -> Just (n:p))
  | isJust r = r >>= (\p -> Just (n:p))
  | otherwise = Nothing
  where l = treepath elem left
        r = treepath elem right

leaves :: Tree a -> [a]
leaves E = []
leaves (Node n E E) = [n]
leaves (Node n left right) = (leaves left) ++ (leaves right)

dfrep :: Tree a -> [a]
dfrep E = []
dfrep (Node n left right) = [n] ++ (dfrep left) ++ (dfrep right)

bfhelper :: [Tree a] -> [a] -> [a]
bfhelper [] res = res
bfhelper (E:ts) res = bfhelper ts res
bfhelper ((Node n left right):ts) res = bfhelper (ts ++ [left, right]) (res ++ [n])

bfrep :: Tree a -> [a]
bfrep E = []
bfrep tree = bfhelper [tree] []

-- take k from list with smallest lengths
dist :: Num a => (a, a) -> a
dist (x, y) = x*x + y*y
fab :: Ord a => Num a => [(a,a)] -> Int -> [(a,a)]
fab points k = take k $ sortOn dist points

----------------------------------------
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--foldr f z [] = z
--foldr f z (x:xs) = f x (foldr f z xs)

--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--foldl f z [] = z
--foldl f z (x:xs) = foldl f (f z x) xs

-- foldl with foldr

foldl2 g e (x:xs) = foldr f e (x:xs)
  where f = \a b -> (wrap a) b
        wrap x = \u -> g u x

foldl3 f v xs = foldr (\b g x -> ) id xs a


\c=id a=x -> \y -> g y x
\y -> g y x :: c -> b




--foldl2 g e (x:xs) = foldr f v (x:xs)
--  where f = \a b -> (wrap a) b
--        v = 0
--        wrap x = \u -> g u x


--apply f to x and v and store result in next v
--foldl2 g e x:xs = foldr (\x0 v0 -> g v0 x0) e x:xs 

--foldr f v x:xs = f x (foldr f v xs)

--foldl2 f e xs = foldr (\x y -> f y x) e (reverse xs)
--reverse2 xs = foldr (\e l -> l++[e]) [] xs



----------------------------------------

--append to first elem in res list or create new first elem in res list
helper c [] res = res
helper c _ [] = []
helper c (x:xs) (y:ys)
  | c == x    = helper c xs ([]:y:ys)
  | otherwise = helper c xs ((y++[x]):ys)
split :: Char -> [Char] -> [[Char]]
split c str = reverse $ helper c str [[]] 

-------------------------------------------------
filtermap p f = filter p . map f
filtermap2 p f = foldr aux e 
  where aux x y 
          | p (f x) = ((f x):y)
          | otherwise = y
        e = []
-------------------------------------------------
concatmap f = concat . map f 
concatmap2 f = foldr aux e 
  where aux x y = (f x) ++ y
        e = []

map2 f = foldr ((:).f) []

-------------------------------------------------
--- difference lists
-- type DList a = [a] -> [a]

empty = \x -> x
sgt e = \xs -> e:xs
app a b = \z -> (a.b) z
-- a = \x -> blah ++ x
-- b = \y -> blah ++ y

fromList ys = \xs -> ys ++ xs
toList xs = xs []

------------------------------------------------

























