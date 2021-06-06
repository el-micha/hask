
-- more hs

-- parser generator: generates a parser for every rule... 
-- rule 

fix f = f (fix f)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

treemap' = \g f tree -> case tree of
  Leaf -> Leaf
  Node x l r -> Node (f x) (g f l) (g f r)

treemap = fix treemap'



