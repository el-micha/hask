-- tree tests


data Tree a = Leaf | Node a [Tree a]

structure :: Show a => Tree a -> String
structure Leaf = "(.)"
structure (Node a xs) = "(" ++ show a ++ (foldr ((++) . structure) "" xs) ++ ")"

t1 = Node 1 [ Node 2 [], Leaf, Node 4 [Node 4.1 [], Node 4.2 [Leaf]] , Node 5 [Leaf, Node 5.1 []] ]
