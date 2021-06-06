-- exercises



data Prop a = Var a | Not (Prop a) | And (Prop a) (Prop a) | Or (Prop a) (Prop a)

foldProp :: (a -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Prop a -> b
foldProp fvar fnot fand for = go
  where go (Var a)   = fvar a
        go (Not p)   = fnot (go p)
        go (And p q) = fand (go p) (go q)
        go (Or p q)  = for (go p) (go q)

evalProp :: (a -> Bool) -> Prop a -> Bool
evalProp v = foldProp v (not) (&&) (||)

propVars :: Eq a => Prop a -> [a] 
propVars = foldProp (:[]) id (++) (++)

satProp :: Eq a => Prop a -> Bool
satProp p = any $ map evaluate allAssignments
  where evaluate assignment = evalProp assignment p
        vars = propVars p
        allAssignments = map makeAssignment (allTruthTables (length vars))
        makeAssignment truthTable = \v -> snd $ head $ filter ((v==) . fst) (zip vars truthTable)
        allTruthTables :: Int -> [[Bool]]
        allTruthTables 1 = [[True], [False]]
        allTruthTables n = map (True:) (allTruthTables (n-1)) ++ (map (False:) (allTruthTables (n-1)))









