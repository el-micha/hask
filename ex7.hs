import Control.Monad.State.Lazy
import Data.List


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show


test = Node "a" (Node "b" (Node "c" Leaf Leaf) (Node "d" Leaf Leaf)) (Leaf)

-- 1 replacement UND ein state update

-- State s a = State s -> (a, s)
-- State Int (Tree a)
--       state result

step :: Tree a -> State Int (Tree Int)
step Leaf = return Leaf
step (Node x l r) = do newL <- step l
                       modify (+1)
                       newX <- get
                       newR <- step r
                       return (Node newX newL newR)


-- =============================================================

match :: String -> Bool
match = go [] 
  where go :: String -> String -> Bool
        go [] [] = True
        go (s:ss) [] = False
        go ss@(s:st) xs@(x:xt)
          | not (relevant x)  = go ss xt
          | opening x         = go (x:ss) xs
          | not (matches' x s) = False
          | otherwise         = go st xt
        

relevant :: Char -> Bool
relevant c = elem c "(){}"

matches' :: Char -> Char -> Bool
matches' '(' ')' = True
matches' '{' '}' = True
matches' _ _ = False

opening :: Char -> Bool
opening c = elem c "({"

-- ==================================================


risers :: Ord a => [a] -> [[a]]
risers xs = reverse $ map reverse $ filter (not . null) $ go [[]] xs
  where  go :: Ord a => [[a]] -> [a] -> [[a]]
         go acc [] = acc
         go (a:aa) (x:xs)
           | notLesser x a = go ((x:a):aa) xs
           | otherwise     = go ([x]:a:aa) xs
         notLesser :: Ord a => a -> [a] -> Bool
         notLesser a [] = True
         notLesser a (x:xs) = a >= x


type Poly a = [(Integer, [a])]

testp :: Poly String
testp = [(3, ["x", "y"]), (1, ["y", "y"])]

evalPoly :: (a -> Integer) -> Poly a -> Integer
evalPoly f p = sum $ map helper p
  where helper (i, xs) = product $ [i] ++ (map f xs)


mapping :: String -> Integer
mapping "x" = 2
mapping "y" = 3
mapping "z" = 5

data SymbExpr a = Var a | Lit Integer | Add (SymbExpr a) (SymbExpr a) | Mul (SymbExpr a) (SymbExpr a)

instance Show a => Show (SymbExpr a) where
  show (Var x) = show x
  show (Lit x) = show x
  show (Add x y) = "(" ++ (show x) ++ " + " ++ (show y) ++ ")"
  show (Mul x y) = "(" ++ (show x) ++ " * " ++ (show y) ++ ")"

foldSymbExpr :: (a -> b) -> (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> SymbExpr a -> b
foldSymbExpr fvar flit fadd fmul (Var x) = fvar x
foldSymbExpr fvar flit fadd fmul (Lit x) = flit x
foldSymbExpr fvar flit fadd fmul (Add x y) = fadd (foldSymbExpr fvar flit fadd fmul x) (foldSymbExpr fvar flit fadd fmul y)
foldSymbExpr fvar flit fadd fmul (Mul x y) = fmul (foldSymbExpr fvar flit fadd fmul x) (foldSymbExpr fvar flit fadd fmul y)

toPoly :: SymbExpr a -> Poly a
toPoly = foldSymbExpr fvar flit fadd fmul
  where flit x = [(x, [])]
        fvar x = [(1, [x])]
        fadd = (++)
        fmul xs ys = [(xi * yi, xv ++ yv) | (xi, xv) <- xs, (yi, yv) <- ys]

expr = Mul (Add (Var "y") (Var "x")) (Add (Var "x") (Lit 14))

-- ===================================================================
-- ex 7.5 regexp with parsing
-- ######################################################################
data Parser a = Prs (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Prs p) inp = p inp

completeParse :: Parser a -> String -> a
completeParse p inp
  | null results = error "Parse unsuccessful"
  | otherwise     = head results
  where results = [res | (res, "") <- parse p inp]

failure :: Parser a
failure = Prs (\x -> [])

return' :: a -> Parser a
return' x = Prs (\inp -> [(x, inp)])

item :: Parser Char
item = Prs (\inp -> case inp of
                       "" -> []
                       (x:xs) -> [(x, xs)])

(|||) :: Parser a -> Parser a -> Parser a
p ||| q = Prs (\s -> parse p s ++ parse q s)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Prs (\s -> case parse p s of 
                         [] -> parse q s
                         res -> res)

(>>) :: Parser a -> Parser b -> Parser b
p >> q = Prs (\s -> [(u, s2) | (t, s1) <- parse p s, (u, s2) <- parse q s1])

instance Monad Parser where
  return x = Prs (\inp -> [(x, inp)])
  p >>= g = Prs (\s -> [(u, s2) | (t, s1) <- parse p s, (u, s2) <- parse (g t) s1])

instance Functor Parser where
  fmap f v = v >>= (return . f)

instance Applicative Parser where
  pure x = return x
  u <*> v = u >>= \f -> v >>= \x -> return (f x)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x else failure

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string "" = return ""
string (x:xs) = char x Main.>> string xs Main.>> return (x:xs)

many :: Parser a -> Parser [a] 
many p = many1 p ||| return []

many1 :: Parser a -> Parser [a]
many1 p = do t <- p
             ts <- many p
             return (t:ts)
-- ################################################################################


data Regexp = Literal Char | Seq [Regexp] | Or Regexp Regexp | Iter Regexp
  deriving Show

--instance Show Regexp where 
--  show (Literal x) = [x]
--  show (Seq xs) = intersperse ',' $ concat (map show xs)
--  show (Or x y) = "(" ++ show x ++ "|" ++ show y ++ ")"
--  show (Iter x) = show x ++ "+"

str2regexp :: String -> Regexp
str2regexp = completeParse pregexp

valid :: Char -> Bool
valid c = notElem c "()+|"

pchar :: Parser Regexp
pchar = do x <- item
           if valid x then return (Literal x) else failure

patom :: Parser Regexp
patom = pchar ||| (do char '(' 
                      x <- pregexp
                      char ')'
                      return x)

piteration :: Parser Regexp
piteration = patom ||| (do x <- patom
                           char '+'
                           return (Iter x))

psequence :: Parser Regexp
psequence = do xs <- (many1 piteration)
               return (Seq xs)

pregexp :: Parser Regexp
pregexp = psequence ||| (do x <- psequence
                            char '|' 
                            y <- pregexp
                            return (Or x y))

-- ===
regexpParser :: String -> Parser Bool
regexpParser s = helper $ str2regexp s

re1 = str2regexp "a(b|c)d"
re2 = str2regexp "ab+|d(e|f)+gh"

data Regexp' = Literal' Char | Seq' [Regexp'] | Or' Regexp' Regexp' | Iter' Regexp'

helper :: Regexp -> Parser Bool
helper (Literal x) = do c <- item
                        return (c == x)
helper (Or x y) = do xx <- helper x
                     yy <- helper y
                     return (xx || yy)
helper (Seq xs) = do bools <- mapM helper xs
                     return (all id bools)
helper (Iter x) = do bools <- many1 (helper x)
                     return (all id bools)

--completeParse :: Parser a -> String -> a
--completeParse p inp
--  | null results = error "Parse unsuccessful"
--  | otherwise     = head results
--  where results = [res | (res, "") <- parse p inp]

completeParse' :: Parser Bool -> String -> Bool
completeParse' p inp
  | null results = False
  | otherwise     = any id results
  where results = [res | (res, "") <- parse p inp]

matches :: String -> String -> Bool
matches rex s = completeParse' (regexpParser rex) s










































