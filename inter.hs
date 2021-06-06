


--data Expr = Identifier Ident | Number Num | Plus Expr Expr
--data Assign = Assignment Ident Expr
--type Ident = String
--type Num = Int


data Expr = Lit Int | Add Expr Expr | Sub Expr Expr 
  deriving Eq

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Sub a b) = (eval a) - (eval b)

instance Show Expr where
  show (Lit n) = show n
  show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"


-- parser: is a function taking a string as input
--         result is an element of type a and a remainder, or multiple such tuples. (result, remainder)

data Parser a = Prs (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Prs p) inp = p inp    --analog zu runState

completeParse :: Parser a -> String -> a
completeParse p inp
  | null results = error "Parse unsuccessful"
  | otherwise     = head results
  where results = [res | (res, "") <- parse p inp]


isDigit = (\x -> '0'<=x && x<='9')

-- fail trivially
failure :: Parser a
failure = Prs (\x -> [])

-- succeed trivially without progress
--return :: a -> Parser a
--return x = Prs (\inp -> [(x, inp)])

-- succeed trivially with progress
item :: Parser Char
item = Prs (\inp -> case inp of
                       "" -> []
                       (x:xs) -> [(x, xs)])

-- parse single character with property p
sat :: (Char -> Bool) -> Parser Char
--sat p = Prs (\inp -> case inp of
--                       "" -> []
--                       (x:xs) -> if p x then [(x,xs)] else [])

-- gluing parsers together

-- mutual selection: apply both parsers
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = Prs (\s -> parse p s ++ parse q s)

-- alternative selection: if first fails, apply second
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Prs (\s -> case parse p s of 
                         [] -> parse q s
                         res -> res)

-- sequencing: first p, then q to results, but problem: result of first parser is lost (t)
(>>) :: Parser a -> Parser b -> Parser b
p >> q = Prs (\s -> [(u, s2) | (t, s1) <- parse p s, (u, s2) <- parse q s1])

-- solution: use as second argument a parser generator, which takes as input the result of the first parser

--(>>=) :: Parser a -> (a -> Parser b) -> Parser b
--p >>= g = Prs (\s -> [(u, s2) | (t, s1) <- parse p s, (u, s2) <- parse (g t) s1])



instance Monad Parser where
  return x = Prs (\inp -> [(x, inp)])
  p >>= g = Prs (\s -> [(u, s2) | (t, s1) <- parse p s, (u, s2) <- parse (g t) s1])

instance Functor Parser where
  fmap f v = v >>= (return . f)

instance Applicative Parser where
  pure x = return x
  u <*> v = u >>= \f -> v >>= \x -> return (f x)

-- hence, p >> q = p >>= \_ -> q

sat p = item >>= \x -> if p x then return x else failure

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string "" = return ""
string (x:xs) = char x Main.>> string xs Main.>> return (x:xs)
-- check char for each element, break correctly if ending on "" (failure otherwise) and return everything at the end

-- repetition: 0 or more repetitions of p
many :: Parser a -> Parser [a] 
many p = many1 p ||| return []

--many1 :: Parser a -> Parser [a] --one or more repetitions of p
--many1 p = p >>= \t -> many p >>= \ts -> return (t:ts)


many1 p = do t <- p
             ts <- many p
             return (t:ts)


numPos :: Parser Int
numPos = do ts <- many1 (sat isDigit)
            return (read ts)

numNeg :: Parser Int
numNeg = do char '-'
            x <- numPos
            return (-x)

num :: Parser Int
num = do numPos ||| numNeg


--Concrete grammar:
--Atom ::= Int | '(' Expr ')'
--Expr ::= Atom | Atom '+' Expr | Atom '-' Expr

atom = lit ||| pexpr
expr = atom ||| add ||| sub

lit = do x <- num
         return (Lit x)

pexpr = do char '('
           x <- expr
           char ')'
           return x

add = do a <- atom
         char '+'
         b <- expr
         return (Add a b)

sub = do a <- atom
         char '-'
         b <- expr
         return (Sub a b)

















































