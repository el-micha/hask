-- json parser
import System.IO

data Parser a = Prs (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Prs p) inp = p inp

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
return' :: a -> Parser a
return' x = Prs (\inp -> [(x, inp)])

-- succeed trivially with progress
item :: Parser Char
item = Prs (\inp -> case inp of
                       "" -> []
                       (x:xs) -> [(x, xs)])

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


instance Monad Parser where
  return x = Prs (\inp -> [(x, inp)])
  p >>= g = Prs (\s -> [(u, s2) | (t, s1) <- parse p s, (u, s2) <- parse (g t) s1])

instance Functor Parser where
  fmap f v = v >>= (return . f)

instance Applicative Parser where
  pure x = return x
  u <*> v = u >>= \f -> v >>= \x -> return (f x)


-- parse single character with property p
sat :: (Char -> Bool) -> Parser Char
--sat p = Prs (\inp -> case inp of
--                       "" -> []
--                       (x:xs) -> if p x then [(x,xs)] else [])
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

many1 :: Parser a -> Parser [a] --one or more repetitions of p
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
num = numPos ||| numNeg

-- ===================================================

data Bul = Lit Char | And Bul Bul | Or Bul Bul | Not Bul 
  deriving Show

--(AND (NOT A) B) ;; A = T, B = T

plit :: Parser Bul
plit = do c <- item
          return (Lit c)

pand = do string "(AND "
          a <- pbul
          many1 (char ' ')
          b <- pbul
          char ')'
          return (And a b)

pnot' = do string "(NOT "
           a <- pbul
           char ')'
           return (Not a)

pnot = (string "(NOT ") >>= (\_ -> 
                   pbul >>= (\a -> 
               char ')' >>= (\_ -> 
                        return (Not a))))


pbul = plit ||| pand ||| pnot


eval :: Bul -> (Char -> Bool) -> Bool
eval (Lit c) ass = ass c
eval (And a b) ass = (&&) (eval a ass) (eval b ass)
eval (Not c) ass = not (eval c ass)

pande :: String -> (Char -> Bool) -> Bool
pande s = eval (completeParse pbul s)













