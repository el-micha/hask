
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


type Poly a = [(Integer,  [a])]

evalPoly :: (a -> Integer) -> Poly a -> Integer
evalPoly f p = sum $ map apply p
  where apply (i, xs) = product $ [i] ++ map f xs

testp = [(3, ["X", "Y"]), (1, ["Y", "Y"])]

mapping :: String -> Integer
mapping "X" = 2
mapping "Y" = 3
mapping "Z" = 5

-- rest: see laptop

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

data Regexp = Lit Char | Seq Regexp Regexp | Or Regexp Regexp | Iter Regexp
  deriving Show

str2regexp :: String -> Regexp
str2regexp = undefined














































