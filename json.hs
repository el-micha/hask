-- json parser
import System.IO

data JValue = JNull 
            | JBool Bool 
            | JString String 
            | JNumber Double 
            | JArray [JValue] 
            | JObject [(String, JValue)]
              deriving (Eq, Ord, Show)

--instance Show JValue where
--  show JNull = "null"
--  show (JBool b) = show b
--  show (JNumber n) = show n
--  show (JString s) = show s
--  show (JArray xs) = show xs
--  show (JObject (o:[])) = "{" ++ "\n" ++ (show o) ++ "\n" ++  "}"
--  show (JObject (o:os)) = "{" ++ "\n" ++ (show o) ++ "\n" ++ (show os) ++ "\n" ++ "}"

-- =============================================================

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

numFrac :: Parser Double
numFrac = do pre <- num
             char '.'
             post <- numPos
             return (read (show pre ++ "." ++ show post) :: Double)

jpnull = do res <- string "null"
            return JNull

jpbool = do b <- string "true" +++ string "false"
            if b == "true" then return (JBool True) else return (JBool False)

jpnumber = do res <- numFrac
              return (JNumber res)

jpstring = do res <- sanestring
              return (JString res)

sanestring = do char '"'
                symbols <- many allowed
                char '"'
                return symbols

allowed = sat (\x -> not (elem x ['\\', '"']))

jparray = do jpws
             char '['
             jpws
             maybeval <- sepBy (char ',') jpvalue +++ return []
             jpws
             char ']'
             jpws
             return (JArray maybeval)

jpobject = do jpws
              char '{'
              jpws
              kvpairs <- sepBy (char ',') kvpair +++ return []
              jpws
              char '}'
              jpws
              return (JObject kvpairs)

kvpair = do jpws
            key <- sanestring
            jpws
            char ':'
            jpws
            value <- jpvalue
            jpws
            return (key, value)


-- parser for separators, parser for elements
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep ele = do jpws
                   val <- ele
                   jpws
                   vals <- many (jpws Main.>> sep Main.>> jpws Main.>> ele)
                   jpws
                   return (val:vals)

ws = char ' ' +++ char '\t' +++ char '\n'
jpws = many ws

jpvalue = jpnull ||| jpbool ||| jpstring ||| jpnumber ||| jparray ||| jpobject




main = do
  contents <- readFile "testjson.json"
  let json = completeParse jpvalue contents
  putStr (show json)











