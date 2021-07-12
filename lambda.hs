
-- lambda parser

import Parsers

isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

spaces :: Parser String
spaces = many $ sat (==' ')

identifier' :: Parser Char
identifier' = do spaces
                 s <- item
                 spaces
                 return s

identifier :: Parser String
identifier = do spaces
                id <- many1 (sat isLetter)
                spaces
                return id

token :: String -> Parser String
token s = do spaces
             t <- string s
             spaces
             return t

-- attempt 1:

data Lam' = Var' Char | App' Lam' Lam' | Abs' [Char] Lam'
  deriving Show

pvar' = do s <- identifier'
           return (Var' s)

papp' = do char '('
           t1 <- plam'
           char ')'
           char '('
           t2 <- plam'
           char ')'
           return (App' t1 t2)

pabs' = do char '%'
           vars <- many identifier'
           char '.'
           t <- plam'
           return (Abs' vars t)

plam' = pvar' ||| papp' ||| pabs'

-- attempt 2 with given grammar: 

data Term = Id String | Ap Term Term | Lam String Term
  deriving Show

atom = ident ||| lamb ||| paren

ident = do s <- identifier
           return (Id s)

lamb = do char '%'
          ids <- many1 identifier
          char '.'
          t <- term
          return $ foldr Lam t ids
          

paren = do char '('
           t <- term
           char ')'
           return t

term = do t <- atom
          ts <- many atom
          return $ foldl Ap t ts
          
str2term = completeParse term

pretty :: Term -> String
pretty (Id s) = s
pretty (Ap t1 t2) = "(" ++ (pretty t1) ++ " " ++ (pretty t2) ++ ")"
pretty (Lam s t) = "(%" ++ s ++ ". " ++ (pretty t) ++ ")"


























