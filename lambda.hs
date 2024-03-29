
-- lambda parser

import Parsers
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Identity
--import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe

isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isDigit c = c >= '0' && c <= '9'

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

data Term = Id String | Ap Term Term | Lam String Term -- | Lit Int | Plus Term Term
--  deriving Show

atom = ident ||| lamb ||| paren -- ||| lit ||| plus

ident = do s <- identifier
           return (Id s)

--lit = do n <- many1 $ sat isDigit
--         return $ Lit (read n)

--plus = do char '('
--          t1 <- term
--          char '+'
--          t2 <- term
--          char ')'
--          return (Plus t1 t2)

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
--pretty (Lit n) = show n
pretty (Ap t1 t2) = "(" ++ (pretty t1) ++ " " ++ (pretty t2) ++ ")"
pretty (Lam s t) = "(%" ++ s ++ ". " ++ (pretty t) ++ ")"
--pretty (Plus t1 t2) = "(" ++ (pretty t1) ++ "+" ++ (pretty t2) ++ ")"

instance Show Term where
  show = pretty

t1 = str2term "%z.(%x y.(y x z))"
t2 = str2term "%z.(%x y.(y x z)t)"
t3 = str2term "(%x y.x)a b"
t4 = str2term "(%x.(%y.y)x)"

-- pretty $ str2term "(%x y.(y+(x+2))+3)"
-- =================================================================

type VSet = Set.Set String

free :: Term -> VSet
free (Id s) = Set.singleton s
free (Ap t1 t2) = Set.union (free t1) (free t2)
free (Lam s t) = Set.difference (free t) (Set.singleton s)

-- data Term = Id String | Ap Term Term | Lam String Term

-- subst t v s = t[v <- s]
subst :: Term -> String -> Term -> Term
subst (Id x) v s = if x == v then s else (Id x)
subst (Ap t1 t2) v s = Ap (subst t1 v s) (subst t2 v s)
subst (Lam x t) v s = if x == v then (Lam x t) else
  case Set.member x (free s) of 
    False -> Lam x (subst t v s)
    True  -> Lam z (subst (subst t x (Id z)) v s)
      where z = fresh $ Set.union (free s) (free t)
            fresh m = (foldr max "" m) ++ "'"

beta :: Term -> Term -> Term
beta (Lam x m) n = subst m x n

eager :: Term -> Term
eager (Id x) = Id x
eager (Lam x t) = Lam x (eager t)
eager (Ap t1 t2) = case eager t1 of
  (Lam x t) -> eager $ beta (Lam x t) (eager t2)
  otherwise -> Ap (eager t1) (eager t2) -- or throw error here

lazy :: Term -> Term
lazy (Id x) = Id x
lazy (Lam x t) = Lam x t
lazy (Ap t1 t2) = case lazy t1 of
  (Lam x t) -> lazy $ beta (Lam x t) t2
  otherwise -> Ap (lazy t1) t2


-- next: 
-- - add error handling, ErrorT
-- - logging step by step, WriterT
-- - introduce literal values and some operations, ReaderT
-- - rep loop, IO
-- - perhaps find duplicate subtrees
-- - eliminate unused subtrees (not necessary for lazy) 
-- - type inference

lazyM :: Term -> WriterT [String] Identity Term
lazyM (Id x) = do tell [x ++ " evaluates to itself"]
                  return $ Id x
lazyM (Lam x t) = do tell [show (Lam x t) ++ " not evaluated"]
                     return $ Lam x t
lazyM (Ap t1 t2) = do tell ["evaluating first term in " ++ show (Ap t1 t2)]
                      r <- lazyM t1
                      tell ["first term " ++ show t1  ++ " evaluated to " ++ show r]
                      case r of 
                        (Lam x t) -> lazyM $ beta (Lam x t) t2
                        otherwise -> return $ Ap r t2


evalM term = runIdentity $ runWriterT $  lazyM term

--disp :: (Term, [String]) -> IO ()
disp (t, out) = forM_ out print

lazyM2 :: Term -> ReaderT String (WriterT [String] Identity) Term
lazyM2 (Id x) = do ind <- ask
                   tell [ind ++ x ++ " evaluates to itself"]
                   return $ Id x
lazyM2 (Lam x t) = do ind <- ask
                      tell [ind ++ show (Lam x t) ++ " not evaluated"]
                      return $ Lam x t
lazyM2 (Ap t1 t2) = do ind <- ask
                       tell [ind ++ "evaluating first term in " ++ show (Ap t1 t2)]
                       r <- local (++"    ") (lazyM2 t1)
                       tell [ind ++ "first term " ++ show t1  ++ " evaluated to " ++ show r]
                       case r of 
                         (Lam x t) -> local (++"    ") (lazyM2 (beta (Lam x t) t2))
                         otherwise -> return $ Ap r t2

evalM2 term = runIdentity $ runWriterT $ runReaderT (lazyM2 term) "    "



