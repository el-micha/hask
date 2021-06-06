import Control.Monad.Identity
import Control.Monad.Error


-- evaluate arithmetic with logging


data Expr = Lit Int | Plus Expr Expr | Minus Expr Expr 
  deriving Show

eval0 :: Expr -> Int

eval0 (Lit x) = x
eval0 (Plus x y) = eval0 x + eval0 y
eval0 (Minus x y) = eval0 x - eval0 y

eval1 :: Expr -> Identity Int
eval1 (Lit x) = return x
eval1 (Plus x y) = do x1 <- eval1 x
                      x2 <- eval1 y
                      return (x1 + x2)
eval1 (Minus x y) = do x1 <- eval1 x
                       x2 <- eval1 y
                       return (x1 - x2)

t1 = Minus (Lit 8) (Lit 4)
t2 = Minus (Lit 4) (Lit 8)
-- runIdentity $ eval1 t

eval2 :: Expr -> ErrorT String Identity Int
eval2 (Lit x) = return x
eval2 (Plus x y) = do x1 <- eval2 x
                      x2 <- eval2 y
                      return (x1 + x2)
eval2 (Minus x y) = do x1 <- eval2 x
                       x2 <- eval2 y
                       case (x2 > x1) of 
                         True -> throwError "no"
                         False -> return (x1 - x2)

-- runIdentity $ runErrorT $ eval2 t2










