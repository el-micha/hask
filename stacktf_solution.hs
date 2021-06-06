
-- stack: push element, pop element, return size of stack

newtype Stack s = Stack {elements :: [s]} 

instance Show a => Show (Stack a) where
  show (Stack xs) = show xs

emptyStack = Stack []

-- functions on stack are StackTransformers:

data StackTransformer s a = ST ((Stack s) -> (a, Stack s))

runST :: StackTransformer s a -> Stack s -> (a, Stack s)
runST (ST f) x = f x

-- cheating: canonical implementations of superclass functions via monadic return and bind. neat. 
instance Functor (StackTransformer s) where
  fmap f v = v >>= (return . f)
instance Applicative (StackTransformer s) where
  pure x = return x
  u <*> v = u >>= \f -> v >>= \x -> return (f x)

-- here the magic happens:
instance Monad (StackTransformer s) where
  return x = ST (\s -> (x, s))
  p >>= q = ST (\s -> let (a1, s1) = runST p s
                      in runST (q a1) s1)
  

size :: StackTransformer s Int
size = ST (\stack -> (length $ elements stack, stack))

pop :: StackTransformer s s
pop = ST (\stack -> (head $ elements stack, Stack $ tail $ elements stack))

push :: s -> StackTransformer s ()
push = \x -> ST (\stack -> ((), Stack $ x:(elements stack)))

test = do push 3
          push 5
          five <- pop
          push 4
          push 5
          len <- size
          return (five == 5, len == 3)

-- runST test emptyStack ---> ((True,True),[5,4,3])
