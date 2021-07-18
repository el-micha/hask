class Idable a where 
  getId :: a -> Id a

data Id a = Id Int 
  deriving Show

data User = User String (Id User)
  deriving Show

data Comment = Comment String (Id Comment)
  deriving Show

instance Idable User where
  getId (User _ id) = id

instance Idable Comment where
  getId (Comment _ id) = id

data Either2 a b = Left2 a | Right2 b

type Meither b = Either2 Int




data D1024 = D1024

data D512 = D512

data Tensor a b 



type A = Int
type B = Int

a == b

newtype C = Int


data TensorData = TensorData [Int] [Int]

data TensorData = TensorData [Int] [Stride]



-- Tensor with shape (H, W, 3)
newtype Image = Image TensorData

mkImage :: TensorData -> Maybe Image
mkImage = ... 

extractPatches :: Image -> [Image]




f :: Int -> Int
f = (+1)

f (1::A)

newtype Volume = Volume Int

square :: Int -> Int

sq' :: Volume -> Volume


M a
M a b c d

M (M (M (M a)))


data M a = M Int

x1 = M 3 :: M () Int
x2 = M 3 :: M (M ()) Int
x3 = M 3 :: M (M (M ())) Int
x4 = M 3 :: M (M (M (M ()))) Int


