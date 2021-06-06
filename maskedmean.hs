import Data.Maybe

type Tensor = [Int]
reduce :: Tensor -> Int
reduce = sum
matmult :: Tensor -> Tensor -> Tensor
matmult = zipWith (*)
identityMat = repeat 1

maskedMean :: Maybe Tensor -> Tensor -> Int
maskedMean Nothing x = reduce x
maskedMean (Just mask) x = reduce $ matmult mask x

maskedMean' :: Maybe Tensor -> Tensor -> Int
maskedMean' = maybe reduce (\mask' -> \inp -> reduce $ matmult mask' inp)

maskedMean'' :: Maybe Tensor -> Tensor -> Int
maskedMean'' mask = reduce . matmult (fromMaybe identityMat mask)



