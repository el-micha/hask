

--data Maybe a = Nothing | Just a

pack :: a -> Maybe a
pack x = Just x

divby2 :: Int -> Maybe Int
divby2 n = if even n then Just (n `div` 2) else Nothing

minus7 :: Int -> Maybe Int
minus7 n = if n>7 then Just (n-7) else Nothing


bothfuncs n = let res1 = divby2 n in
           case res1 of Nothing -> Nothing
                        (Just res1content) -> minus7 n

allthree n = let res1 = bothfuncs n in
           case res1 of Nothing -> Nothing
                        (Just res1content) -> minus7 n



andThen f1 f2 n = let res1 = f1 n in
           case res1 of Nothing -> Nothing
                        (Just res1content) -> f2 n

--firststepres = f1 n
--andThen' firststepres f2

--andThen' :: Maybe a -> (a -> Maybe a) -> Maybe a
andThen' x f = case x of Nothing -> Nothing
                         (Just y) -> f y

divide x = (pack x) `andThen'` divby2 `andThen'` divby2 `andThen'` allthree



packl x = [x]

andThenl xs f = [z |x <- xs, z <- f x]




data M a = M Int

x1 = M 3 :: M () 
x2 = M 3 :: M (M ()) 
x3 = M 3 :: M (M (M ())) 
x4 = M 3 :: M (M (M (M ()))) 

f1 :: M () -> Int
f1 x = 1

-- > f1 x1      ~> 1
-- > f1 x2      ~> type error (geht nur für spezifische Ms)

f2 :: M a -> M a -> Int
f2 x y = 4

-- > f2 x3 x3     ~> 4
-- > f2 x2 x1     ~> type error (geht nur für gleiche Ms)

f3 :: M a -> M b -> Int
f3 x y = 5

-- > f3 x1 x2     ~> 5 (geht für alle Ms)


newtype Volume = Volume Float
newtype Area = Area Float

data FThis = V Volume | A Area

f (V (Volume x)) = V $ Volume (x+2)
f (A (Area x)) = A $ Area (x-2)


Type Algebra: 

Product Type ~ Tuple:
a * b = (a, b) 

Sum Type ~ Either
a + b = Either a b = Left a | Right b

Neutrale Elemente:
0 = Void   -- der Type OHNE Elemente
1 = ()     -- der Type mit nur einem Element, namens Unit

Beispiele:
1 + 1 = Either () () = Bool
1 + a = Either () a = Maybe a

Kann man Gleichungen aufstellen?
Das macht evlt noch Sinn:
l(a) = 1 + a * l(a)
l(a) - a * l(a) = 1
l(a) = 1 / (1 - a)
Das macht scheinbar keinen Sinn mehr, weil die Inversen nicht exisiteren. 
Aber:
1 / (1 - a) sieht aus wie eine geometrische Reihe, also:
l(a) = 1 + a + a*a + a*a*a + ...
Dh:
l(a) ist entweder () oder a oder (a,a) oder (a,a,a) oder (a,a,a,a)... 
also eine Aufzählung aller Listen...
l(a) = List a

Und in der ersten Gleichung steckt eigentlich auch schon die Definition von List
l(a)   = 1  +       a * l(a) 
List a = Nil | Cons a   (List a)   -- Cons ~ Tuple














