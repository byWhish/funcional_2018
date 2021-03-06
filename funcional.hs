--1)
id' ::  a -> a
id' x = x

--2)
const' :: a -> b -> a
const' x _ = x

--3)
fst' :: (a,b) -> a
fst' (x,_) = x

--4)
snd' :: (a,b) -> b
snd' (_,y) = y

--5)
swap' :: (a,b) -> (b,a)
swap' (x,y) = (y,x)

--6) no considero la lista vacia
head' :: [a] -> a
head' (x:xs) = x

--7) no considero la lista vacia
tail' :: [a] -> [a]
tail' (x:xs) = xs

--8)
sum', product' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

--09)
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = x == y || elem x ys   

noElem'  :: Eq a => a -> [a] -> Bool
noElem' x ys =  not ( elem' x ys )

--10)
and', or' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

or' [] = False
or' (x:xs) = x || or' xs

--11) no considero la lista vacia
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

--12)
init' :: [a] ->  [a]
init' [x] = []
init' (x:xs) = x : init' xs

--13)
subset' :: Eq a => [a] -> [a] -> Bool
subset' [] _ = True
subset' (x:xs) (ys) = elem' x ys && subset' xs ys

--14)
(+++) ::  [a] -> [a] -> [a]
(+++) [][] = []
(+++) (x:xs) y = x : (+++) xs y
(+++) x (y:ys) = y : (+++) x ys

--15)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x +++ concat' xs

--16) no considero la lista vacia
--lista indexada a partir de 0
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) i = (!!!) xs (i-1)

--17) considero que pido menor o igual cantidad de elementos de lista
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' i (x:xs) = x : take' (i-1) xs

--18) consideto que tiro igual o menor cantidad de elementos que la lista
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop' (n-1) xs

--19) asumo listas de igual cantidad de elementos
zip' :: [a] -> [b] -> [(a,b)]
zip' [][] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--20) asumo minimo 2 elementos
splitAt' :: Int -> [a] -> ([a],[a])
splitAt' 0 xs = ([],xs)
splitAt' _ [] = ([],[])
splitAt' n (x:xs) = ( x:ys, zs)
                where ( ys, zs ) = splitAt' (n-1) xs

--21) asumo minimo un elemento 
maximun', minimun' :: Ord a => [a] -> a
maximun' [x] = x 
maximun' (x:xs) = max x ( maximun' xs )

minimun' [x] = x 
minimun' (x:xs) = min x ( maximun' xs ) 

--22)
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' y [] = Nothing
lookup' y (x:xs) | y == fst' x = Just( snd' x )
                 | otherwise  = lookup' y xs
                
--23)       
unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])    

--24)
tails' :: [a] -> [[a]]
tails' [] = []
tails' (x:xs) = (x:xs) : ( tails' xs )

--25)
replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' i x = x : ( replicate' (i-1) x )

--26)
repeat' :: a -> [a]
repeat' x = x : repeat' x

--27)
cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = xs ++ cycle' xs 

--28)
nats' :: [Int]
nats' = [1..]

--29)
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar [x] = [[x]]
