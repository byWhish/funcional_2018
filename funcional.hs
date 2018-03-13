--1)
id ::  a -> a
id x = x

--2)
const :: a -> b -> a
const x _ = x

--3)
fst :: (a,b) -> a
fst (x,y) = x

--4)
snd :: (a,b) -> b
snd (x,y) = Main.fst ( swap (x,y) )

--5)
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--6)
head :: [a] -> a
head (x:xs) = x

--7)
tail :: [a] -> [a]
tail (x:xs) = xs

--8)
sum, product :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + Main.sum xs

--9)
product [] = 1
product (x:xs) = x * Main.product xs

--10)
elem, noElem  :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) | x == y = True
              | otherwise = Main.elem x ys  

--11)
noElem x [] = True
noElem x (y:ys) | x == y = False
                | otherwise = noElem x ys

--12)
subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) (y:ys) = x == y && subset xs ys