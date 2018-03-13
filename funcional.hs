test = 5

id ::  a -> a
id x = x

const :: a -> b -> a
const x y = x

fst :: (a,b) -> a
fst (x,y) = x

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

snd :: (a,b) -> b
snd (x,y) = Main.fst ( swap (x,y) )

head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

sum, product :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + Main.sum xs

product [] = 1
product (x:xs) = x * Main.product xs