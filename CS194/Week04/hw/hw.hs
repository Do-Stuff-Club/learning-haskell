fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . (takeWhile (/=1)) . (iterate (\i -> if even i then i `div` 2 else 3 * i + 1))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node h l b r) = case compare (height l) (height r) of
        GT -> Node ((max (height l) (height (insert a r))) + 1) l b (insert a r)
        _ -> Node ((max (height (insert a l)) (height r)) + 1) (insert a l) b r

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (\b acc -> if b then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> (f a):acc) []

-- FIXME double check this somehow
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> acc `f` x) base xs

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

formula :: Integer -> Integer -> Integer
formula i j = i + j + (2 * i * j)

filterCondition :: Integer -> (Integer,Integer) -> Bool
filterCondition n (i,j) = (1 <= i) && (i <= j) && (formula i j < n)

sievePairs :: Integer -> [(Integer, Integer)]
sievePairs n = filter (filterCondition n) (cartProd [1..n] [1..n])

mapPairs :: [(Integer, Integer)] -> [Integer]
mapPairs = map (uncurry formula)

sievedList :: Integer -> [Integer]
sievedList n = filter (\i -> notElem i (mapPairs (sievePairs n))) [1..n]


doublePlusOne :: [Integer] -> [Integer]
doublePlusOne = map (\i -> 2 * i + 1)

sieveSundaram :: Integer -> [Integer] 
sieveSundaram = doublePlusOne . sievedList