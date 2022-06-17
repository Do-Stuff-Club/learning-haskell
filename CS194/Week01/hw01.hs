charToString :: Char -> String
charToString c = [c]

digitCharToInt :: Char -> Integer
digitCharToInt c = read (charToString c)

toDigits :: Integer -> [Integer]
toDigits n = map digitCharToInt (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverse (toDigits i)

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:[]) = [x]
doubleEveryOtherHelper (x0:(x1:xs)) = (x0 : ((2 * x1) : (doubleEveryOtherHelper xs)))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherHelper (reverse xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + (sumDigits xs)

expandDigits :: [Integer] -> [Integer]
expandDigits [] = []
expandDigits (x:xs) = (toDigits x) ++ (expandDigits xs)

validate :: Integer -> Bool
validate i = (sumDigits (expandDigits (doubleEveryOther (toDigits i)))) `mod` 10  == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)