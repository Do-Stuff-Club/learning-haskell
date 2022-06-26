module Golf where
    import Data.List
    -- Gets list of indices for a list going from 0 to the length-1
    -- of the list, inclusive.
    indices :: [a] -> [Int]
    indices list = [1..(length list)]

    -- Creates a tuple list of the inputted list with each element's index.
    withIndices :: [a] -> [(a, Int)]
    withIndices list = zip list (indices list)

    withoutIndices :: [(a, Int)] -> [a]
    withoutIndices list = map fst list

    -- Determines if the entry in an indexed list is the nth entry.
    isNth :: Int -> (a, Int) -> Bool
    isNth n (_, index) = index `mod` n == 0

    -- In an indexed list, get every nth element and return just the nth elements.
    nthElements :: [a] -> Int -> [a]
    nthElements list n = withoutIndices (filter (isNth n) (withIndices list))

    skips :: [a] -> [[a]]
    skips list = map (nthElements list) (indices list)

    localMaxima :: [Integer] -> [Integer]
    localMaxima (x1:(x2:(x3:xs))) = case x2 > x1 && x2 > x3 of -- FIXME use guards
        True -> x2:(localMaxima (x2:(x3:xs)))
        False -> (localMaxima (x2:(x3:xs)))
    localMaxima _ = []

    emptyCounts :: [Integer]
    emptyCounts = [0,0,0,0,0,0,0,0,0,0]

    incrementCount :: [Integer] -> Integer -> [Integer]
    incrementCount (x:xs) 0 = (x+1):xs
    incrementCount (x:xs) n = x:(incrementCount xs (n-1))
    incrementCount [] _ = []

    getCounts :: [Integer] -> [Integer]
    getCounts list = foldl incrementCount emptyCounts list

    printChar :: Integer -> Integer -> Char
    printChar a b = if a <= b then '*' else ' '

    printCountRow :: [Integer] -> Integer -> String 
    printCountRow counts n = map (printChar n) counts

    headerRows :: [String]
    headerRows = ["0123456789","=========="]


    printCounts :: [Integer] -> [String]
    printCounts counts = reverse (headerRows ++ (map (printCountRow counts) [1..(maximum counts)]))

    histogram :: [Integer] -> String
    histogram list = intercalate "\n" (printCounts (getCounts list))
