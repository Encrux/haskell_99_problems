--Problem 1
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (x:xs) = myLast xs

--Problem 2
myButLast :: [a] -> a
myButLast [] = error "No end for empty lists!"
myButLast [x] = error "No seconds last element for list of size 1"
myButLast (x:xs) =
              if length xs == 1
              then x
              else myButLast xs

--Problem 3
elementAt :: [a] -> Int -> a
elementAt list i = list!!(i - 1)

--Problem 4
myLength :: [a] -> Int
myLength list = myLengthAkku list 0
  where myLengthAkku [] akku = akku
        myLengthAkku (x:xs) akku = myLengthAkku xs (akku + 1)

--Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl(\x xs -> xs : x)[]

--Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == (myReverse list)

--Problem 7
--check ich ned

--Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x: (compress $ dropWhile (==x) xs)

--Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (==x) xs) : pack (dropWhile(==x)xs)


--Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = map(\x -> ((length x), x!!0))(pack list)



--Problem 11

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map convertTuple . encode
  where
    convertTuple (1,b) = Single b
    convertTuple (a,b) = Multiple a b

--Problem 12
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodeListItem x) ++ (decodeModified xs)

decodeListItem :: ListItem a -> [a]
decodeListItem (Single a) = [a]
decodeListItem (Multiple i a) = replicate i a

--Problem 13
--kein bock

--Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--Problem 15
repli :: [a] -> Int -> [a]
repli [] i = []
repli (x:xs) i = (replicate i x) ++ (repli xs i)

--Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery list i = map (\x -> fst x) (filter (\(a,b) -> b `mod` i /= 0) (zip list [1..]))

--Problem 17
split :: [a] -> Int -> ([a], [a])
split (x:xs) i = splitAkku (x:xs) i 0 [] []
  where
    splitAkku [] i j first second = (first, second)
    splitAkku (x:xs) i j first second = if i > j
      then splitAkku xs i (j + 1) (first++[x]) second
      else splitAkku xs i (j + 1) first (second++[x])

--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice list start end = map (\(a,b) -> b) (filter (\(i, ele) -> i >= start && i <= end) (zip [1..] list))

--Problem 19
rotateLeft :: [a] -> Int -> [a]
rotateLeft [] i = []
rotateLeft (list) i
      | i > 0  = (drop i list) ++ (take i list)
      | i < 0  = (drop ((length list) + i) list) ++ (take ((length list) + i) list)
      | otherwise = list

--Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt i [] = error "list shouldn't be empty"
removeAt i list = (list!!(i-1), (take (i - 1) list) ++ drop i list)

--Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt ele list i = (take (i-1) list) ++ [ele] ++ (drop (i-1) list)

--Problem 22
range :: Int -> Int -> [Int]
range i j
  | i < j = [i] ++ (range (i+1) (j-1)) ++ [j]
  | i > j = [i] ++ (range (i-1) (j+1)) ++ [j]
  | i == j = [i]


-- Problem 23 bis Problem Problem 25
-- kein bock System.Random zu importieren

--Problem 26
combinations :: Int -> [a] -> [[a]]
combinations 0 list = [[]]
combinations i [] = []
-- skip for now

--Probkem 27


--Problem 31
isPrime :: Int -> Bool
isPrime n
  | n == 1 = False
  | (filter (\k -> n `mod` k == 0) [2..(n-1)]) == [] = True
  | otherwise = False

--Problem 32
myGCD :: Int -> Int -> Int
myGCD i j
  | i < 0 = myGCD (-i) j
  | j < 0 = myGCD i (-j)
  | j == 0 = i
  | i == 0 = j
  | i > j = myGCD (i - j) j
  | otherwise = myGCD i (j - i)

--Problem 33
coprime :: Int -> Int -> Bool
coprime i j = myGCD i j == 1

--Problem 34
totient :: Int -> Int
totient n = length $ filter (coprime n) [1..(n-1)]

--Problem 35
primesTo :: Int -> [Int]
primesTo m = sieve [2..m]
  where
    sieve [] = []
    sieve (x:xs) = x : sieve (filter (\i -> i `mod` x /= 0) xs)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = [firstPrimeDivisor n] ++ primeFactors (div n (firstPrimeDivisor n))
  where firstPrimeDivisor n = head (filter (\k -> n `mod` k == 0) (primesTo n))

--Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = zip (map head $ pack $ primeFactors n) (map length (pack $ primeFactors n))

--Problem 37
fastTotient :: Int -> Int
fastTotient n = product (map (\(p, m) -> (p - 1) * p ^ (m - 1)) (primeFactorsMult n))

--Problem 38
--no solution required

--Problem 39
primesR :: Int -> Int -> [Int]
primesR i j = dropWhile (< i) (primesTo j)

--Problem 40
goldbachList :: Int -> [(Int, Int)]
goldbachList n = [(x,y) | x <- primesTo n, let y = n - x, isPrime y]

goldbach :: Int -> (Int, Int)
goldbach n = head $ goldbachList n

--Problem 41
goldbachRange :: Int -> Int -> [(Int, Int)]
goldbachRange i j = map goldbach (filter even [i..j])

goldbachRange' :: Int -> Int -> Int -> [(Int, Int)]
goldbachRange' i j limit = filter (\(x,y) -> x >= limit && y >= limit) (goldbachRange i j)

















--eof
