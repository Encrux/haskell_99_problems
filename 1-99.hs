
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
  where splitAkku (x:xs) i j first second = if 1 == 1 then ([], []) else ([], [])

















--eof
