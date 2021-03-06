import Data.List

-- Problem 1
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = foldr1 (\x acc -> acc) 


-- Problem 2
myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs


-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) i 
  | i == 1 = x
  | otherwise = elementAt xs (i - 1)


-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myLength' :: [a] -> Int
myLength' = foldr (\x acc -> acc + 1) 0


-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs  ++ [x] 

myReverse' :: [a] -> [a]
myReverse' = foldr (\x acc -> acc ++ [x]) [] 


-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True 
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)


-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (\x acc -> flatten x ++ acc) [] xs

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List xs) = concatMap flatten xs 

-- Problem 8
compress :: Eq a => [a] -> [a]
compress = foldr removeDuplicates [] 
  where
    removeDuplicates x [] = [x]
    removeDuplicates x acc
      | x == head acc = acc
      | otherwise = x : acc 

compress' :: Eq a => [a] -> [a]
compress' = map head . group

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack = group

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' list@(x:_) = let (group,rest) = span (==x) list
				in group : pack' rest 
-- Problem 10
encode :: Eq a => [a] -> [(Int,a)]
encode = map (\xs -> (length xs, head xs)) . pack

encode' :: Eq a => [a] -> [(Int,a)]
encode' xs = [(length x, head x) | x <- pack xs]
