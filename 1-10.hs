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
flatten (Elem x) = x
