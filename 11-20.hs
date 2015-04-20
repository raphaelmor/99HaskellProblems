import Data.List
-- Problem 11
data ListItem a = Single a | Multiple Int a
	deriving (Show)

encode :: Eq a => [a] -> [(Int,a)]
encode xs = [(length x, head x) | x <- group xs]

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where
		encodeHelper (1,x) = Single x
		encodeHelper (n,x) = Multiple n x

-- Problem 12	
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = [] 
decodeModified (Single x : xs) = x : (decodeModified xs)
decodeModified ((Multiple i x):xs) = (replicate i x) ++ (decodeModified xs) 

-- Problem 13
encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
	where
		helper x [] = [(1,x)]
		helper x (y@(a,b):ys)
			| x == b = (a+1,b):ys
			| otherwise = (1,x):y:ys

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map helper . encode'
	where
		helper (1,x) = Single x
		helper (n,x) = Multiple n x

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs count = xs >>= replicate count
