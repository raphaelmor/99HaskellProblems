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

