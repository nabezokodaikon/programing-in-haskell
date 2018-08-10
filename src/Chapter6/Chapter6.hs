module Chapter6.Chapter6 where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

multiple :: Int -> Int -> Int
multiple n 0 = 0
multiple n m = n + (n `multiple` (m - 1))

product' :: Num a => [a] -> a
product' [] = 1
product' (n : ns) = n * product' ns

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x : xs) ys = x : (xs `concat'` ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) | x <= y = x : y : ys
                  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)
