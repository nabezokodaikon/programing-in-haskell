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

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs)(y : ys) = (x, y) : zip xs ys 

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_ : xs) = drop (n - 1) xs

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                     smaller = [a | a <- xs, a <= x]
                     larger = [b | b <- xs, b > x]

even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs

init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs 

power :: Int -> Int -> Int
power 0 _ = 0
power _ 0 = 1
power n m = n * (n `power` (m - 1))

and' :: [Bool] -> Bool 
and' [] = True
and' (x : xs) = x && and xs

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (xs : xss) = xs ++ concat'' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

get :: [a] -> Int -> a
get (x : _) 0 = x 
get (_ : xs) n = get xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x : xs) = a == x || elem' a xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x : xs) (y : ys) | x <= y = x : (merge xs (y : ys))
                        | otherwise = y: (merge (x: xs) ys)

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where 
        half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msrot [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
    where
        (left, right) = halve xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

take' :: Integral b => b -> [a] -> [a]
take' 0 _ = []
take' n (x : xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (x : xs) = last' xs
