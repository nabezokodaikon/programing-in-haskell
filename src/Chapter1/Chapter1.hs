module Chapter1.Chapter1 where

double :: Num a => a -> a
double x = x + x

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x]

product' :: Num a => [a] -> a
product' [] = 1
product' (x : xs) = x * product' xs

qsort' :: Ord a => [a] -> [a]
qsort' [] = []
qsort' (x : xs) = qsort' larger ++ [x] ++ qsort' smaller
    where smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x]

qsort'' :: Ord a => [a] -> [a]
qsort'' [] = []
qsort'' (x : xs) = qsort'' smaller ++ [x] ++ qsort'' larger
    where smaller = [a | a <- xs, a < x]
          larger = [b | b <- xs, b > x]
