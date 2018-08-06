module Chapter2.Chapter2 where

double x = x + x

quadruple x = double(double x)

factorial n = product[1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where a = 10
          xs = [1, 2, 3, 4, 5]

last1 :: [a] -> a
last1 [x] = x
last1 (x : xs) = last1 xs

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

last3 :: [a] -> a
last3 xs = head (reverse xs)

init1 :: [a] -> [a]
init1 [x] = []
init1 (x : xs) = [x] ++ init1 xs

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs

init3 :: [a] -> [a]
init3 xs = reverse (drop 1 (reverse xs))
