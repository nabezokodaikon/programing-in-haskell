module Chapter6.Chapter6 where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

multiple :: Int -> Int -> Int
multiple n 0 = 0
multiple n m = n + (n `multiple` (m - 1))
