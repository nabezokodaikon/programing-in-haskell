module Chapter7.Chapter7 where

add :: Int -> (Int -> Int)
add = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f(f(x))

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' xs = foldr' (+) 0 xs

sum'' :: Num a => [a] -> a
sum'' = foldr' (+) 0

length' :: [a] -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse xs ++ [x]

snoc :: a -> [a] -> [a] 
snoc x xs = xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x : xs) = snoc x (reverse'' xs)

reverse''' :: [a] -> [a]
reverse''' = foldr snoc []

sum2 :: [Int] -> Int
sum2 = sum' 0
    where
        sum' v [] = v
        sum' v (x : xs) = sum'(v + x) xs

sumViaFoldl :: [Int] -> Int
sumViaFoldl = foldl (+) 0
