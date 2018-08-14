module Chapter7.Chapter7 where

import Data.Char

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

sumsqreven2 :: [Int] -> Int
sumsqreven2 = sum . map (^2) . filter even
-- sumsqreven ns = sum (map (^2) (filter even ns))

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
    where weights = iterate(*2) 1

bin2int2 :: [Bit] -> Int
bin2int2 = foldr(\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode
