module Chapter4.Chapter4 where

halve :: [a] -> ([a], [a])
halve xs | length xs `mod` 2 == 0 = (take (f xs) xs, drop (f xs) xs)
         | otherwise = ([], [])
    where f xs = length xs `div` 2

safetail :: [a] -> [a]
safetail [] = []
safetail (_ : xs) = xs

safetail2 :: [a] -> [a]
safetail2 xs = if null xs
                  then []
                  else tail xs

safetail3 :: [a] -> [a]
safetail3 xs 
    | null xs = []
    | otherwise = tail xs

orElse :: Bool -> Bool -> Bool
orElse False False = False
orElse _ _ = True

orElse2 :: Bool -> Bool -> Bool
orElse2 x y = if x == False && y == False
                 then False
                 else True

orElse3 :: Bool -> Bool -> Bool
orElse3 x y | x == False && y == False = False
            | otherwise = True

andAlso :: Bool -> Bool -> Bool
andAlso x y = if x == True
                 then 
                    if y == True
                    then True
                    else False
                 else False

mult :: Int -> Int -> Int -> Int
mult a b c = f a b c
    where f = (\x -> \y -> \z -> x * y * z)
