multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x*y*z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = ( `elem` ['A'..'Z'])

applyTwice :: (a -> a) ->a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> ( b -> a -> c )
flip' f x y = f y x  

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

chain :: (Integral a ) => a -> [a]
chain 1 = [1]
chain n  
    | even n  = n:chain (n`div`2)
    | odd n = n:chain (3*n +1)

-- numLongChains :: Int
-- numLongChains = length (filter isLong (map chain [1..100]))
--     where isLong x = length x > 15

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: Num a => [a] -> a
sum' = foldl (+) 0 

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldl (\acc y -> if x == y then True else acc) False

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\ x acc -> (f x):acc) []

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc b -> if acc >= b then acc else b)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr ( \x acc -> if p x then x:acc else acc ) []

head' :: [a] -> a
head' = foldl1 (\acc _ -> acc)

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
