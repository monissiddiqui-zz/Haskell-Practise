-- My own implementation using case statements
-- maximum' :: (Ord a) => [a] -> a
-- maximum' xs = case xs of [] -> error "Nothing in this list"
--                             (x:[]) -> x
--                             (x:y:ys) -> if x > y then maximum' (x:ys) else maximum' (y:ys)

-- Book implementation
-- maximum' :: (Ord a) => [a] -> a  
-- maximum' [] = error "maximum of empty list"  
-- maximum' [x] = x  
-- maximum' (x:xs)   
--     | x > maxTail = x  
--     | otherwise = maxTail  
--     where maxTail = maximum' xs  

-- a more elegant implementation using the opposite of the word here we 

maximum' :: (Ord a ) => [a] -> a 
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- my own implementation
-- replicate' :: (Int a) => a -> b -> [b]
-- replicate' 0 x = []
-- replicate' n x = x:(replicate' (n-1) x)

-- Book's implementation
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' xs = case xs of [] -> [] 
                         (x:y) -> reverse' y ++ [x]

repeat' :: a -> [a]
repeat' x = repeat x ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) 
    | a == x = True
    | otherwise = elem' a xs
-- elem' a xs = case a xs of (a, a:ys) -> True
--                           (a, x:ys) -> return elem' a ys 

quicksort :: (Ord a ) => [a] -> [a]
quicksort [] = [] 
quicksort (x:xs) = 
    let smallerthan =  quicksort [y | y <- xs, y <= x]
        largerthan = quicksort [y | y <- xs, y >x]
    in smallerthan ++ [x] ++ largerthan