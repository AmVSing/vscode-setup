
length' :: [a] -> Int
length' = foldl ((succ .) . const) 0

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc x -> acc && p x) True

map' :: (a -> b) -> [a] -> [b]
map' f = reverse . (foldl (\acc x -> f x : acc) [])

filter' :: forall a. (a -> Bool) -> [a] -> [a]
filter' p = reverse . foldl g []
    where
        g :: [a] -> a -> [a]
        g acc x = case p x of
            True -> x : acc
            otherwise -> acc

concat' :: [[a]] -> [a]
concat' = foldl (++) []

scanlSum :: [Int] -> [Int]
scanlSum = reverse . foldl (\acc x -> (x + h acc) : acc) []
    where
        h (x:_) = x
        h [] = 0

count :: forall a. Eq a => a -> [a] -> Int
count x = foldl g 0
    where
        g :: Int -> a -> Int
        g acc y = case y == x of
            True -> succ acc
            otherwise -> acc