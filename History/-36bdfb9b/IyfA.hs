
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
count x = foldl (\acc y -> acc + (if x == y then 1 else 0) ) 0

balanced :: String -> Bool --only for '(' and ')'
balanced xs = (foldl (g) 0 xs) == 0
    where
        g :: Int -> Char -> Int
        g c '(' = succ c
        g c ')' = c-1

foldr' :: forall a b. (a -> b -> b) -> b -> [a] -> b
foldr' cons nil xs = foldl f id xs nil
    where
        f :: (b -> b) -> a -> (b -> b)
        f acc x = acc . (cons x)