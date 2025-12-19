
length' :: [a] -> Int
length' = foldl ((succ .) . const) 0

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc x -> acc && p x) True

map' :: (a -> b) -> [a] -> [b]
map' f xs = go f xs []
    where
        go :: (a -> b) -> [a] -> [b] -> [b]
        go _ [] = id
        go f (x:xs) ys = go xs . (:) f x