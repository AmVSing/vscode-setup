
length' :: [a] -> Int
length' = foldl ((succ .) . const) 0

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc x -> acc && p x) True

map' :: (a -> b) -> [a] -> [b]
map' f = reverse . (foldl (\acc x -> f x : acc) [])
