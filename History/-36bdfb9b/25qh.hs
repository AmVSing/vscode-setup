
length' :: [a] -> Int
length' = foldl ((succ .) . const) 0

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\acc x -> acc && p x) True

map' :: (a -> b) -> [a] -> [b]
map' f = reverse . (foldl (\acc x -> f x : acc) [])

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldl g []
    where
        g acc x = case p x of
            True -> p x : acc
            otherwise -> acc