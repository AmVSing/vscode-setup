
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