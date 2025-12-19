
length' :: [a] -> Int
length' = foldl ((succ .) . const) 0

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (and . p) True