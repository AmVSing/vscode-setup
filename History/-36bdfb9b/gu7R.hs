
length' :: [a] -> Int
length' = foldl ((succ .) const) 0

all' :: (a -> b) -> [a] -> Bool
all' = (and .) . map