-- Functions can be written in pointfree forms
-- This often involes using function composition to remove parameters
-- the principle is that
-- foo x = f . (g x)
-- foo = \x -> f . (g x)
-- since (.) p q = (\x -> p (q x))
-- foo = ((.)  f) . g
-- i.e. foo = (.) ((.) f) g
-- examples

f1 :: [Int] -> Int
-- f1 x = length (filter even x)
f1 = length . (filter even)

f8 :: Eq a => a -> [a] -> Bool
-- f8 y = any (== y)
f8 = any . (==)

f9 :: Ord a => a -> [a] -> [a]
-- f9 y = filter (> y)

