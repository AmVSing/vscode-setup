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
-- f9 y zs = filter (> y) zs
f9 = filter . (flip (>))

f10 :: Ord a => a -> [a] -> Int
-- f10 x ys = length (filter (> x) ys)
f10 = length . filter . (>)

f11 :: (b -> c) -> (a -> b) -> a -> c
-- f11 f g x = f (g x)
f11 = (.)


f12 :: (e -> f) -> (c -> d -> e) -> (c -> d -> f)
-- f12 f g x y = f (g x y)
f12 = (.) . (.)

f13 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f13 f g x = f . (g x)
f13 = (.) . (.)