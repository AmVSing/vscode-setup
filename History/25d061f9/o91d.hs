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

f7 :: [Int] -> Int
f7 = sum . map (*2) 
