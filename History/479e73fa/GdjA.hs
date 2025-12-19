import Data.Char
import Data.List
depunctuate :: [Char] -> [Char]
depunctuate = filter (\x -> x `notElem` ".,:")

makeString :: [Int] -> [Char]
makeString = foldr (\x acc -> chr x : acc) []

enpower' :: [Int] -> Int
enpower' xs = foldl1 (^) (reverse xs)

revAll :: [[a]] -> [a]
revAll = foldr (\x acc -> reverse x ++ acc) []

rev :: [a] -> [a]
rev = foldl' (\acc x -> x : acc) []


dezip :: [(a,b)] -> ([a], [b])
dezip = foldr (\(x,y) (xs, ys) -> ((x:xs), (y:ys))) ([], [])

allSame :: [Int] -> Bool
allSame xs = and (zipWith (==) xs (tail xs))

allSame' :: [Int] -> Bool
allSame' xxs@(x:_) = all (==x) xxs

approxE :: Int -> Double
approxE n  = (sum . take n) (map (1/) (scanl (*) 1 [1..]))

squash :: (a -> a -> b) -> [a] -> [b]
squash f xs = zipWith (f) xs (tail xs)


all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

isElem :: Eq a => a -> [a] -> Bool
isElem = any . (==)


(<.>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d 
(<.>) = (.) . (.)

all'' :: (a -> Bool) -> [a] -> Bool
all'' = ((.) . (.)) and map

(<..>) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(<..>) f g x y = (f <.> g)

pipeline :: [a -> a] -> [a] -> [a]
pipeline f xs = undefined