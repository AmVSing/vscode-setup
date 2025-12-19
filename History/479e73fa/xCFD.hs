import Data.Char

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



pipeline :: [a -> a] -> [a] -> [a]
pipeline fs = map (foldr (\x acc -> x . acc) id fs) 


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _ _ = []

zipWith'' :: (a -> b -> c) -> [a] -> ([b] -> [c])
zipWith'' f [] = const []
zipWith'' f (x:xs) = \ys -> case ys of 
    [] -> []
    (y:ys') -> f x y : zipWith'' f xs ys'


zipWith''' :: (a -> b -> c) -> [a] -> ([b] -> [c])
zipWith''' f = foldr g (const [])
    where
        g x acc ys = case ys of
            [] -> []
            (y:ys') -> f x y : acc ys'

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f v' xs
    where
        !v' = f v x