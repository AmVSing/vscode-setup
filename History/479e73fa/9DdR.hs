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
approxE n  = (sum . take n) (map recip (scanl (*) 1 [1..]))

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


zipWith''' :: forall a b c. (a -> b -> c) -> [a] -> ([b] -> [c])
zipWith''' f = foldr g (const [])
    where
        g :: a -> ([b] -> [c]) -> ([b] -> [c])
        g x acc ys = case ys of
            [] -> []
            (y:ys') -> f x y : acc ys'

-- foldr' :: (a -> b -> b) -> [a] -> (b -> b)
-- foldr' _ [] = id
-- foldr' f (x:xs) = foldr' g xs
--     where
--         g :: (a -> b -> b) 

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl _ v [] = v
-- foldl f v (x:xs) = foldl f (f v x) xs

foldla :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldla f = flip $ foldr (\x acc v -> acc (f v x)) id
-- foldlFlip _ [] = id
-- foldlFlip f (x:xs) = g x (foldlFlip f xs)
--     where
--         g :: a -> (b -> b) -> (b -> b)
--         g x acc = \v -> acc (f v x)

