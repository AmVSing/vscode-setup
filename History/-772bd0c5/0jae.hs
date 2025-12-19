module Radix where

import Prelude hiding (and, or)

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

data Bit = Zero | One
         deriving (Eq, Show)

type RadixTree = Tree Bit

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative

bI, lI, nI :: Int
bI = 1
lI = 4
nI = 12


sizeIT :: IntTree -> Int
sizeIT Empty = bI
sizeIT (Leaf _) = lI
sizeIT (Node _ lt rt) = nI + sizeIT lt + sizeIT rt


lR = 1
nR = 8
sizeRT :: RadixTree -> Int
sizeRT (Leaf _) = lR
sizeRT (Node _ lt rt) = nR + sizeRT lt + sizeRT rt

--
-- NOTE: The above import Prelude hiding (and, or) 
-- will allow you to name these two functions without
-- a name clash
--
and :: Bit -> Bit -> Bit
and One One = One
and _ _     = Zero

or :: Bit -> Bit -> Bit
or One _ = One
or _ One = One
or _ _ = Zero

binary :: Int -> BitString
binary 0 = [0]
binary x = bin x []
  where
    bin :: Int -> [Int] -> [Int]
    bin 0 ds = ds 
    bin x ds = bin (x `div` 2) ((x `mod` 2):ds)


insert :: BitString -> RadixTree -> RadixTree
insert [] (Node _ lt rt) = Node One lt rt
insert [] (Leaf _) = Leaf One
insert (x:xs) (Node m lt rt) = case x of 
  0      -> Node m (insert xs lt) rt
  otherwise -> Node m lt (insert xs rt)
insert (x:xs) (Leaf m) = case x of 
  0      -> Node m (insert xs (Leaf Zero)) (Leaf Zero)
  otherwise -> Node m (Leaf Zero) (insert xs (Leaf Zero))

buildRadixTree :: [Int] -> RadixTree
buildRadixTree [] = Leaf Zero
buildRadixTree xs = insert (binary xs) Leaf Zero

member :: Int -> RadixTree -> Bool
member
  = undefined

union :: RadixTree -> RadixTree -> RadixTree
union
  = undefined

intersection :: RadixTree -> RadixTree -> RadixTree
intersection
  = undefined

-- CONCLUSION: The break-even point is xxx.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node Zero (Leaf One)
               (Node One (Leaf Zero)
                          (Node One (Node Zero (Leaf One)
                                                 (Leaf Zero))
                                     (Leaf One)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node Zero (Node Zero (Leaf One)
                            (Node One (Leaf Zero) (Leaf One)))
                (Leaf One)
