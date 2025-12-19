module Calculus (vars, inTermsOf, eval, pretty, diff, maclaurin, pi) where

import Prelude hiding (pi)

import Expr

import Data.Map (Map, (!))
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set

type Env = Map String Double

---------------------------------------------------------------------------
-- Type classes and class instances

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger = Val . fromInteger
  negate      :: Expr -> Expr
  negate (Val 0.0) = Val 0.0
  negate e = Pre Neg e

  (+), (*)    :: Expr -> Expr -> Expr
  (Val 0.0) + e = e
  e + (Val 0.0) = e
  (+) e1 e2 = Bin Add e1 e2

  (Val 0.0) * _ = Val 0.0
  _ * (Val 0.0) = Val 0.0
  (Val 1.0) * e = e
  e * (Val 1.0) = e
  (*) e1 e2 = Bin Mul e1 e2



instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational = Val . fromRational
  (/)          :: Expr -> Expr -> Expr
  (Val 0.0) / _ = Val 0.0
  e / (Val 1.0) = e
  (/) e1 e2 = Bin Div e1 e2

instance Floating Expr where
  sin, cos, log, exp :: Expr -> Expr
  sin = Pre Sin 
  cos = Pre Cos
  log = Pre Log
  exp = Pre Exp

---------------------------------------------------------------------------
preOpM :: Map PreOp (Double -> Double)
preOpM = Map.fromList [
  (Neg, negate),
  (Sin, sin),
  (Cos, cos),
  (Log, log),
  (Exp, exp)]

binOpM :: Map BinOp (Double -> Double -> Double)
binOpM = Map.fromList [
  (Add, (+)),
  (Mul, (*)),
  (Div, (/))]

prettyPreM :: Map PreOp String
prettyPreM = Map.fromList [
  (Neg, "(-"),
  (Sin, "sin("),
  (Cos, "cos("),
  (Log, "log("),
  (Exp, "exp(")]


prettyBinM :: Map BinOp String
prettyBinM = Map.fromList [
  (Add, "+"),
  (Mul, "*"),
  (Div, "/")]


{-|
Computes the sum of first `n` terms of the given series.
-}
sumTo :: Num a => Int -> [a] -> a
sumTo n= sum . take n

{-|
Computes the set of variable names that appear within the given expression.
-}
vars :: Expr -> Set String
vars (Val _) = Set.empty
vars (Id x) = Set.singleton x
vars (Bin _ e1 e2) = Set.union (vars e1) (vars e2)
vars (Pre _ e1) = vars e1

{-|
Does the given expression only use the given variable and no others?
-}
inTermsOf :: String -> Expr -> Bool
inTermsOf x expr = (vars expr) `Set.isSubsetOf` Set.singleton x 

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Env -> Expr -> Double
eval _ (Val x) = x
eval env (Id s) = env Map.! s
eval env (Bin op e1 e2) = (binOpM Map.! op) (eval env e1) (eval env e2)
eval env (Pre op ex) = (preOpM Map.! op) (eval env ex)
{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
pretty :: Expr -> String
pretty = show

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: String -> Expr -> Expr
diff _ (Val _) = Val 0
diff x (Id s)
  | s == x = Val 1
  | otherwise = Val 0
diff x (Bin Add e1 e2) = diff x e1 + diff x e2
diff x (Bin Mul e1 e2) = e1 * diff x e2 + diff x e1 * e2
diff x (Bin Div e1 e2) = (diff x e1 * e2 - e1 * diff x e2) / (e2 * e2)
diff x (Pre Sin e) = cos e * diff x e
diff x (Pre Cos e) = negate (sin e * diff x e)
diff x (Pre Log e) = diff x e / e
diff x (Pre Exp e) = diff x e * exp e
diff x (Pre Neg e) = negate (diff x e)

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}
maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin e1 x n = sumTo n terms
  where
    factorials = scanl (*) 1 [1..]
    ds = iterate (diff "x") e1
    evald0 = map (eval (Map.singleton "x" 0)) ds
    terms = zipWith3 g [0..] factorials evald0
      where
        g :: Int -> Int -> Double -> Double
        g n k d = (x^n)*d/ fromIntegral k
    

-- Extension
instance Num a => Num [a] where
  fromInteger  :: Integer -> [a]
  fromInteger  = undefined
  negate       :: [a] -> [a]
  negate       = undefined
  (+), (*)     :: [a] -> [a] -> [a]
  (+)          = undefined
  (*)          = undefined

instance Fractional a => Fractional [a] where
  fromRational :: Rational -> [a]
  fromRational = undefined
  (/)          :: [a] -> [a] -> [a]
  (/)          = undefined

instance Floating a => Floating [a] where
  (**)         :: [a] -> [a] -> [a]
  (**)         = undefined

pi :: (Enum a, Fractional a) => Int -> a
pi = undefined
