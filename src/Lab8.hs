--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 8: Foldables                                                           --
--------------------------------------------------------------------------------

module Lab8 where

import Control.Applicative
import Data.Foldable hiding (asum)

--------------------------------------------------------------------------------

data Expr a = Var a
            | Val Int
            | Add (Expr a) (Expr a)

e1 :: Expr String
e1 = Var "x"

e2 :: Expr (String, Int)
e2 = Var ("x",22)

e3 :: Expr a
e3 = Val 4

e4 :: Expr String
e4 = Add (Val 8) (Var "y")

e5 :: Expr Int
e5 = Add (Val 8) (Var 7)

e6 :: Expr (String, Int)
e6 = Add e2 (Var ("y",42))

e7 :: Expr (String, Int)
e7 = Add e6 e6

instance Foldable Expr where
    foldr f acc (Var x) = f x acc
    foldr f acc (Val x) = acc
    foldr f acc (Add x y) = foldr f (foldr f acc y) x

--------------------------------------------------------------------------------

data Zipper a = Zipper [a] a [a]
    deriving (Eq, Show)

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs

view :: Zipper a -> a
view (Zipper xs y zs) = y

left :: Zipper a -> Zipper a
left p@(Zipper xs y []) = p
left (Zipper xs y (z:zs)) = Zipper (y:xs) z zs

right :: Zipper a -> Zipper a
right p@(Zipper [] y zs) = p
right (Zipper (x:xs) y zs) = Zipper xs x (y:zs)

instance Foldable Zipper where
    foldr f acc (Zipper xs y zs) = foldr f (f y (foldr f acc zs)) xs

--------------------------------------------------------------------------------

-- | `filterF` is a generalisiation of `filter` which works on all
-- data structures which have an instance of `Foldable`. That is, `filterF`
-- @p xs@ reduces @xs@ to a list of elements which satisfy the predicate @p@.
filterF :: Foldable f => (a -> Bool) -> f a -> [a]
filterF f items = filter f $ toList items

-- | `asum` @xs@ combines all computations in @xs@ as alternatives in one big
-- computation of type @f a@.
asum :: (Alternative f, Foldable t) => t (f a) -> f a
asum xs = foldr (<|>) empty xs

--------------------------------------------------------------------------------
