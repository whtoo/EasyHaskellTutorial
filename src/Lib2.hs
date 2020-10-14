{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib2 
    (addX,getSum,sum1,sum2) where

addX :: Num a => a -> a
addX x = x + 1

data Position  = Position { line :: !Int, columns :: !Int }

instance Semigroup Position where
    (<>) (Position a b) (Position 0 d) = Position a (b + d)
    (<>) (Position a _) (Position c d) = Position (a+c) d

instance Monoid Position where
    mempty = Position 0 0 

newtype Delta = Delta Int deriving (Eq, Show,Num)

instance Semigroup Delta where
    (<>) = (+)
instance Monoid Delta where
    mempty  = 0

newtype Sum a = Sum { getSum :: a } deriving (Eq,Show)

instance Functor Sum where
    fmap f (Sum a) = Sum (f a)

instance Applicative Sum where
    pure = Sum
    (<*>) (Sum f) (Sum a) = Sum (f a)
    
sum1 :: Sum Integer
sum1 = Sum 1

sum2 :: Sum Integer
sum2 = Sum 2


