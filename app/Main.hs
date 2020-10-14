module Main where

import Lib ( Parser(runParser), sci)
import Lib2 ( sum1,sum2,getSum )
import Lib3 (students,_select,firstName,lastName,studentName)
import Prelude
import System.Random (getStdRandom,randomR)
import Data.List ()
import Data.Monoid ()
import Data.Semigroup ( Min )

infixl 4 $>
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

minDie :: Int 
minDie = 1
maxDie :: Int 
maxDie = 6

rollDice :: IO Int
rollDice = getStdRandom (randomR (1,6))

-- data Tree = Leaf | Node Tree Tree deriving Show

-- trees :: Int -> [Tree]
-- trees 0 = [Leaf]
-- trees n = [Node lt rt | l <- [0..(n-1)], lt <- trees l,rt <- trees (n-l-1)]

-- brace :: Tree -> String
-- brace Leaf = ""
-- brace (Node l r) = "(" ++ brace l ++ ")" ++ brace r

insert :: (Num a,Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x > y = y : insert x ys
                | otherwise = x:(y:ys)

insertSort :: (Num a,Ord a)=> [a] -> [a] -> [a]
-- insertSort [] lst = lst
-- insertSort (x:xs) [] = insertSort xs [x]
-- insertSort (x:xs) lst = insertSort xs (l <> [x] <> r)
--     where (l,r) = filterSplit (<x) lst
insertSort [] lst = lst
insertSort (x:xs) lst = insertSort xs (insert x lst)

bSearch :: (Ord a) => a -> [a] -> Bool
bSearch _ [] = False
bSearch a xs | m < a = bSearch a behind
             | m > a = bSearch a front
             | otherwise = True
                where (front,m:behind) = splitAt (length xs `div` 2) xs

minFree :: [Int] -> Int
minFree xs = bsearch xs 0 (length xs - 1)

bsearch :: [Int] -> Int -> Int -> Int
bsearch xs l u 
    | xs == [] = l
    | length as == m-l+1 = bsearch bs (m+1) u
    | otherwise = bsearch as l m 
    where
    m=(l+u)`div`2
    (as, bs) = partition (<=m) xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (l,r)
    where 
        l = filter f xs
        r = filter (not . f) xs

positions :: (Eq t, Num t, Num a, Enum a) => t -> a -> [[a]]
positions 0 _ = [[]]
positions k n = [x:xs | x <- [1..n], xs <- positions (k-1) n]

noSameRow :: Eq a => [a] -> Bool
noSameRow [] = True
noSameRow (x:xs) = (not $ elem x xs) && noSameRow xs

noSameDiag :: (Eq a, Num a, Enum a) => [a] -> Bool
noSameDiag [] = True
noSameDiag xs@(_:xs') = and [abs (i1-i) /= abs (p1-p) | (i,p) <- ip] && noSameDiag xs'
    where (i1,p1):ip = zip [1..] xs

queen :: (Eq a, Num a, Enum a) => a -> [[a]]
queen n = [xs | xs <- positions n n, noSameRow xs, noSameDiag xs]
safe :: Int -> [Int] -> Int -> Bool
safe _ [] _ = True
safe x (x1:xs) n =
    x /= x1
    && x /= x1 + n && x /= x1 - n
    && safe x xs (n+1)

queensN :: Int -> [[Int]]
queensN n = queens n
  where
    queens 0 = [[]]
    queens m = [ x : y | y <- queens (m-1), x <- [1..n], safe x y 1]


lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where
        vowels = length . filter (`elem` "aeiou")

-- newtype Product a = Product { getProduct :: a }
--     deriving (Eq, Ord, Read, Show, Bounded)

-- instance Num a => Semigroup (Product a) where
--     Product x <> Product y = Product (x * y)

-- instance Num a => Monoid (Product a) where
--     mempty = Product 1
--     Product x `mappend` Product y = Product (x * y + 1)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = foldMap f l `mappend`
                                f x `mappend`
                                foldMap f r

testTree :: Tree Integer
testTree = Node 5  
            (Node 3  
             (Node 1 Empty Empty)  
             (Node 6 Empty Empty)  
            )  
            (Node 9  
             (Node 8 Empty Empty)  
             (Node 10 Empty Empty)  
            )

type Indexed a = (Min Int, a)
-- type String = [Char]

mkToken :: [Indexed Char] -> Indexed [Char]
mkToken = sequenceA

token :: [Indexed Char] -> [Indexed String]
token = undefined

data B = B !Int !Int deriving (Eq,Show)
instance Semigroup B where
    (<>) = mappend

instance Monoid B where
    mempty = B 0 0
    mappend (B a b) (B c d) 
        | b <= c = B (a+c - b) d
        | otherwise = B a (d + b - c)

parse :: Char -> B
parse '(' = B 0 1
parse ')' = B 1 0 
parse _   = B 0 0 

balance :: Foldable t => t Char -> Bool
balance xs = foldMap parse xs == B 0 0

sq :: Integer -> Integer
sq = (\x -> x * x) 

main :: IO ()
main = do 
    putStrLn "Input your SCI expr:"
    expr <- getLine
    putStrLn $ (show $ snd $ runParser sci expr)

