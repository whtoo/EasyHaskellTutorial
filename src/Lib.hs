module Lib
    (
        someFunc,
        double,
        addThenDouble,
        SixSidedDie(S1,S2,S3,S4,S5,S6),
        Describale(describle,printDesc),
        quickSort',
        mean,
        split,
        combine,
        mergeSort,
        Student,
        zhao,
        qian,
        g,
        f,
        classX,
        map'
        ,filterSplit
        ) where
import Data.List ()
import System.IO ()
-- import qualified Data.Map as Map

map' :: (t -> a) -> [t] -> [a]
map' f (x:xs) = (f x) : map' f xs
map' _ [] = []

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Generic programming --
double :: Num a => a -> a
double x = x * 2

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

-- Quick Sort --
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort mini ++ [x] ++ quickSort maxi
    where   mini = filter (<x) xs
            maxi = filter (>=x) xs

-- 改进 filter 过程 --
filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit _ [] = ([],[])
filterSplit f (x:xs)    |   f x = ((x:l),r)
                        |   otherwise = (l,(x:r))
    where  (l,r) = filterSplit f xs

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' [x] = [x]
quickSort' (x:xs) = quickSort' l ++ [x] ++ quickSort' r
    where   (l,r) = filterSplit (<x) xs

-- Merge Sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =  let (ys,zs) = split xs [] []
                in combine (mergeSort ys) (mergeSort zs)

split :: [a] -> [a] -> [a] -> ([a], [a])
split [x] accA accB = (x:accA,accB)
split [] accA accB = (accA,accB)
split (x:y:zs) accA accB = split zs (x:accA) (y:accB)


combine :: Ord a => [a] -> [a] -> [a]
combine (x:xs) (y:ys) | x <= y = x:combine xs (y:ys)
                      | otherwise = y : combine (x:xs) ys
combine [] ys = ys
combine xs [] = xs                    

-- fib

-- Data definations --
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- TypeClass --

class Describale a where
    describle :: a -> String
    printDesc :: a -> IO ()

instance Show SixSidedDie where 
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four" 
    show S5 = "five" 
    show S6 = "six"

-- Implement Describale on SixSidedDie --
instance Describale SixSidedDie where
    describle = show
    printDesc = putStrLn . describle

instance Describale Int where
    describle = show
    printDesc = putStrLn . describle

instance Describale Integer where
    describle = show
    printDesc = putStrLn . describle

instance Describale Float where
    describle = show
    printDesc = putStrLn . describle

instance Describale Double where
    describle = show
    printDesc = putStrLn . describle

type ID = Int
type Name = String
type Score = Int
data Student = Stu ID Name Score

zhao :: Student
zhao = Stu 1 "Zhao" 99
qian :: Student
qian = Stu 1 "Qian" 98
classX :: (Student, Student)
classX = (zhao,qian)

instance Eq Student where
    Stu x _ _ == Stu y _ _ = x == y

-- Transformer --

mean :: (Real a) => [a] -> Double
mean xs = total / len where
    total = (realToFrac . sum) xs
    len = (realToFrac . length) xs

-- Monad --
g :: [String]
g = do
    x <- ['a','b','c']
    y <- ['a','b','c']
    pure ([x] ++ [y])

f :: (Eq b, Fractional b) => b -> Maybe b
f x = do
    y <- if x /= 0 then pure (100/x)
                   else Nothing
    pure (y * 10)


-- newtype Reader r a = Reader { runReader :: r -> a}
-- instance Monad Reader where
--     Reader f >>= Reader g = Reader (\r -> g (f r) r)


-- f :: Reader Double Double
-- f = do
--     x <- Reader (+1)
--     y <- Reader (x*)
--     z <- Reader (y-)
--     pure z

-- class Apllicative m => Monad m where
--     return :: a -> m a
--     return = pure

--     join :: m (m a)
--     (>>=) :: m a -> (a -> m b) -> m b

-- instance Monad Maybe where
--     join (Just (Just x)) = x

--     Just x >>= f = fx
--     _      >>= _ = Nothing

-- instance Monad [] where
--     (>>=) :: [a] -> (a -> [b]) -> [b]
--     xs >>= fs = concat ()



-- newtype Parser a = Parser 
--     { runParser :: String -> (String, Maybe a)}

-- digits :: Parser Int
-- digits = Parser $ \input ->
--     let r = takeWhile isDigit input
--     in if null r
--         then ([],Nothing)
--         else (drop (length r) input, Just $
--         foldl (\acc a -> acc * 10 + (fromEnum a-48)) 0 r)
--     where isDigit x = x >= '0' && x <= '9'

-- chars :: Parser String
-- chars = Parser $ \input ->
--     let r = takeWhile isChar input
--     in if null r
--         then ([],Nothing)
--         else (drop (length r) input, Just r)
--     where isChar x = elem x ['a'..'z'] || elem x ['A'..'Z']

-- char :: Char -> Parser ()
-- char c = Parser $ \ input -> case input of
--     (x:xs) | x == c -> (xs,Just ())
--     _ -> (input,Nothing)

-- instance Functor Parser where
--     fmap f (Parser p) = Parser $ \ input ->
--         let (input',ma) = p input
--         in (input',f <$> ma)


-- type WillCoId = Int
-- type GamerId  = Int
-- type UserName = String
-- type PlayerCredits = Int

-- gamerIdDB :: Map.Map WillCoId GamerId
-- gamerIdDB = Map.fromList [(1001,1),(1002,2),(1003,3),(1004,4),(1005,5),(1006,6)]
-- gamerNameDB :: Map.Map GamerId UserName
-- gamerNameDB = Map.fromList [(1,"arhtur"),(2,"angel"),(3,"will"),(4,"lisa"),(5,"luke"),(6,"john")]
-- gamerCreditDB :: Map.Map UserName PlayerCredits
-- gamerCreditDB = Map.fromList [("arhtur",123),("angel",213),("will",95),("lisa",98),("luke",97),("john",98)]
-- lookupGamerId :: WillCoId -> Maybe GamerId 
-- lookupGamerId id = Map.lookup id gamerIdDB
-- lookupUserName :: GamerId -> Maybe UserName
-- lookupUserName id = Map.lookup id gamerNameDB

-- lookupCredits :: UserName -> Maybe PlayerCredits
-- lookupCredits userName = Map.lookup userName gamerCreditDB
-- creditsFromWCId :: WillCoId -> Maybe PlayerCredits
-- creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

-- instance Applicative Parser where
--     pure x = Parser $ \ input -> (input,Just x)
--     (<*>) pf pa = do
--         f <- pf
--         a <- pa
--         return $ f a

-- instance Monad Parser where
--     Parser pa >>= f = Parser $ \ input -> 
--         case pa input of
--             (input',Just a) -> runParser (f a) input'
--             (input',Nothing) -> (input',Nothing)


-- sci :: Parser Int
-- sci = do
--     base <- digits
--     char 'e'
--     exp  <- digits
--     pure (base * 10^exp)