module AIParser (Parser(..),satisfy,char,some,many,optional,isDigit,num,parseStr,sequ) where
import Control.Applicative ( Alternative(empty, (<|>)) )
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }

-- instance Functor Parser where
--     fmap f p = Parser $ \ str -> case runParser p str of
--         Just (a,s) -> Just (f a,s)
--         Nothing -> Nothing
instance Functor Parser where
    fmap f p = Parser $ \ str -> case runParser p str of
        Just (a,s) -> Just (f a,s)
        Nothing -> Nothing

instance Applicative Parser where
    pure a = Parser $ \ str -> Just (a, str)
    (<*>) fp a = Parser $ \ str -> case runParser fp str of
        Nothing -> Nothing
        Just (ab,s) -> case runParser a s of
            Nothing -> Nothing
            Just (at,s1) -> Just (ab at,s1)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (<|>) a b = Parser $ \ str -> case runParser a str of
        Nothing -> runParser b str
        Just (a1,b1) -> Just (a1,b1)

-- instance (Semigroup a) => Semigroup (Parser a) where
--     (<>) p1 p2 = Parser $ \ str -> case runParser p1 str of
--         Nothing -> runParser p2 str
--         Just (a1,b1) -> case runParser p2 b1 of
--             Just (a2,b2) -> Just (a1<>a2,b2)
--             Nothing -> Nothing

-- instance (Monoid a) => Monoid (Parser a) where
--     mempty = Parser $ \ str -> Just (mempty,str)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \ str -> case str of
    [] -> Nothing
    s:ss -> if f s then Just (s,ss) else Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

some :: Alternative f => f a -> f [a]
some v = some_v 
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v
        
many :: Alternative f => f a -> f [a]
many v = many_v 
    where many_v = some_v <|> pure []
          some_v = (:) <$> v <*> many_v


optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing


-- isDigit :: Char -> Bool
-- isDigit c = elem c ['0'..'9']

num :: Parser Int
num  = fmap (foldl (\ x y -> 10*x+y) 0) (many digit)
    where digit = fmap digitToInt (satisfy isDigit)

sequ :: Parser a -> Parser [a] -> Parser [a]
sequ x y = Parser $ \ str -> case runParser x str of
    Nothing -> Nothing
    Just (s,ss) -> case runParser y ss of
        Nothing -> Nothing
        Just (s1,ss1) -> Just (s:s1,ss1)

parseStr :: [Char] -> Parser [Char]
parseStr strs = foldr sequ (Parser $ \ str -> Just ("",str))  [char s | s <- strs]