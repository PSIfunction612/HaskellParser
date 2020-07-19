module Parser where

import Data.Char

newtype Parser a = Parser {apply :: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f where
    f "" = []
    f (ch:chs)  = [(ch, chs)]

instance Functor Parser where
    fmap f p = Parser (\s -> map (\(x, y) -> (f x, y)) $ apply p s) 

instance Applicative Parser where
    pure a = Parser $ \s -> [(a, s)]
    --  (<*>) :: f (a -> b) -> f a -> f b
    (<*>) pf ps = Parser $ \s ->  [ (f a, s'') | (f, s') <- apply pf s, (a, s'') <- apply ps s']

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser foo where
    foo "" = []
    foo (ch:str) 
     | pr ch = [(ch, str)]
     | otherwise = []

lower :: Parser Char
lower = satisfy isLower

char:: Char -> Parser Char
char c = satisfy (== c)

digit:: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication =  (*) <$> digit <* char '*' <*> digit