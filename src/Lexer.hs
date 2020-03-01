module Lexer
    ( tokenize
    , Token(..)
    , Atom(..)
    , ParserResult(..)
    )
where

import           Data.Char
import           Control.Applicative

data ParserError = ParserError{
    reason :: String,
    errorBegin :: Int,
    errorEnd :: Int
} deriving Show

data ParserResult a = Failure String | Success String a | Error ParserError deriving Show

instance Functor ParserResult where
    fmap fn (Success s a) = Success s $ fn a
    fmap _  (Error   pe ) = Error pe
    fmap _  (Failure s  ) = Failure s

instance Applicative ParserResult where
    pure = Success ""
    (Success _ f) <*> a = fmap f a
    (Error   a  ) <*> _ = Error a
    (Failure a  ) <*> _ = Failure a

data Token = Token {
    tokenBegin :: Int,
    tokenEnd :: Int,
    atom :: Atom
} deriving Show

data Atom = AInt Int | ABool Bool | AOp String deriving Show

takeNum :: String -> String -> ParserResult String
takeNum [] num = Success [] num
takeNum s@(x : xs) num | isDigit x = takeNum xs $ num ++ [x]
                       | otherwise = Success s num

consumeNum :: String -> ParserResult Atom
consumeNum s = AInt . read <$> takeNum s ""


tokenize :: String -> [Atom] -> ParserResult [Atom]
tokenize []        atoms = Success "" atoms
tokenize s@(x : _) atoms = if isDigit x
    then (\atom -> atoms ++ [atom]) <$> consumeNum s
    else Success "" atoms
