module Lexer
    ( tokenize
    , Token(..)
    , Atom(..)
    )
where

import           Data.Char

data Token = Token {
    begin :: Int,
    end :: Int,
    atom :: Atom
} deriving Show

data Atom = AInt Int | ABool Bool | AOp String deriving Show

takeNum :: String -> Int -> Int -> (String, Int)
takeNum []       num _    = ([], num)
takeNum (s : xs) num cntr = if isDigit s
    then takeNum xs (digitToInt s * cntr + num) (cntr * 10)
    else (s : xs, num)

consumeNum :: String -> [Atom] -> (String, [Atom])
consumeNum s atoms = (rem, atoms)  where
    (rem, num) = takeNum s 0 1
    atoms      = atoms ++ [AInt num]

tokenize :: String -> [Atom] -> [Atom]
tokenize []       atoms = atoms
tokenize (s : xs) atoms = if isDigit s
    then let (rem, atoms) = consumeNum (s : xs) atoms in tokenize rem atoms
    else atoms
