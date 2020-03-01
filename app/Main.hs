module Main where

import           Lib
import           Lexer

main :: IO ()
main = print $ tokenize "155" []
