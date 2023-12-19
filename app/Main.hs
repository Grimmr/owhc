module Main where

import qualified Lexer (LexState(..), lex);

main :: IO ()
main = do 
    input <- readFile "input.txt"
    putStrLn (show (Lexer.lex Lexer.LexState{Lexer.text=input, Lexer.row=1, Lexer.col=1, Lexer.tokens=[]}))
