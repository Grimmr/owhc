module Main where

import qualified Lexer (LexState(..), lex);
import qualified Parser 
import Fallible

main :: IO ()
main = do 
    input <- readFile "input.txt"
    putStrLn (show (Parser.parseNodeSubunit(shell (Lexer.lex Lexer.LexState{Lexer.text=input, Lexer.row=1, Lexer.col=1, Lexer.tokens=[]}))))
