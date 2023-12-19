{-# LANGUAGE DuplicateRecordFields #-}
module Lexer (
    LexState (..),
    Token (..),
    TokenType (..),
    Lexer.lex,
) where
import Fallible
import Data.List (stripPrefix)
import Prelude hiding (LT, EQ, GT)
import Text.Regex.TDFA ( (=~) )
import Token

data LexState = LexState { text :: String
                         , row :: Int
                         , col :: Int
                         , tokens :: [Token]
                         } deriving (Show);

isKeyword :: String -> Bool;
isKeyword "continue" = True;
isKeyword "nullable" = True;
isKeyword "uintptr" = True;
isKeyword "vastart" = True;
isKeyword "append" = True;
isKeyword "assert" = True;
isKeyword "delete" = True;
isKeyword "export" = True;
isKeyword "insert" = True;
isKeyword "offset" = True;
isKeyword "opaque" = True;
isKeyword "return" = True;
isKeyword "static" = True;
isKeyword "struct" = True;
isKeyword "switch" = True;
isKeyword "valist" = True;
isKeyword "abort" = True;
isKeyword "align" = True;
isKeyword "alloc" = True;
isKeyword "break" = True;
isKeyword "const" = True;
isKeyword "defer" = True;
isKeyword "false" = True;
isKeyword "match" = True;
isKeyword "never" = True;
isKeyword "union" = True;
isKeyword "vaarg" = True;
isKeyword "vaend" = True;
isKeyword "yield" = True;
isKeyword "bool" = True;
isKeyword "case" = True;
isKeyword "else" = True;
isKeyword "free" = True;
isKeyword "null" = True;
isKeyword "rune" = True;
isKeyword "size" = True;
isKeyword "true" = True;
isKeyword "type" = True;
isKeyword "uint" = True;
isKeyword "void" = True;
isKeyword "def" = True;
isKeyword "f32" = True;
isKeyword "f64" = True;
isKeyword "for" = True;
isKeyword "i16" = True;
isKeyword "i32" = True;
isKeyword "i64" = True;
isKeyword "int" = True;
isKeyword "len" = True;
isKeyword "let" = True;
isKeyword "str" = True;
isKeyword "u16" = True;
isKeyword "u32" = True;
isKeyword "u64" = True;
isKeyword "use" = True;
isKeyword "as" = True;
isKeyword "fn" = True;
isKeyword "i8" = True;
isKeyword "if" = True;
isKeyword "u8" = True;
isKeyword "_" = True;
isKeyword _ = False;

prefixCheck :: String -> String -> Maybe (String, String);
prefixCheck prefix text
    | Just suffix <- stripPrefix prefix text = Just (prefix, suffix)
    | Nothing <- stripPrefix prefix text = Nothing;

lexStep :: LexState -> Fallible LexState;
lexStep LexState{text=tex, col=c, row=r, tokens=tok}
    | Just (prefix, suffix) <- prefixCheck "\n"  tex = Success [] LexState {text=suffix, col=1, row=r+1, tokens=tok}
    | Just (prefix, suffix) <- prefixCheck " "   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok}
    | Just (prefix, suffix) <- prefixCheck "\t"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok}
    
    | Just (prefix, suffix) <- prefixCheck "&&=" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_AMP_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "..." tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=TPL_DOT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "<<=" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_LT_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ">>=" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_GT_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "^^=" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_HAT_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "||=" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_PIPE_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "!="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=BANG_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "%="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=MOD_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "&="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AMP_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "*="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=STAR_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "+="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=PLUS_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "-="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=MINUS_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ".."  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_DOT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "&&"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_AMP, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "/="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DIV_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "::"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_COL, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "<<"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_LT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "<="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=LT_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "=="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "=>"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=ARROW, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ">="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=GT_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ">>"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_GT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "^="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=HAT_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "^^"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_HAT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "|="  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=PIPE_EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "||"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DBL_PIPE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "!"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=BANG, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "%"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=MOD, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "&"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AMP, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "*"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=STAR, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "+"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=PLUS, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "-"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=MINUS, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "("   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=L_PAREN, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ")"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=R_PAREN, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ","   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=COMA, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "."   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DOT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "/"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DIV, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ":"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=COL, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ";"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=SEMI, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "<"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=LT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "="   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=EQ, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck ">"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=GT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "?"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=Q_MARK, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "["   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=L_BRAK, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "]"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=R_BRAK, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "^"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=HAT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "{"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=L_BRACE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "|"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=PIPE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "}"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=R_BRACE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "~"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=TILDE, row=r, col=c, literal=prefix}]}

    | Just (prefix, suffix) <- prefixCheck "@threadlocal"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_THREADLOCAL, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "@offset"        tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_OFFSET, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "@packed"        tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_PACKED, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "@symbol"        tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_SYMBOL, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "@init"          tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_INIT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "@test"          tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_TEST, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "@fini"          tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AT_FINI, row=r, col=c, literal=prefix}]}

    | (_, prefix, suffix) <- tex =~ "\\`[a-zA-Z_][a-zA-Z0-9_]*" :: (String, String, String), not (isKeyword prefix), prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=NAME, row=r, col=c, literal=prefix}]}
    
    | (_, prefix, suffix) <- tex =~ "\\`[0-9]+\\.[0-9]+([eE][+-]?[0-9]+)?(f32|f64)?" :: (String, String, String),              prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FLOAT_LIT, row=r, col=c, literal=prefix}]}
    | (_, prefix, suffix) <- tex =~ "\\`[0-9]+([eE][+-]?[0-9]+)?(f32|f64)" :: (String, String, String),                      prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FLOAT_LIT, row=r, col=c, literal=prefix}]}
    | (_, prefix, suffix) <- tex =~ "\\`0x[0-9a-fA-F]+\\.[0-9a-fA-F]+([pP][+-]?[0-9]+)(f32|f64)?" :: (String, String, String), prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FLOAT_LIT, row=r, col=c, literal=prefix}]}
    | (_, prefix, suffix) <- tex =~ "\\`0x[0-9a-fA-F]+([pP][+-]?[0-9]+)(f32|f64)?" :: (String, String, String), prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FLOAT_LIT, row=r, col=c, literal=prefix}]}

    | (_, prefix, suffix) <- tex =~ "\\`0x[a-fA-Z0-9]+(i|u|z|i8|i16|i32|i64|u8|u16|u32|u64)?" :: (String, String, String),           prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=INT_LIT, row=r, col=c, literal=prefix}]}
    | (_, prefix, suffix) <- tex =~ "\\`0o[0-7]+(i|u|z|i8|i16|i32|i64|u8|u16|u32|u64)?" :: (String, String, String),                 prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=INT_LIT, row=r, col=c, literal=prefix}]}
    | (_, prefix, suffix) <- tex =~ "\\`0b[01]+(i|u|z|i8|i16|i32|i64|u8|u16|u32|u64)?" :: (String, String, String),                  prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=INT_LIT, row=r, col=c, literal=prefix}]}
    | (_, prefix, suffix) <- tex =~ "\\`[0-9]+([eE][+-]?[0-9]+)?(i|u|z|i8|i16|i32|i64|u8|u16|u32|u64)?" :: (String, String, String), prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=INT_LIT, row=r, col=c, literal=prefix}]}

    | (_, prefix, suffix) <- tex =~ ("\\`'([^\\\']|"++escapeReg++")'") :: (String, String, String), prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=RUNE_LIT, row=r, col=c, literal=prefix}]}

    | (_, prefix, suffix) <- tex =~ ("\\`((\"([^\"\\\\]|"++escapeReg++")*\")|(`[^`]*`))+") :: (String, String, String), prefix /= "" = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=STRING_LIT, row=r, col=c, literal=prefix}]}

    | Just (prefix, suffix) <- prefixCheck "continue" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=CONTINUE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "nullable" tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=NULLABLE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "uintptr"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=UINTPTR, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "vastart"  tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=VASTART, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "append"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=APPEND, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "assert"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=ASSERT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "delete"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DELETE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "export"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=EXPORT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "insert"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=INSERT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "offset"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=OFFSET, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "opaque"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=OPAQUE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "return"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=RETURN, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "static"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=STATIC, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "struct"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=STRUCT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "switch"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=SWITCH, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "valist"   tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=VALIST, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "abort"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=ABORT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "align"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=ALIGN, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "alloc"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=ALLOC, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "break"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=BREAK, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "const"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=CONST, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "defer"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DEFER, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "false"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FALSE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "match"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=MATCH, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "never"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=NEVER, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "union"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=UNION, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "vaarg"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=VAARG, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "vaend"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=VAEND, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "yield"    tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=YIELD, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "bool"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=BOOL, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "case"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=CASE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "else"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=ELSE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "free"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FREE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "null"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=NULL, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "rune"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=RUNE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "size"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=SIZE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "true"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=TRUE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "type"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=TYPE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "uint"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=UINT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "void"     tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=VOID, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "def"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=DEF, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "f32"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=F32, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "f64"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=F64, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "for"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FOR, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "i16"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=I16, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "i32"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=I32, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "i64"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=I64, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "int"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=INT, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "len"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=LEN, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "let"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=LET, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "str"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=STR, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "u16"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=U16, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "u32"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=U32, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "u64"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=U64, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "use"      tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=USE, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "as"       tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=AS, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "fn"       tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=FN, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "i8"       tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=I8, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "if"       tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=IF, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "u8"       tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=U8, row=r, col=c, literal=prefix}]}
    | Just (prefix, suffix) <- prefixCheck "_"        tex = Success [] LexState {text=suffix, col=c + length prefix, row=r, tokens=tok ++ [Token{tokenType=BLANK, row=r, col=c, literal=prefix}]}
    | otherwise = Fail [MessageLexNoTokenFound{row=r, col=c, firstChar=head tex}]
    where
        escapeReg = "\\\\0|\\\\a|\\\\b|\\\\f|\\\\n|\\\\r|\\\\t|\\\\v|\\\\\\\\|\\\\'|\\\\\"|\\\\x[0-9a-fA-F]{2}|\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}"    

lex :: LexState -> Fallible [Token];
lex pre
    | Success _ LexState{text="", tokens=tok, col=c, row=r} <- lexStep pre = Success [] (tok++[Token{tokenType=EOF, row=r, col=c, literal=""}])
    | Success _ post@LexState{} <- lexStep pre = Lexer.lex post
    | Fail errors <- lexStep pre = Fail errors;