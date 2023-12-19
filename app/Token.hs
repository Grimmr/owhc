module Token (
    Token(..),
    TokenType(..),
    makeUnknownFromPos,
    makeUnknownFromTokens
) 
where
data TokenType = DBL_AMP_EQ
               | TPL_DOT
               | DBL_LT_EQ
               | DBL_GT_EQ
               | DBL_HAT_EQ
               | DBL_PIPE_EQ
               | BANG_EQ
               | MOD_EQ
               | AMP_EQ
               | STAR_EQ
               | PLUS_EQ
               | MINUS_EQ
               | DBL_DOT
               | DBL_AMP
               | DIV_EQ
               | DBL_COL
               | DBL_LT
               | LT_EQ
               | DBL_EQ
               | ARROW
               | GT_EQ
               | DBL_GT
               | HAT_EQ
               | DBL_HAT
               | PIPE_EQ
               | DBL_PIPE
               | BANG
               | MOD 
               | AMP
               | STAR 
               | PLUS
               | MINUS
               | L_PAREN
               | R_PAREN
               | COMA 
               | DOT
               | DIV
               | COL
               | SEMI
               | LT
               | EQ
               | GT
               | Q_MARK
               | L_BRAK
               | R_BRAK
               | HAT
               | L_BRACE
               | PIPE 
               | R_BRACE
               | TILDE
               | AT_THREADLOCAL
               | AT_OFFSET
               | AT_PACKED
               | AT_SYMBOL
               | AT_INIT
               | AT_TEST
               | AT_FINI
               | NAME
               | CONTINUE
               | NULLABLE
               | UINTPTR
               | VASTART
               | APPEND
               | ASSERT
               | DELETE
               | EXPORT
               | INSERT
               | OFFSET
               | OPAQUE
               | RETURN
               | STATIC
               | STRUCT
               | SWITCH
               | VALIST
               | ABORT
               | ALIGN
               | ALLOC
               | BREAK
               | CONST
               | DEFER
               | FALSE
               | MATCH
               | NEVER
               | UNION
               | VAARG
               | VAEND
               | YIELD
               | BOOL
               | CASE
               | ELSE
               | ENUM
               | FREE
               | NULL
               | RUNE
               | SIZE
               | TRUE
               | TYPE
               | UINT
               | VOID
               | DEF
               | F32
               | F64
               | FOR
               | I16
               | I32
               | I64
               | INT
               | LEN
               | LET
               | STR
               | U16
               | U32
               | U64
               | USE
               | AS
               | FN
               | I8
               | IF
               | IS
               | U8
               | BLANK 
               | INT_LIT
               | FLOAT_LIT
               | RUNE_LIT
               | STRING_LIT
               | UNKOWN {-special token used when we need to store a possition in the initial input but we don't have the token becuase one doesnt exist (eg: the Nothing result of an optional expansion) we really dont want this to make it into the final tree as it will behave weird around whitespace (esp new lines) but the info it gives is better then none-} 
               | EOF deriving (Show, Eq);

data Token = Token { tokenType::TokenType
                   , row :: Int
                   , col :: Int
                   , literal :: String
                   } deriving (Show);

makeUnknownFromPos r c = Token{tokenType=UNKOWN,row=r,col=c,literal=""}
makeUnknownFromTokens (h:_) | Token{row=r,col=1} <- h = makeUnknownFromPos r 1
                            | Token{row=r,col=c} <- h = makeUnknownFromPos r (c-1)