module Fallible (
    Message (..),
    Fallible (..),
    shell
) where
import Token
import Control.Monad (liftM, ap)

data Message = MessageParseExpectedOneFoundAnother{expected::Token.TokenType, found::Token.Token}
             | MessageParseUseStatementHasAliasAndStar Token.Token
             | MessageParseUseStatementDblColButNoMembers Token.Token
             | MessageParseBothHalvesOffOneOfFailed [Message] [Message]
             | MessageLexNoTokenFound{row::Int, col::Int, firstChar::Char} deriving (Show); 
             
data Fallible  t = Fail [Message] | Success [Message] t deriving (Show);

injectMessagesPre :: Fallible t -> [Message] -> Fallible t;
injectMessagesPre (Success m1 d) m2 = Success (m2++m1) d;
injectMessagesPre (Fail m1) m2 = Fail (m2++m1);

shell :: Fallible t -> t;
shell (Success _ d) = d;


instance Functor Fallible where
    fmap = liftM
instance Applicative Fallible where 
    pure = Success []
    (<*>) = ap
instance Monad Fallible where
    Fail m1 >>= f = (Fail m1)
    (Success m1 d) >>= f = injectMessagesPre (f d) m1
    
    return = pure