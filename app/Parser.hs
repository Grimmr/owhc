module Parser where
import qualified Token

import Fallible

data ParseResult t = ParseResult {node::t, tokens::[Token.Token], start::Token.Token, end::Token.Token} deriving (Show)

data Dummy = Dummy String deriving (Show);
data NodeDummyPair = NodeDummyPair Dummy Dummy deriving (Show);

type NodeType = Dummy;
type NodeExpression = Dummy;
type NodeStaticAssertionExpression = Dummy;

data LeafName = LeafName String deriving (Show);
data LeafStringConstant = LeafName String deriving (Show);

type NodeIdentifier = [LeafName];

data NodeGlobalBinding = NodeGlobalBinding{declAttr::Maybe LeafStringConstant, threadLocal::Bool, GBIdent::NodeIdentifier, GBType::Maybe NodeType, GBExpr::Maybe NodeExpression} deriving (Show)
data NodeDeclaration = NodeGlobalDeclaration Bool [NodeGlobalBinding] | NodeGlobalStaticAssertionExpression NodeStaticAssertionExpression deriving (Show);
data NodeTopLevelDeclaration = NodeTopLevelDeclaration{exported::Bool, dec::NodeDeclaration} deriving (Show);

data NodeMember = NodeMember {memberAlias::Maybe LeafName, memberActual::LeafName} deriving (Show);
data NodeMembers = NodeMemberList [NodeMember] | NodeMemberStar deriving (Show);
data NodeUseStatement = NodeUseStatement {alias::Maybe LeafName, ident::NodeIdentifier, members::Maybe NodeMembers} deriving (Show);
data NodeSubunit = NodeSubunit [NodeUseStatement] [NodeTopLevelDeclaration] deriving (Show);

data Empty = Empty deriving (Show);

checkToken :: Token.TokenType -> Token.Token -> Bool
checkToken t l = (Token.tokenType l) == t

checkTokens :: [Token.TokenType] -> [Token.Token] -> Bool
checkTokens [t] [l] = checkToken t l
checkTokens (t:ts) (l:ls) = checkToken t l && checkTokens ts ls 

consumeToken :: Token.TokenType -> [Token.Token] -> Fallible (ParseResult Token.Token);
consumeToken t (h:tail) | (Token.tokenType h) == t = Success [] ParseResult{node=h, tokens=tail, start=h, end=h};
consumeToken t (h:_) = Fail [MessageParseExpectedOneFoundAnother{expected=t, found=h}]; 

consumeTokens :: [Token.TokenType] -> [Token.Token] -> Fallible (ParseResult Empty);
consumeTokens (h:tail) tok | tail /= [] = do n1 <- consumeToken h tok
                                             n2 <- consumeTokens tail (tokens n1)
                                             Success [] ParseResult{node=Empty, tokens=(tokens n2), start=(start n1), end=(end n2)}
                           | tail == [] = do n1 <- consumeToken h tok 
                                             Success [] ParseResult{node=Empty, tokens=(tokens n1), start=(start n1), end=(end n1)}                    

shellMaybeList :: Maybe [a] -> [a]
shellMaybeList Nothing = []
shellMaybeList (Just as) = as 

shellMaybeBool :: Maybe Bool -> Bool
shellMaybeBool Nothing = False
shellMaybeBool Just v = v

optional :: ([Token.Token] -> Fallible (ParseResult b)) -> ParseResult c -> [Token.Token] -> Fallible (ParseResult (Maybe b))
optional f p tok | Success m ParseResult{node=n, tokens=t, start=s, end=e} <- f tok = Success m (ParseResult{node=(Just n), tokens=t, start=s, end=e})
                 | Fail m <- f tok = Success [] (ParseResult{node=Nothing, tokens=tok, start=(end p), end=(end p)})

oneOf :: ([Token.Token] -> Fallible (ParseResult b)) -> ([Token.Token] -> Fallible (ParseResult b)) -> [Token.Token] -> Fallible (ParseResult b)
oneOf f g tok | Success m d <- f tok = Success m d
              | Success m d <- g tok = Success m d
              | Fail m1 <- f tok, Fail m2 <- g tok = Fail [MessageParseBothHalvesOffOneOfFailed m1 m2]

oneOfMany :: [[Token.Token] -> Fallible (ParseResult b)] -> [Token.Token] -> Fallible (ParseResult b)
oneOfMany [a] tok = a tok
oneOfMany [a,b] tok = oneOf a b tok
oneOfMany h:t tok = oneOf h (oneOfMany t) tok 

nothing :: a -> (ParseResult b) -> [Token.Token] -> Fallible (ParseResult a)
nothing a i tok = Success [] (ParseResult{node=a, tokens=tok, start=(end i), end=(end i)})  

parseNodeDummy :: [Token.Token] -> Fallible (ParseResult Dummy);
parseNodeDummy tok = do n1 <- consumeToken Token.NAME tok 
                        Success [] (ParseResult{node=Dummy (Token.literal (node n1)), tokens=(tokens n1), start=(start n1), end=(end n1)})

parseNodeStaticAssertionExpression = parseNodeDummy;
parseNodeType = parseNodeDummy;
parseNodeExpression = parseNodeDummy;

{-Parse Leafs-}
parseLeafName :: [Token.Token] -> Fallible (ParseResult LeafName)
parseLeafName tok = do n1 <- consumeToken Token.NAME tok
                       Success [] (ParseResult{node=(LeafName (Token.literal (node n1))), tokens=(tokens n1), start=(start n1), end=(end n1)})

{-6.4 Identifiers-}
parseNodeIdentifier :: [Token.Token] -> Fallible (ParseResult NodeIdentifier)
parseNodeIdentifier tok = do n1 <- parseLeafName tok
                             n2 <- optional hDblColThenIdent n1 (tokens n1)
                             Success [] ParseResult{node=([node n1]++(shellMaybeList (node n2))), tokens=(tokens n2), start=(start n1), end=(end n2)}
                         where
                             hDblColThenIdent t = do {m1 <- consumeToken Token.DBL_COL t; m2 <- parseNodeIdentifier (tokens m1); Success [] ParseResult{node=(node m2), tokens=(tokens m2), start=(start m1), end=(end m2)}}

{-6.11 Declarations-}


{-6.12 Units -}
parseNodeMember :: [Token.Token] -> Fallible (ParseResult NodeMember)
parseNodeMember tok = oneOf (hTwoNames) (hOneName) tok
                      where
                         hOneName t = do {m1 <- parseLeafName t; Success [] (ParseResult{node=(NodeMember{memberAlias=Nothing, memberActual=(node m1)}), tokens=(tokens m1), start=(start m1), end=(end m1)})}
                         hTwoNames t = do {m1 <- parseLeafName t; m2 <- consumeToken Token.EQ (tokens m1); m3 <- parseLeafName (tokens m2); Success [] (ParseResult{node=(NodeMember{memberAlias=(Just (node m1)), memberActual=(node m3)}), tokens=(tokens m3), start=(start m1), end=(end m3)})}
                         

parseNodeMemberList :: [Token.Token] -> Fallible (ParseResult [NodeMember])
parseNodeMemberList tok = do n1 <- parseNodeMember tok
                             n2 <- oneOf hComaThenList (hOptionalComma n1) (tokens n1)
                             Success [] (ParseResult{node=([(node n1)]++(node n2)), tokens=(tokens n2), start=(start n1), end=(end n2)})
                          where
                             hComaThenList t = do {m1 <- consumeToken Token.COMA t; m2 <- parseNodeMemberList (tokens m1); Success [] (ParseResult{node=(node m2), tokens=(tokens m2), start=(start m1), end=(end m1)})} 
                             hOptionalComma d t = do {m1 <- optional (consumeToken Token.COMA) d t; Success [] (ParseResult{node=[], tokens=(tokens m1), start=(start m1), end=(end m1)})}

parseNodeImportAlias :: [Token.Token] -> Fallible (ParseResult LeafName)
parseNodeImportAlias tok = do n1 <- parseLeafName tok
                              n2 <- consumeToken Token.EQ (tokens n1)
                              Success [] (ParseResult{node=(node n1),tokens=(tokens n2),start=(start n1),end=(end n2)})

parseNodeUseStatement :: [Token.Token] -> Fallible (ParseResult NodeUseStatement)
parseNodeUseStatement tok = do n1 <- consumeToken Token.USE tok
                               n2 <- oneOf hWithStar (hWithOptionalAlias n1) (tokens n1)
                               n3 <- consumeToken Token.SEMI (tokens n2)
                               Success [] (ParseResult{node=(node n2), tokens=(tokens n3), start=(start n1), end=(end n3)})
                            where                                
                               hWithOptionalAlias d t = do {m1 <- optional parseNodeImportAlias d t; m2 <- parseNodeIdentifier (tokens m1); m3 <- optional hMemberList m2 (tokens m2); Success [] ParseResult{node=(NodeUseStatement (node m1) (node m2) (node m3)), tokens=(tokens m3), start=(start m1), end=(end m3)}}
                               hMemberList t = do {m1 <- consumeTokens [Token.DBL_COL, Token.L_BRACE] t; m2 <- parseNodeMemberList (tokens m1); m3 <- consumeToken Token.R_BRACE (tokens m2); Success [] ParseResult{node=(NodeMemberList (node m2)), tokens=(tokens m3), start=(start m1), end=(end m3)}}  
                               hWithStar t = do {m1 <- parseNodeIdentifier t; m2 <- consumeTokens [Token.DBL_COL, Token.STAR] (tokens m1); Success [] ParseResult{node=(NodeUseStatement Nothing (node m1) (Just NodeMemberStar)), tokens=(tokens m2), start=(start m1), end=(end m2)}}

parseNodeImports :: [Token.Token] -> Fallible (ParseResult [NodeUseStatement])
parseNodeImports tok = do n1 <- parseNodeUseStatement tok
                          n2 <- oneOf parseNodeImports (nothing [] n1) (tokens n1)
                          Success [] ParseResult{node=([node n1]++(node n2)), tokens=(tokens n2), start=(start n1), end=(end n2)} 

parseNodeSubunit :: [Token.Token] -> Fallible (ParseResult NodeSubunit)
parseNodeSubunit tok = do n1 <- parseNodeImports tok
                          n2 <- parseNodeDeclarations (tokens n1)
                          n3 <- consumeToken Token.EOF (tokens n2) 
                          Success [] (ParseResult{node=(NodeSubunit (node n1) (node n2)), tokens=(tokens n2), start=(start n1), end=(end n3)})