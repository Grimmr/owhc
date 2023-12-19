module Parser where
import qualified Token

import Fallible

data ParseResult t = ParseResult {node::t, tokens::[Token.Token], start::Token.Token, end::Token.Token} deriving (Show)

data Dummy = Dummy String deriving (Show);
data NodeDummyPair = NodeDummyPair Dummy Dummy deriving (Show);
data NodeDeclaration = NodeDeclaration Dummy deriving (Show);

data LeafName = LeafName String deriving (Show);

type NodeIdentifier = [LeafName];

data NodeMember = NodeMember {memberAlias::Maybe LeafName, memberActual::LeafName} deriving (Show);
data NodeMembers = NodeMemberList [NodeMember] | NodeMemberStar deriving (Show);
data NodeUseStatement = NodeUseStatement {alias::Maybe LeafName, ident::NodeIdentifier, members::Maybe NodeMembers} deriving (Show);
data NodeSubunit = NodeSubunit [NodeUseStatement] [NodeDeclaration] deriving (Show);

checkToken :: Token.TokenType -> Token.Token -> Bool
checkToken t l = (Token.tokenType l) == t

checkTokens :: [Token.TokenType] -> [Token.Token] -> Bool
checkTokens [t] [l] = checkToken t l
checkTokens (t:ts) (l:ls) = checkToken t l && checkTokens ts ls 

consumeToken :: Token.TokenType -> [Token.Token] -> Fallible (ParseResult Token.Token);
consumeToken t (h:tail) | (Token.tokenType h) == t = Success [] ParseResult{node=h, tokens=tail, start=h, end=h};
consumeToken t (h:_) = Fail [MessageParseExpectedOneFoundAnother{expected=t, found=h}]; 

optional :: ([Token.Token] -> Fallible (ParseResult b)) -> ParseResult c -> [Token.Token] -> Fallible (ParseResult (Maybe b))
optional f p tok | Success m ParseResult{node=n, tokens=t, start=s, end=e} <- f tok = Success m (ParseResult{node=(Just n), tokens=t, start=s, end=e})
                 | Fail m <- f tok = Success [] (ParseResult{node=Nothing, tokens=tok, start=(end p), end=(end p)})

oneOf :: ([Token.Token] -> Fallible (ParseResult b)) -> ([Token.Token] -> Fallible (ParseResult b)) -> [Token.Token] -> Fallible (ParseResult b)
oneOf f g tok | Success m d <- f tok = Success m d
              | Success m d <- g tok = Success m d
              | Fail m1 <- f tok, Fail m2 <- g tok = Fail [MessageParseBothHalvesOffOneOfFailed m1 m2]

parseNodeDummy :: [Token.Token] -> Fallible (ParseResult Dummy);
parseNodeDummy tok = do n1 <- consumeToken Token.NAME tok 
                        Success [] (ParseResult{node=Dummy (Token.literal (node n1)), tokens=(tokens n1), start=(start n1), end=(end n1)})

parseNodeDeclarations :: [Token.Token] -> Fallible (ParseResult [NodeDeclaration])
parseNodeDeclarations tok = do n1 <- parseNodeDummy tok
                               Success [] ParseResult{node=[NodeDeclaration (node n1)], tokens=(tokens n1), start=(start n1), end=(end n1)}

{-Parse Leafs-}
parseLeafName :: [Token.Token] -> Fallible (ParseResult LeafName)
parseLeafName tok = do n1 <- consumeToken Token.NAME tok
                       Success [] (ParseResult{node=(LeafName (Token.literal (node n1))), tokens=(tokens n1), start=(start n1), end=(end n1)})

{-6.4-}
parseNodeIdentifier :: [Token.Token] -> Fallible (ParseResult NodeIdentifier)
parseNodeIdentifier tok@(h1:h2:_) | checkToken Token.DBL_COL h2 = do n1 <- parseLeafName tok
                                                                     n2 <- consumeToken Token.DBL_COL (tokens n1)
                                                                     n3 <- parseNodeIdentifier (tokens n2)
                                                                     Success [] (ParseResult{node=((node n1):node(n3)), tokens=(tokens n2), start=(start n1), end=(end n3)})
parseNodeIdentifier tok = do n1 <- parseLeafName tok
                             Success [] (ParseResult{node=[node (n1)], tokens=(tokens n1), start=(start n1), end=end(n1)})

{-6.12-}
parseNodeMember :: [Token.Token] -> Fallible (ParseResult NodeMember)
parseNodeMember tok = oneOf (hTwoNames) (hOneName) tok
                      where
                         hOneName t = do {m1 <- parseLeafName t; Success [] (ParseResult{node=(NodeMember{memberAlias=Nothing, memberActual=(node m1)}), tokens=(tokens m1), start=(start m1), end=(end m1)})}
                         hTwoNames t = do {m1 <- parseLeafName t; m2 <- consumeToken Token.EQ (tokens m1); m3 <- parseLeafName (tokens m2); Success [] (ParseResult{node=(NodeMember{memberAlias=(Just (node m1)), memberActual=(node m3)}), tokens=(tokens m3), start=(start m1), end=(end m3)})}
                         

parseNodeMemberList :: [Token.Token] -> Fallible (ParseResult [NodeMember])
parseNodeMemberList tok = do n1 <- parseNodeMember tok
                             n2 <- (oneOf hComaThenList (hOptionalComma n1) (tokens n1))
                             Success [] (ParseResult{node=([(node n1)]++(node n2)), tokens=(tokens n2), start=(start n1), end=(end n2)})
                          where
                             hComaThenList t = do {m1 <- consumeToken Token.COMA t; m2 <- parseNodeMemberList (tokens m1); Success [] (ParseResult{node=(node m2), tokens=(tokens m2), start=(start m1), end=(end m1)})} 
                             hOptionalComma d t = do {m1 <- optional (consumeToken Token.COMA) d t; Success [] (ParseResult{node=[], tokens=(tokens m1), start=(start m1), end=(end m1)})}

parseNodeMembers :: [Token.Token] -> Fallible (ParseResult (Maybe NodeMembers))
parseNodeMembers tok@(h1:h2:_) | checkTokens [Token.DBL_COL, Token.L_BRACE] [h1, h2]  = do n1 <- consumeToken Token.DBL_COL tok
                                                                                           n2 <- consumeToken Token.L_BRACE (tokens n1)
                                                                                           n3 <- parseNodeMemberList (tokens n2)
                                                                                           n4 <- consumeToken Token.R_BRACE (tokens n3)
                                                                                           Success [] (ParseResult{node=(Just (NodeMemberList (node n3))), tokens=(tokens n4), start=(start n1), end=(end n4)})
parseNodeMembers tok@(h1:h2:_) | checkTokens [Token.DBL_COL, Token.STAR] [h1, h2]  = do n1 <- consumeToken Token.DBL_COL tok
                                                                                        n2 <- consumeToken Token.STAR (tokens n1) 
                                                                                        Success [] (ParseResult{node=(Just (NodeMemberStar)), tokens=(tokens n2), start=(start n1), end=(end n2)})
parseNodeMembers tok@(h1:_) | checkToken Token.DBL_COL h1 = Fail [MessageParseUseStatementDblColButNoMembers h1] 
parseNodeMembers tok = Success [] (ParseResult{node=Nothing, tokens=tok})

parseNodeImportAlias :: [Token.Token] -> Fallible (ParseResult (Maybe LeafName))
parseNodeImportAlias tok@(h1:h2:_) | checkTokens [Token.NAME, Token.EQ] [h1,h2] = do n1 <- parseLeafName tok
                                                                                     n2 <- consumeToken Token.EQ (tokens n1)
                                                                                     Success [] (ParseResult{node=(Just (node n1)),tokens=(tokens n2),start=(start n1),end=(end n2)})
parseNodeImportAlias tok = Success [] (ParseResult{node=Nothing, tokens=tok})


valdiateNodeUseStatement (Success m (ParseResult{node=(NodeUseStatement{alias=Just _, members=Just NodeMemberStar}), tokens=_, start=s})) = Fail (m++[MessageParseUseStatementHasAliasAndStar s])
validateNodeUseStatement a = a
parseNodeUseStatement :: [Token.Token] -> Fallible (ParseResult NodeUseStatement)
parseNodeUseStatement tok = validateNodeUseStatement (do n1 <- consumeToken Token.USE tok
                                                         n2 <- parseNodeImportAlias (tokens n1)
                                                         n3 <- parseNodeIdentifier (tokens n2)
                                                         n4 <- parseNodeMembers (tokens n3)
                                                         n5 <- consumeToken Token.SEMI (tokens n4)
                                                         Success [] (ParseResult{node=(NodeUseStatement (node n2) (node n3) (node n4)), tokens=(tokens n5), start=(start n1), end=(end n5)}))

parseNodeImports :: [Token.Token] -> Fallible (ParseResult [NodeUseStatement])
parseNodeImports tok@(h:_) | (Token.tokenType h) == Token.USE = do n1 <- parseNodeUseStatement tok
                                                                   n2 <- parseNodeImports (tokens n1)
                                                                   Success [] (ParseResult{node=(node n1):(node n2), tokens=(tokens n2), start=(start n1), end=(end n2)}) 
parseNodeImports tok = Success [] (ParseResult{node=[], tokens=tok})

parseNodeSubunit :: [Token.Token] -> Fallible (ParseResult NodeSubunit)
parseNodeSubunit tok = do n1 <- parseNodeImports tok
                          n2 <- parseNodeDeclarations (tokens n1)
                          n3 <- consumeToken Token.EOF (tokens n2) 
                          Success [] (ParseResult{node=(NodeSubunit (node n1) (node n2)), tokens=(tokens n2), start=(start n1), end=(end n3)})