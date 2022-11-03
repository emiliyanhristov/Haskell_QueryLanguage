{ 
module RQLQTokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
 $white+                               ; 
 "--".*                                ;
SELECT                                 {\s -> TokenSelect}
WHERE                                  {\s -> TokenWhere}
PRINT                                  {\s -> TokenPrint}
IS                                     {\s -> TokenIs}
AS                                     {\s -> TokenAs}
AND                                    {\s -> TokenAnd}
OR                                     {\s -> TokenOr}
UPDATE                                 {\s -> TokenUpdate}
CLONE                                  {\s -> TokenClone}
BETWEEN                                {\s -> TokenBetween}
NOT                                    {\s -> TokenNot}
TO                                     {\s -> TokenTo}
subject                                {\s -> TokenSubject}
predicate                              {\s -> TokenPredicate}
object                                 {\s -> TokenObject}
\;                                     {\s -> TokenSemiColon}
\,                                     {\s -> TokenComma}
\$                                     {\s -> TokenDollar}
\"                                     {\s -> TokenParen}
\(                                     {\s -> TokenLBracket}
\)                                     {\s -> TokenRBracket}
\-                                     {\s -> TokenMinus}
\+                                     {\s -> TokenPlus}
true                                   {\s -> TokenTrue}
false                                  {\s -> TokenFalse}
$digit+                                {\s -> TokenInt (read s)}
$alpha [$alpha $digit \_ \â€™\/\.\#\:]*  {\s -> TokenString s} 
 
{ 
data RQLQToken = 
  TokenSelect          |
  TokenWhere           |
  TokenPrint           |
  TokenIs              |
  TokenAs              |
  TokenAnd             |
  TokenOr              |
  TokenUpdate          |
  TokenClone           |
  TokenBetween         |
  TokenNot             |
  TokenTo              |
  TokenSubject         |
  TokenPredicate       |
  TokenObject          |
  TokenSemiColon       |
  TokenComma           |
  TokenDollar          |
  TokenParen           |
  TokenLBracket        |
  TokenRBracket        |
  TokenMinus           |
  TokenPlus            |
  TokenTrue            |
  TokenFalse           |
  TokenInt Int         |
  TokenString String
    deriving (Eq, Show)   
}