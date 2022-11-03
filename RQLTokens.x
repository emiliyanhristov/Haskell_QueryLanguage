{ 
module RQLTokens where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
 $white+                                                           ; 
 "--".*                                                            ;
 true                                                              {\s -> TokenTrue}
 false                                                             {\s -> TokenFalse}
 \,                                                                {\s -> TokenComma}
 \:                                                                {\s -> TokenColon}
 \@base                                                            {\s -> TokenBase s}
 \@prefix                                                          {\s -> TokenPrefix s}
 \<                                                                {\s -> TokenLURIBracket}
 \>                                                                {\s -> TokenRURIBracket}
 \.                                                                {\s -> TokenDot}
 \;                                                                {\s -> TokenSemiColon}
 \"                                                                {\s -> TokenParen}
 \-                                                                {\s -> TokenMinus}
 \+                                                                {\s -> TokenPlus}
 "http://"                                                         {\s -> TokenAbsolute}
 [$digit \_ \â€™\/\.\#]* $alpha [$alpha $digit \_ \â€™\/\.\#]*     {\s -> TokenString s} 
 $digit+                                                           {\s -> TokenInt (read s)} 
 
 
{ 
data RQLToken = 
  TokenTrue             |
  TokenFalse            |
  TokenComma            |
  TokenColon            |
  TokenBase String      |
  TokenPrefix String    | 
  TokenLURIBracket      |
  TokenRURIBracket      |
  TokenDot              |
  TokenSemiColon        |
  TokenParen            |
  TokenMinus            |
  TokenPlus             |
  TokenAbsolute         |
  TokenInt Int          |
  TokenString String       
    deriving (Eq, Show)   
}
      