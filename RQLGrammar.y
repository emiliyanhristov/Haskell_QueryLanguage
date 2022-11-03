{ 
module RQLGrammar where 
import RQLTokens 
import Data.List
}

%name parseCalc 
%tokentype { RQLToken } 
%error { parseError }
%token 
  int            { TokenInt $$} 
  str            { TokenString $$}
  true           { TokenTrue}
  false          { TokenFalse}
  comma          { TokenComma}
  colon          { TokenColon}
  base           { TokenBase $$}
  prefix         { TokenPrefix $$}
  lURIBracket    { TokenLURIBracket}
  rURIBracket    { TokenRURIBracket}
  dot            { TokenDot}
  semiColon      { TokenSemiColon}
  paren          { TokenParen}
  minus          { TokenMinus}
  plus           { TokenPlus}
  abs            { TokenAbsolute}

%left NEG 
%nonassoc true false int str lURIBracket rURIBracket minus plus paren base prefix colon semiColon dot comma
%%

ExpList : Exp                                                  { [$1]}
        | Exp ExpList                                          { ($1:$2)}

Exp : prefix str colon URIExp dot                              { Prefix $2 $4}
    | base URIExp dot                                          { Base $2}
    | URIExp URIExp ObjectExp dot                              { Triple $1 $2 $3}
    | URIExp URIExp ObjectExpList dot                          { ObjMTriple $1 $2 $3}
    | URIExp PredObjExpList dot                                { PredObjMTriple $1 $2}
    | str colon str str colon str str colon str dot            { SubPrefix $1 $3 $4 $6 $7 $9}

ObjectExpList : ObjectExp %prec NEG                            { [$1]}
              | ObjectExp comma ObjectExpList                  { ($1:$3)}

PredObjExpList : URIExp ObjectExpList %prec NEG                { [($1,$2)]}
               | URIExp ObjectExpList semiColon PredObjExpList { (($1, $2):$4)}

ObjectExp : lURIBracket abs str rURIBracket                    { AbsURI $3}
          | lURIBracket str rURIBracket                        { URI $2}
          | int                                                { Int $1}
          | plus int                                           { PlusInt $2}
          | minus int                                          { MinusInt $2}
          | true                                               { Bool True}
          | false                                              { Bool False}
          | paren str paren                                    { String $2}

URIExp : lURIBracket abs str rURIBracket                       { AbsExpr $3} 
       | lURIBracket str rURIBracket                           { URIExpr $2}

    
{ 

parseError :: [RQLToken] -> a
parseError ts = error ("Error on tokens: " ++ (show ts))

data TurtleExp = Prefix String URIExp
               | Base URIExp
               | Triple URIExp URIExp Object
               | ObjMTriple URIExp URIExp [Object]
               | PredObjMTriple URIExp [(URIExp, [Object])]
               | SubPrefix String String String String String String
          deriving Show

data Object = Int Int
            | String String
            | Bool Bool
            | MinusInt Int
            | PlusInt Int
            | URI String
            | AbsURI String
          deriving Show

data URIExp = URIExpr String
            | AbsExpr String
          deriving Show

} 
