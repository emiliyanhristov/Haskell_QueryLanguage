{ 
module RQLQGrammar where 
import RQLQTokens 
import Data.List
}

%name parseQuery
%tokentype { RQLQToken } 
%error { parseError }
%token 
  SELECT                                 {TokenSelect}
  WHERE                                  {TokenWhere}
  PRINT                                  {TokenPrint}
  IS                                     {TokenIs}
  AS                                     {TokenAs}
  AND                                    {TokenAnd}
  OR                                     {TokenOr}
  UPDATE                                 {TokenUpdate}
  CLONE                                  {TokenClone}
  BETWEEN                                {TokenBetween}
  NOT                                    {TokenNot}
  TO                                     {TokenTo}
  subject                                {TokenSubject}
  predicate                              {TokenPredicate}
  object                                 {TokenObject}
  semiColon                              {TokenSemiColon}
  comma                                  {TokenComma}
  dollar                                 {TokenDollar}
  paren                                  {TokenParen}
  lBracket                               {TokenLBracket}
  rBracket                               {TokenRBracket}
  minus                                  {TokenMinus}
  plus                                   {TokenPlus}
  true                                   {TokenTrue}
  false                                  {TokenFalse}
  int                                    {TokenInt $$}
  str                                    {TokenString $$} 

%nonassoc IS
%left AND OR
%nonassoc true false int str minus plus paren semiColon comma AS object
%%

QueList : Que                            {[$1]}
        | Que QueList                    {($1:$2)}

Que : SELECT SelectQue                   {Select $2}
    | WHERE GeneralWhereQue              {Where $2}
    | UPDATE GeneralUpdateQue            {Update $2}
    | PRINT PrintQue                     {Print $2}
    | CLONE CloneQue                     {Clone $2}

----------------------------------------------------------------------------------------------

SelectQue : str AS str                    {[($3,$1)]}
          | str AS str comma SelectQue     {(($3,$1):$5)}

----------------------------------------------------------------------------------------------
GeneralWhereQue : WhereQue AS str                            {($3,$1)}

WhereQue : NormalWhereQue                                    {NormalWhereRequest $1}
         | WhereQue OR WhereQue                              {OrWhereRequest $1 $3}
         | WhereQue AND WhereQue                             {AndWhereRequest $1 $3}

NormalWhereQue : str dollar Triplets IS Literal                                      {IsLit ($1, $3) $5}
               | str dollar Triplets IS str dollar Triplets                        {Is ($1, $3) ($5, $7)}
               | str dollar Triplets IS BETWEEN lBracket Literal comma Literal rBracket       {IsBetween ($1, $3) ($7, $9)}
               | str dollar Triplets IS NOT BETWEEN lBracket Literal comma Literal rBracket   {IsNotBetween ($1, $3) ($8, $10)}

----------------------------------------------------------------------------------------------
GeneralUpdateQue : str dollar UpdateQue     {($1, $3)}

UpdateQue : Triplets TO Literal            {NormalUpdate ($1, $3)}
          | Triplets Literal               {CalcUpdate ($1, $2)}

----------------------------------------------------------------------------------------------
PrintQue : str semiColon                 {[$1]}
         | str comma PrintQue            {($1:$3)}

----------------------------------------------------------------------------------------------
CloneQue : str AS str                 {($1, $3)}

----------------------------------------------------------------------------------------------
Literal : int                            {QInt $1}
        | minus int                      {QMinusInt $2}
        | plus int                       {QPlusInt $2}
        | paren str paren                {QString $2}
        | true                           {QBool True}
        | false                          {QBool False}

Triplets : subject                       {Subject}
         | predicate                     {Predicate}
         | object                        {Object}

{ 

parseError :: [RQLQToken] -> a
parseError ts = error ("Error on tokens: " ++ (show ts))


data Query = Select [(String, String)]
           | Where (String, WhereType)
           | Update (String, UpdateType)
           | Print [String]
           | Clone (String, String)
        deriving Show

data WhereType = NormalWhereRequest ConditionalType
               | OrWhereRequest WhereType WhereType
               | AndWhereRequest WhereType WhereType
            deriving Show

data ConditionalType = Is (String, Triplet) (String, Triplet)
                     | IsLit (String, Triplet) LiteralType
                     | IsBetween (String, Triplet) (LiteralType, LiteralType)
                     | IsNotBetween (String, Triplet) (LiteralType, LiteralType)
                deriving Show

data UpdateType = NormalUpdate (Triplet, LiteralType)
                | CalcUpdate (Triplet, LiteralType)
           deriving Show

data Triplet = Subject
             | Predicate
             | Object
           deriving Show

data LiteralType = QInt Int
                 | QMinusInt Int
                 | QPlusInt Int
                 | QString String
                 | QBool Bool
               deriving Show
} 