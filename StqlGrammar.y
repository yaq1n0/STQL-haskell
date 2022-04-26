{
module StqlGrammar where
import Lexer
}

%name parseStql
%tokentype { Token }
%error { parseError }
%token
  new       { TokenNew}
  merge     { TokenMerge }
  filter    { TokenFilter }
  to        { TokenTo }
  setall    { TokenSetAll }
  incrall   { TokenIncrAll }
  subj      { TokenSubj }
  pred      { TokenPred }
  obj       { TokenObj }
  str       { TokenString }
  num       { TokenNum }
  bool      { TokenBool }
  true      { TokenTrue }
  false     { TokenFalse }
  '=='      { TokenEQ }
  '>'       { TokenGT }
  '<'       { TokenLT }
  '>='      { TokenGTE }
  '<='      { TokenLTE }
  path      { TokenPath $$ }
  var       { TokenVar $$ }

%left 'and' 'or'
%%

Exp : new path                                      { New $2 }
    | filter Cat path Comp Cat path to path         { Filter $2 $3 $4 $5 $6 $8 }
    | filter Cat path Comp Lit var to path         { Filter $2 $3 $4 $5 $6 $8 }
    | merge path path to path                          { Merge2 $2 $3 $5 }
    | merge path path path to path                      { Merge3 $2 $3 $4 $6}
    | setall Cat path Lit var                        { SetAll $2 $3 $4 $5}
    | incrall obj path num var                       { IncrAll $2 $3 $4 $5}

Comp : '==' { CompEQ }
     | '>'  { CompGT }
     | '<'  { CompLT }
     | '>=' { CompGTE }
     | '<=' { CompLTE }

Lit : str  { LitStr }
     | num  { LitNum }
     | bool { LitBool }

Cat : subj { SubjCat }
    | pred { PredCat }
    | obj  { ObjCat }

{
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error"

data StqlCat = SubjCat | PredCat | ObjCat deriving (Show, Eq)

data StqlLit = LitStr | LitNum | LitBool deriving (Show, Eq)

data StqlComp = CompEQ | CompGT | CompLT | CompGTE | CompGTE deriving (Show, Eq)

data StqlExp = New String | Let String String
               | Filter StqlCat String StqlComp StqlCat String String
               | Filter StqlCat String StqlComp StqlLit String String
               | Merge2 String String String
               | Merge3 String String String String
               | SetAll StqlCat String StqlLit String
               | IncrAll ObjCat Strig LitNum String
}
