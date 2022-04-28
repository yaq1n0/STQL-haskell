{
module StqlGrammar where
import StqlTokens
}

%name parseStql
%tokentype { Token }
%error { parseError }
%token
  new       { TokenNew }
  merge     { TokenMerge }
  print     { TokenPrint }
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
  '=='      { TokenEQ }
  '>'       { TokenGT }
  '<'       { TokenLT }
  '>='      { TokenGTE }
  '<='      { TokenLTE }
  path      { TokenPath $$ }
  var       { TokenVar $$ }

%%

Exp : new path                                      { New $2 }
    | print path                                    { Print $2 }
    | filter Cat path Comp Cat path to path         { FilterC $2 $3 $4 $5 $6 $8 }
    | filter Cat path Comp Lit var to path          { FilterL $2 $3 $4 $5 $6 $8 }
    | merge path path to path                       { Merge2 $2 $3 $5 }
    | merge path path path to path                  { Merge3 $2 $3 $4 $6}
    | setall Cat path Lit var                       { SetAll $2 $3 $4 $5}
    | incrall Cat path Lit var                      { IncrAll $2 $3 $4 $5}

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
parseError _ = error "Unknown Parse Error"

data StqlCat = SubjCat | PredCat | ObjCat deriving (Show, Eq)

data StqlLit = LitStr | LitNum | LitBool deriving (Show, Eq)

data StqlComp = CompEQ | CompGT | CompLT | CompGTE | CompLTE deriving (Show, Eq)

data StqlExp = New String | Print String
               | FilterC StqlCat String StqlComp StqlCat String String
               | FilterL StqlCat String StqlComp StqlLit String String
               | Merge2 String String String
               | Merge3 String String String String
               | SetAll StqlCat String StqlLit String
               | IncrAll StqlCat String StqlLit String
               deriving (Show, Eq)
}
