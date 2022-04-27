{
module StqlTokens where
}

%wrapper "basic"
$alpha = [a-zA-Z0-9]

tokens :-
$white+    ;
  "--".*     ;
  NEW        { \s -> TokenNew}
  PRINT      { \s -> TokenPrint}
  MERGE      { \s -> TokenMerge}
  FILTER     { \s -> TokenFilter}
  TO         { \s -> TokenTo}
  SETALL     { \s -> TokenSetAll}
  INCRALL    { \s -> TokenIncrAll}
  SUBJ       { \s -> TokenSubj}
  PRED       { \s -> TokenPred}
  OBJ        { \s -> TokenObj}
  STR        { \s -> TokenString}
  NUM        { \s -> TokenNum}
  BOOL       { \s -> TokenBool}
  TRUE       { \s -> TokenTrue}
  FALSE      { \s -> TokenFalse}
  ==         { \s -> TokenEQ}
  \>         { \s -> TokenGT}
  \<         { \s -> TokenLT}
  \>=        { \s -> TokenGTE}
  \<=        { \s -> TokenLTE}
  $alpha+\.ttl    { \s -> TokenPath s}
  $alpha+   { \s -> TokenVar s}

{
data Token =
  TokenNew |
  TokenPrint |
  TokenMerge |
  TokenFilter |
  TokenTo |
  TokenSetAll |
  TokenIncrAll |
  TokenSubj |
  TokenPred |
  TokenObj |
  TokenString |
  TokenNum |
  TokenBool |
  TokenTrue |
  TokenFalse |
  TokenEQ |
  TokenGT |
  TokenLT |
  TokenGTE |
  TokenLTE |
  TokenPath String |
  TokenVar String
  deriving (Eq, Show)
}
