{
module StqlTokens where
}

%wrapper "basic"
$digit = 0-9
-- digits
$alpha = [a-zA-Z]
-- alphabet

tokens :-
$white+    ;
  "//".*     ;
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
  .+\.ttl    { \s -> TokenPath (read s)}
  $alpha [$alpha $digit \_ \’]*   { \s -> TokenVar (read s)}

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
