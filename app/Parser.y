-- Happy Parser
{
module Parser(parse) where
import AST
import qualified Lexer as L
import Control.Monad.Error
}

-- REF1 (REF for reference)
%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%name parse
%tokentype{L.Token}
%error {parseError}

%token
  true			{L.TTrue}
  false     {L.TFalse}
  IMPORT		{L.TImport}
  NEW			  {L.TNew}
  WHERE			{L.TWhere}
  ADDTO			{L.TAddTo}

%%

Term	:  true				  {STrue}
      |  false			  {SFalse}

{
parseError :: L.Token -> a
parseError _ = error "Parse Error!"
}
-- END OF REF1

-- REFERENCES
-- REF 1, Book: Alex and Happy Lexers and Parsers in Haskell by Jyotirmoy Bhattacharya, accessed April 2022: https://leanpub.com/alexandhappy/read#leanpub-auto-monadic-parsers-and-lexers