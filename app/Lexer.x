-- Alex Lexer
{
module Lexer (Token(..),P,evalP,lexer) where
import Control.Monad.State
import Control.Monad.Error
import Data.Word
}

tokens :-
  $white+		; -- whitespace
  true			{TTrue}
  false     {TFalse}
  IMPORT		{TImport}
  NEW			  {TNew}
  WHERE			{TWhere}
  ADDTO			{TAddTo}

{
data Token = 
     TTrue
     | TFalse
     | TZero
     | TImport
     | TNew
     | TWhere
     | TAddTo
     | TEOF --end of file
     deriving (Eq,Show)

-- REF1 (REF for reference)
type AlexInput = [Word8]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (b:bs) = Just (b,bs)
alexGetByte []    = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

-- Parser monad
type P a = StateT AlexInput (Either String) a

evalP :: P a -> AlexInput -> Either String a
evalP = evalStateT

-- Action to read a token
readToken::P Token
readToken = do
          s <- get  
          case alexScan s 0 of
                AlexEOF -> return TEOF
                AlexError _ -> throwError "Lexical error occurred."
                AlexSkip inp' _ -> do	
                                    put inp'
                                    readToken
                AlexToken inp' _ tk -> do 
                                        put inp'
                                        return tk

-- The lexer function to be passed to Happy
lexer::(Token -> P a)->P a
lexer cont = do
  token <- readToken
  cont token
-- END OF REF1

}

-- {
-- main::IO ()
-- main = do
--   s <- getContents
--   let toks = alexScanTokens s
--   mapM_ putStrLn toks
-- }

-- REFERENCES
-- REF 1, Book: Alex and Happy Lexers and Parsers in Haskell by Jyotirmoy Bhattacharya, accessed April 2022: https://leanpub.com/alexandhappy/read#leanpub-auto-monadic-parsers-and-lexers