module Main (main) where
import System.IO
import System.IO.Error
import qualified Lexer as L
import qualified Parser as P
import qualified AST as AST
import qualified Evaluator as E
import Codec.Binary.UTF8.String (encode)

-- REF1 (REF for reference), Book: Alex and Happy Lexers and Parsers in Haskell
output::AST.Term -> String
output t = (show t)++"\n"++(show (E.eval t))

mainREPL::IO()
mainREPL = do
  putStr "> "
  l <- tryIOError getLine
  case l of
    Left e ->
          if isEOFError e
            then putStrLn "An error occurred!"
            else ioError e     
    Right s ->
          if s/="" 
          then 
            do 
              case L.evalP P.parse (encode s) of
                Right t -> putStrLn (output t)
                Left s -> putStrLn s
              mainREPL
          else
            mainREPL

main::IO ()
main = do
  hSetBuffering stdout NoBuffering
  mainREPL
-- END OF REF1

-- REFERENCES
-- REF 1, Book: Alex and Happy Lexers and Parsers in Haskell by Jyotirmoy Bhattacharya, accessed April 2022: https://leanpub.com/alexandhappy/read#leanpub-auto-monadic-parsers-and-lexers