--module StqlInterpreter where
import StqlTokens
import StqlGrammar
import StqlEval
import Control.Exception
import System.Environment

recurse [] = putStrLn "EOF"
recurse (l : ls) = if (length ls > 0) then exec l
                   else recurse ls

main :: IO()
main = do (fileName : _ ) <- getArgs
          sourceText <- readFile fileName

          let _lines = filter (not . null) (lines sourceText)
          putStrLn ("Input: \n" ++ sourceText ++ "\n")

          let lexed = map alexScanTokens _lines
          putStrLn ("Lexed as: \n" ++ (show lexed) ++ "\n")

          let parsed = map parseStql lexed
          putStrLn ("Parsed as: \n" ++ (show parsed) ++ "\n")

          recurse parsed
