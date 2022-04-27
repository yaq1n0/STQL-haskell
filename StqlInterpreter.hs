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

          {-
          recurse (lines sourceText)
          let exprs = lines sourceText
          let tokens = map alexScanTokens exprs
          putStrLn (show tokens)
          -}

          exec (parseStql (alexScanTokens "NEW out1.ttl"))
          {-
          putStrLn ("Input: \n" ++ sourceText ++ "\n")
          let lexed =  map alexScanTokens (lines sourceText)
          putStrLn ("Lexed as: \n" ++ "\n")
          let parsed = map parseStql lexed
          putStrLn ("Parsed as: \n" ++ "\n")
          let result = recurse parsed
          putStrLn ("Evaluates to: \n " ++ "\n")
          -}
