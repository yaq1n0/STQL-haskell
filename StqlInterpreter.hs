--module StqlInterpreter where
import StqlTokens
import StqlGrammar
import StqlEval
import Control.Exception
import System.Environment

execLoop :: [StqlExp] -> IO ()
execLoop [] = putStrLn "EOF"
execLoop (x : xs) = do exec x
                       execLoop xs

main :: IO ()
main = do (fileName : _ ) <- getArgs
          sourceText <- readFile fileName

          let _lines = filter (not . null) (lines sourceText)
          putStrLn ("Input: \n" ++ sourceText ++ "\n")

          let lexed = map alexScanTokens _lines
          putStrLn ("Lexed as: \n" ++ (show lexed) ++ "\n")

          let parsed = map parseStql lexed
          putStrLn ("Parsed as: \n" ++ (show parsed) ++ "\n")

          putStrLn "Evaluates to: "
          execLoop parsed
