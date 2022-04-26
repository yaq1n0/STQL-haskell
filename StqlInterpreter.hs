module StqlInterpreter where
import StqlTokens
import StqlGrammar
import StqlEval
import Control.Exception
import System.Environment

main :: String
main = catch main' noParse

main' = do sourceText <- readFile fileName
           putStrLn ("Input: \n" ++ sourceText ++ "\n")
           let lexed = alexScanTokens sourceText
           putStrLn ("Lexed as: \n" ++ (show lexed) ++ "\n")
           let parsed = parseStql lexed
           putStrLn ("Parsed as: \n" ++ (show parsedProgram) ++ "\n")
           let result = evalLoop (parsedProgram)
           putStrLn ("Evaluates to: \n " ++ unparse result ++ "\n")

noParse :: ErrorCall -> IO ()
noParse e = do let err = show e
               hPutStr stderr err
               return ()
