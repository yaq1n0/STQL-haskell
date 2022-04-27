--module Stql where
import System.IO
import System.Process
import System.Environment

main :: IO ()
main = do (fileName : _ ) <- getArgs
          -- call cabal install and cabal build commands before executing anything
          callCommand "cabal install --lib --package-env . swish utf8-string mtl text array network-uri --allow-newer=base"
          callCommand "cabal build --allow-newer=base"

          -- alex and happy
          callCommand "alex StqlTokens.x"
          callCommand "happy StqlGrammar.y"

          -- compile interpreter and pass the fileName to it
          callCommand "ghc StqlInterpreter.hs"
          callCommand ("./StqlInterpreter " ++ fileName)
