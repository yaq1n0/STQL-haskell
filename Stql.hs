module Stql where
import System.IO
import System.Process

main :: IO ()
main = do (fileName : _ ) <- getArgs
           -- call cabal install and cabal build commands before executing anything
           callCommand "cabal install --lib --package-env . swish utf8-string mtl text array network-uri --allow-newer=base"
           callCommand "cabal build --allow-newer=base"

           -- compile interpreter and pass the fileName to it
           callCommand "ghc StqlInterpreter.hs"
           callCommand ("./StqlInterpreter " ++ fileName)
