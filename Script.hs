-- to be renamed Stql.hs
import System.Process

main = do
    callCommand "cabal build --allow-newer=base"
    putStrLn "cabal"
    callCommand "cabal install --lib --package-env . swish utf8-string mtl text array network-uri --allow-newer=base"
    callCommand "cabal build --allow-newer=base"
    putStrLn "ghc"
    callCommand "ghc Main.hs"
    callCommand "./Main"