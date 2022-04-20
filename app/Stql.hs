-- to be renamed Stql.hs
import System.Process

main = do
    -- the cabal commands won't work here in the app folder; they only work in the main directory cause of the PLC.cabal file
    -- callCommand "cabal build --allow-newer=base"
    -- putStrLn "EXECUTING CABAL"
    -- callCommand "cabal install --lib --package-env . swish utf8-string mtl text array network-uri --allow-newer=base"
    -- callCommand "cabal build --allow-newer=base"
    -- putStrLn "EXECUTING GHC"
    
    callCommand "ghc Stql-Logic.hs"
    callCommand "./Stql-Logic"