# PLC
Programming Language Concepts Coursework

## How to run
1. ```cabal install --lib --package-env . mtl swish utf8-string --allow-newer=base```
	(**OR** ```cabal install swish utf8-string mtl text array —lib --allow-newer=base``` (this is shorter, but installs it globally; you, as markers, probably don’t want it on your machine permanently))
2. ```cabal build --allow-newer=base```
3. ```ghc ./app/Stql.hs```