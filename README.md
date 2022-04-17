# PLC
Programming Language Concepts Coursework

## How to run
1. ```cabal install --lib --package-env . swish --allow-newer=base```
	(**OR** ```cabal install swish —lib --allow-newer=base``` (this is shorter, but installs it globally; you, as markers, probably don’t want it on your machine permanently))
2. ```cabal install --lib --package-env . mtl --allow-newer=base```
3. ```cabal build --allow-newer=base```
4. ```ghc ./app/Stql.hs```