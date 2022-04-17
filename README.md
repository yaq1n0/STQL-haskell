# PLC
Programming Language Concepts Coursework

## How to run
```cabal install --lib --package-env . swish --allow-newer=base```
	OR ```cabal install swish —lib --allow-newer=base``` (this is shorter, but installs it globally; you, as markers, probably don’t want it on your machine permanently)
```cabal install --lib --package-env . mtl --allow-newer=base```
```cabal build --allow-newer=base```
```ghc ./app/Stql.hs```