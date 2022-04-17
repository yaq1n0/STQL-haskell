# PLC
Programming Language Concepts Coursework

## How to run
```cabal install --lib --package-env . swish --allow-newer=base```
	OR ```cabal install swish —lib --allow-newer=base``` (this is shorter, but installs it globally; you, as markers, probably don’t want it on your machine permanently)__
```cabal install --lib --package-env . mtl --allow-newer=base```__
```cabal build --allow-newer=base```__
```ghc ./app/Stql.hs```__