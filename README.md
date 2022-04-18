# PLC
Programming Language Concepts Coursework

## How to run
1. ```cabal build --allow-newer=base``` 
2. ```cabal install --lib --package-env . swish utf8-string mtl text array network-uri --allow-newer=base```
	(**OR** ```cabal install swish utf8-string mtl text array —lib --allow-newer=base``` (this is shorter, but installs it globally; you, as markers, probably don’t want it on your machine permanently))
3. ```cabal build --allow-newer=base``` again
4. ```cd app```
4. ```ghc Stql.hs```

### Disclaimer
If something went wrong and you've run ```cabal install...``` multiple times, you should delete the ```dist-newstyle``` folder and the ```.ghc.environment...``` files that showed up as a result cause they, most likely, will be incorrect, and run the command again, once.