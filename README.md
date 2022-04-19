# PLC
Programming Language Concepts Coursework

## How to run
1. ```cabal build --allow-newer=base``` 
2. ```cabal install --lib --package-env . swish utf8-string mtl text array network-uri --allow-newer=base```
3. ```cabal build --allow-newer=base``` again
4. ```cd app```
5. ```ghc Stql.hs```

### Disclaimer
If something went wrong and you've run ```cabal install...``` multiple times, you should delete the ```dist-newstyle``` folder and the ```.ghc.environment...``` files that showed up as a result cause they, most likely, will be incorrect, and run the command again, once.