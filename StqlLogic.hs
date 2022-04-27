{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module StqlLogic where
-- IMPORTS
-- HASKELL BASE IMPORTS
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)
import qualified Data.Set as S (Set, filter, size, elemAt, toList, fromList)
-- SWISH IMPORTS
import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle, parseTurtlefromText)
-- import Swish.Monad (SwishState, SwishStatus, SwishStateIO, emptyState, setFormat, setInfo, SwishFormat (Turtle), NamedGraphMap, format, base, graph, graphs, rules, rulesets, infomsg, errormsg, exitcode)
import Swish.RDF.Graph (RDFGraph, RDFLabel(Res), NamespaceMap, NSGraph, Arc, Selector, ToRDFLabel, RDFTriple, fromRDFTriple, setNamespaces, addArc, merge, toRDFGraph, fmapNSGraph, traverseNSGraph, arc, emptyRDFGraph, toRDFLabel, nodes, getNamespaces, extract, arcSubj, arcPred, arcObj, allLabels, fromRDFLabel, update, isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral, isDatatyped, isUri, getLiteralText, getScopedName, remapLabels, labels, emptyNamespaceMap)
import Swish.RDF.Formatter.Turtle (formatGraphAsText, formatGraphAsLazyText, formatGraphIndent, formatGraphAsBuilder)
import Swish.Commands (swishInput)
import Swish.QName (QName, getQNameURI, getNamespace, getLocalName, getQNameURI)
import Swish.RDF.Ruleset (RDFRuleMap, RDFRulesetMap)
import Swish.GraphClass (ArcSet, LDGraph, setArcs, getArcs, arcLabels, arcToTriple)
import Swish.Namespace (Namespace, ScopedName, getScopeNamespace, makeURIScopedName, getScopeLocal, getScopePrefix, getScopeURI, getQName, getScopedNameURI)
  -- MTL IMPORTS
import Control.Monad.State as CMS (get, put, runStateT, evalStateT, StateT, runState)
import Control.Monad.State.Lazy (get, return, liftIO, execState, forever)
import Control.Monad.Reader.Class (ask)
  -- TEXT IMPORTS
import qualified Data.Text.Lazy as TL (Text, pack, unpack, toStrict)
import qualified Data.Text as T (Text, pack, unpack, stripPrefix, stripSuffix)
import Data.List (nub, (\\), sort, intersperse, dropWhileEnd)
import Data.Char (isSpace)
  -- NETWORK IMPORTS
import Network.URI (URI, parseURI)

data Category = Subj | Pred | Obj
data Combinator = And | Or
-- direction of filtering a graph's triples' objects with two numbered literals
data Direction = In | Out

-- new substituting triple
-- we don't know the types of the triple's elements, but we know they belong to the same class; the existential type packs all those class types into one
type SubTriple = (Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple)

-- REF1
-- wrap LabelType in an existential type
data LabelTypeSubTriple = forall a. LabelType a => LabelTypeSubTriple a | Incr Integer
-- END OF REF1

type LabelTypeTuple = (Category, RDFLabel)
-- data LabelType a = URI a | String a | Int a | Bool a
class    LabelType a      where valToLabel :: a -> RDFLabel
instance LabelType Bool   where valToLabel = toRDFLabel
instance LabelType URI    where valToLabel = toRDFLabel
instance LabelType String where valToLabel = strToLabel
instance LabelType Int where valToLabel = toRDFLabel
instance LabelType Integer where valToLabel = toRDFLabel
-- instance (Num a, Eq a, ToRDFLabel a) => LabelType a where valToLabel = toRDFLabel
-- instance LabelType Num Int where valToLabel = toRDFLabel

-- instance LabelType Double where valToLabel = toRDFLabel
-- instance LabelType Float where valToLabel = toRDFLabel

main :: IO ()
main = putStrLn "works!"

---------- PROBLEMS ----------
  -- SETUP
files :: (FilePath, FilePath, FilePath, FilePath)
files = ("bar.ttl", "foo.ttl", "fooProb5.ttl", "out.ttl")

  -- P1
printP1 :: IO ()
printP1 = printWrapper "PROBLEM 1" execP1

execP1 :: IO ()
execP1 = do
        let (file, file', _, out) = files
        _unwrap2 mergeGraphs file file' out
        printFile out

  -- P2
printP2 :: IO ()
printP2 = printWrapper "PROBLEM 2" execP2

execP2 :: IO ()
execP2 = do
        let (_, file', _, out) = files
        let filters = p2Test
        _unwrap1 (filterMultiple filters And) file' out
        printFile out

p2Test :: [LabelTypeTuple]
p2Test = do
        let problem2Subj = valToLabel "http://www.cw.org/#problem2"
        let problem2Obj = valToLabel True
        let filters = [(Subj, problem2Subj), (Obj, problem2Obj)]
        filters

printP2Extended :: IO ()
printP2Extended = printWrapper "PROBLEM 2" $ do
                      let (_, file', _, _) = files
                      contents <- getFileContents file'
                      let g = graphFromFileContents contents

                      let fs = p2Test
                      putStrLn "1)"
                      let graphFil1 = filterBySubj (snd $ head fs) g
                      printGraph graphFil1
                      putStrLn "2)"
                      let graphFil2 = filterByObj (snd $ last fs) g
                      printGraph graphFil2
                      putStrLn "Combined)"
                      let combined = filterMultiple fs And g
                      printGraph combined

  -- P3
printP3 :: IO ()
printP3 = printWrapper "PROBLEM 3" execP3

execP3 :: IO ()
execP3 = do
        let (_, file', _, out) = files
        let filters = p3Test
        _unwrap1 (filterMultiple filters Or) file' out
        printFile out

p3Test :: [LabelTypeTuple]
p3Test = do
            let problem3Pred1 = valToLabel "http://www.cw.org/problem3/#predicate1"
            let problem3Pred2 = valToLabel "http://www.cw.org/problem3/#predicate2"
            let problem3Pred3 = valToLabel "http://www.cw.org/problem3/#predicate3"
            let filters = [(Pred, problem3Pred1), (Pred, problem3Pred2), (Pred, problem3Pred3)]
            filters

printP3Extended :: RDFGraph -> IO ()
printP3Extended g = printWrapper "PROBLEM 3" $ do
                    let fs = p3Test
                    putStrLn "\nPROBLEM 3"
                    putStrLn "1)"
                    printGraph $ filterByPred (snd $ head fs) g
                    putStrLn "2)"
                    printGraph $ filterByPred (snd $ (fs !! 1)) g
                    putStrLn "3)"
                    printGraph $ filterByPred (snd $ last fs) g

  -- P4

printP4 :: IO ()
printP4 = printWrapper "PROBLEM 4" execP4

execP4 :: IO ()
execP4 = do
        let (file, file', _, out) = files
        _unwrap2 (compareGraphs Subj Obj) file file' out
        printFile out

  -- P5
printP5 :: IO ()
printP5 = printWrapper "PROBLEM 5" execP5

execP5 :: IO ()
execP5 = do
        let (_, _, file'', out) = files
        let (foo1, foo2, foo3) = ("foo1.ttl", "foo2.ttl", "foo3.ttl")
        let (triple, triple', triple'') = p5Test
        _unwrap1 (editGraphs Out 0 99 triple) file'' foo1
        _unwrap1 (editGraphs In 0 99 triple') file'' foo2
        _unwrap1 (editGraphs In 0 99 triple'') file'' foo3
        _unwrap3 mergeMultiple foo1 foo2 foo3 out
        printFile out

p5Test
  :: ((Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple),
      (Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple),
      (Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple))
p5Test = do
            let triple = (Nothing, Just (LabelTypeSubTriple "http://www.cw.org/problem5/#inRange"), Just (LabelTypeSubTriple False))
            let triple' = (Nothing, Nothing, Just (Incr 1))
            let triple'' = (Nothing, Just (LabelTypeSubTriple "http://www.cw.org/problem5/#inRange"), Just (LabelTypeSubTriple True))
            let triples = (triple, triple', triple'')
            triples
------------------------------

------- STRINGIFYING GRAPHS -------
-- get file contents from a file
getFileContents :: FilePath -> IO String
getFileContents filepath = readFile filepath

-- created an expanded and cleaned graph from file contents
graphFromFileContents :: String -> RDFGraph
graphFromFileContents contents = do
      let orgGraph = getGraph contents
      let expanded = expandTriples orgGraph
      let graph = cleanNamespace expanded
      graph

graphToFile :: RDFGraph -> FilePath -> IO ()
graphToFile g filepath = writeFile filepath (graphFormatOut g)

-- print a fully expanded graph
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn $ graphFormatOut g

-- print graph with indentations and predicate/object lists
printGraphIndented :: RDFGraph -> IO ()
printGraphIndented g = putStrLn $ graphFormatOut g

printFile :: FilePath -> IO ()
printFile f = do
              contents <- getFileContents f
              putStrLn contents

-- executes an RDFGraph function on one file, producing an out file
_unwrap1 :: (RDFGraph -> RDFGraph) -> FilePath -> FilePath -> IO ()
_unwrap1 function filepath out = do
                              c <- getFileContents filepath
                              let g = graphFromFileContents c
                              let final = function g
                              graphToFile final out

-- executes an RDFGraph function on two files, producing an out file
_unwrap2 :: (RDFGraph -> RDFGraph -> RDFGraph) -> FilePath -> FilePath -> FilePath -> IO ()
_unwrap2 function filepath filepath' out = do
                              c <- getFileContents filepath
                              c' <- getFileContents filepath'
                              let g = graphFromFileContents c
                              let g' = graphFromFileContents c'
                              let final = function g g'
                              graphToFile final out

-- executes an RDFGraph function on three files, producing an out file
-- only handles functions that accept a list of graphs
_unwrap3 :: ([RDFGraph] -> RDFGraph) -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
_unwrap3 function filepath filepath' filepath'' out = do
                              c <- getFileContents filepath
                              c' <- getFileContents filepath'
                              c'' <- getFileContents filepath''
                              let g = graphFromFileContents c
                              let g' = graphFromFileContents c'
                              let g'' = graphFromFileContents c''
                              let graphs = [g, g', g'']
                              let final = function graphs
                              graphToFile final out

{-
_unwrap11 :: ((RDFGraph -> RDFGraph) -> (RDFGraph -> RDFGraph) -> RDFGraph) -> FilePath -> FilePath -> FilePath -> IO ()
_unwrap11 function filepath filepath' out = do
                              contents <- readFile filepath
                              contents' <- readFile filepath'
                              let g = graphFromFile contents
                              let g' = graphFromFile contents'
                              let final = function g g'


_unwrap21 :: ((RDFGraph -> RDFGraph -> RDFGraph) -> (RDFGraph -> RDFGraph) -> RDFGraph) -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
_unwrap21 function filepath filepath' filepath'' out = do
                              contents <- readFile filePath
                              contents' <- readFile filepath'
                              contents'' <- readFile filepath''
                              let g = graphFromFile contents
                              let g' = graphFromFile contents'
                              let g'' = graphFromFile contents''
                              let final = function g g' g''

_unwrap22 :: ((RDFGraph -> RDFGraph -> RDFGraph) -> (RDFGraph -> RDFGraph -> RDFGraph) -> RDFGraph) -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
_unwrap22 function filepath filepath' filepath'' filepath''' out = do
                              contents <- readFile filepath
                              contents' <- readFile filepath'
                              contents'' <- readFile filepath''
                              contents''' <- readFile filepath'''
                              let g = graphFromFile contents
                              let g' = graphFromFile contents'
                              let g'' = graphFromFile contents''
                              let g''' = graphFromFile contents'''
                              let final = function g g' g'' g'''
                              graphToFile final out
-}

graphFormatOut :: RDFGraph -> String
graphFormatOut g = trim $ unlines recheckForDuplicates
        where
          recheckForDuplicates = nub triplesStringified
          triplesStringified = [(tripleToOut triple) ++ " ." | triple <- sorted]
          sorted = sort $ triples g

-- REF2
-- trim whitespace from end of string
trim :: [Char] -> [Char]
trim = dropWhileEnd isSpace . dropWhile isSpace
-- END OF REF2

tripleToOut :: (RDFLabel, RDFLabel, RDFLabel) -> String
tripleToOut (lb, lb', lb'') = lbToOut lb ++ lbToOut lb' ++ lbToOut lb''

lbToOut :: RDFLabel -> String
lbToOut lb = case isUri lb of
              True -> show lb
              False -> case fromRDFLabel lb :: Maybe Integer of
                            Just x -> " " ++ show x
                            Nothing -> case (show lb == "true" || show lb == "false") of
                                            True -> " " ++ show lb
                                            False -> show lb

triples :: RDFGraph -> [(RDFLabel, RDFLabel, RDFLabel)]
triples g = [(arcSubj arc, arcPred arc, arcObj arc) | arc <- S.toList $ getArcs g]
------------------------------

------- EDITING GRAPHS -------
-- substitutes graph triples with a provided triple based on restrictions
editGraphs :: Direction -> Integer -> Integer -> SubTriple -> RDFGraph -> RDFGraph
editGraphs dir startInt endInt triple g = substituted
                                            -- putStrLn "FILTERED"
                                            -- printGraph filteredGraph
                                            -- putStrLn "ARCS TO SUB"
                                            -- print arcsToSub
                                            -- putStrLn "SUBSTITUTED"
                                            -- printGraph substituted
                          where
                            -- graph to substitute, filtered arcs by constraints
                            filteredGraph = handleFilterRanges dir startInt endInt g
                            -- arcs of the graph to substitute
                            arcsToSub = S.toList $ getArcs filteredGraph
                            -- do the substitution
                            substituted = substituteGraph arcsToSub filteredGraph triple

editFullGraphs :: SubTriple -> RDFGraph -> RDFGraph
editFullGraphs triple g = substituteGraph (S.toList $ getArcs g) g triple

substituteGraph :: [Arc RDFLabel] -> RDFGraph -> SubTriple -> RDFGraph
substituteGraph [] g triple = g
substituteGraph arcs@(a:as) g triple = substituteGraph as (substituteArc a g triple) triple

substituteArc :: Arc RDFLabel -> RDFGraph -> SubTriple -> RDFGraph
substituteArc arcToSub graph triple = do
                              let strippedGraph = removeRangeArc arcToSub graph
                              addSubTripleArc triple arcToSub strippedGraph

-- add new triple to the graph, substituting the previous one
addSubTripleArc :: SubTriple -> Arc RDFLabel -> RDFGraph -> RDFGraph
addSubTripleArc (t, t', t'') arcToSub g = addGraphArc (arc a a' a'') g
                                       where
                                          -- evaluate what values to put in the new arc
                                          a = arcLbFromSubTriple t (Subj) arcToSub
                                          a' = arcLbFromSubTriple t' (Pred) arcToSub
                                          a'' = arcLbFromSubTriple t'' (Obj) arcToSub

arcLbFromSubTriple :: Maybe LabelTypeSubTriple -> Category -> Arc RDFLabel -> RDFLabel
-- if nothing has changed, return the appropriate value from the previous, substituted arc
arcLbFromSubTriple (Nothing) catg subbed = getCategoryLabel catg subbed
-- return the new value of type LabelType
arcLbFromSubTriple (Just (LabelTypeSubTriple x)) catg subbed = valToLabel x
-- increment arc object by the specified amount
arcLbFromSubTriple (Just (Incr i)) catg subbed = valToLabel (prevInt + i)
                    where prevInt = intValue (getCategoryLabel catg subbed) "arcLbFromSubTriple"

addGraphArc :: Arc RDFLabel -> RDFGraph -> RDFGraph
addGraphArc arc graph = addArc arc graph

removeRangeArc :: Arc RDFLabel -> RDFGraph -> RDFGraph
removeRangeArc toDel graph = extract s graph
                      where
                        s arc = arc /= toDel
------------------------------

--------- COMPARING ----------
compareGraphs :: Category -> Category -> RDFGraph -> RDFGraph -> RDFGraph
compareGraphs catg catg' g g' = merged
                      where
                        -- merge all the filtered graphs
                        merged = mergeMultiple filtered
                        filtered = tuplesToList filteredTupled
                        -- filter both graphs by the matches between those graphs' lists of (subj OR pred OR obj)
                        filteredTupled = [(filterWithLabelTypes catg d g, filterWithLabelTypes catg' d g') | d <- duplicates firstCatgs sndCatgs]
                        -- all the matches between the (subj OR pred OR obj) lists of both graphs
                        duplicates xs ys = filter (\x -> (x `elem` ys)) xs
                        -- all (subj OR pred OR obj) of the first and second graphs, respectively
                        firstCatgs = [getCategoryLabel catg arcg | arcg <- S.toList $ getArcs g]
                        sndCatgs = [getCategoryLabel catg' arcg' | arcg' <- S.toList $ getArcs g']

{-
compareFullGraphs :: RDFGraph -> RDFGraph -> RDFGraph
compareFullGraphs g g' = merged
                      where
                        -- merge all the filtered graphs
                        merged = mergeMultiple filtered
                        filtered = tuplesToList filteredTupled
                        -- filter both graphs by the matches between those graphs' lists of (subj OR pred OR obj)
                        filteredTupled = [(filterByAll d g, filterByAll d g') | d <- duplicates firstCatgs sndCatgs]
                        -- all the matches between the (subj OR pred OR obj) lists of both graphs
                        duplicates xs ys = filter (\x -> (x `elem` ys)) xs
                        -- all (subj OR pred OR obj) of the first and second graphs, respectively
                        firstCatgs = [arcg | arcg <- S.toList $ getArcs g]
                        sndCatgs = [arcg' | arcg' <- S.toList $ getArcs g']
-}

mergeMultiple :: [RDFGraph] -> RDFGraph
mergeMultiple [] = createGraph
mergeMultiple (x:xs) = mergeGraphs x (mergeMultiple xs)

tuplesToList :: [(a, a)] -> [a]
tuplesToList ((a, b) : cs) = a : b : tuplesToList cs
tuplesToList _ = []
------------------------------

--- MANIPULATING THE GRAPH ---
createGraph :: RDFGraph
createGraph = emptyRDFGraph

getGraphs :: [String] -> [RDFGraph]
getGraphs gs = [getGraph g | g <- gs]

-- clean namespaces of graphs
cleanGraphs :: [RDFGraph] -> [RDFGraph]
cleanGraphs gs = [cleanNamespace g | g <- gs]

-- expand the triples of graphs
expandGraphs :: [RDFGraph] -> [RDFGraph]
expandGraphs gs = [expandTriples g | g <- gs]

cleanNamespace :: RDFGraph -> RDFGraph
cleanNamespace g = setNamespaces emptyNamespaceMap g

getGraph :: String -> RDFGraph
getGraph contents = case (parseIntoTurtle contents (getBase contents)) of
                    Left err -> error "Can't parse the file."
                    Right rdfGraph -> rdfGraph

-- parse input into the turtle format
parseIntoTurtle :: String -> Maybe URI -> ParseResult
parseIntoTurtle foo base = parseTurtle (strToLText foo) base

-- get the base value from a turtle file
getBase :: String -> Maybe URI
getBase foo = do
            let prefix = "@base <"
            let suffix = "> ."

            let bases = Prelude.filter (isBase prefix suffix) $ lines foo

            case bases of
              [] -> Nothing
              [a] -> Just (toURI $ extractBase a prefix suffix)
              (a:as) -> error "The document contains too many @base properties (more than one)"
      -- checks whether the provided text is a base LabelTypeerty
      where isBase prefix suffix c = prefix `isPrefixOf` c && suffix `isSuffixOf` c
            toURI a = fromJust $ parseURI a

-- extract the actual base string
extractBase :: String -> String -> String -> String
extractBase base prefix suffix = do
            let maybesP = T.stripPrefix (strToText prefix) (strToText base)
            let sP = fromJust maybesP
            let maybesS = T.stripSuffix (strToText suffix) sP
            let sS = fromJust maybesS
            textToStr sS

getCategoryLabel :: Category -> Arc RDFLabel -> RDFLabel
getCategoryLabel (Subj) arc = arcSubj arc
getCategoryLabel (Pred) arc = arcPred arc
getCategoryLabel (Obj) arc = arcObj arc

allowURIOnly :: RDFLabel -> RDFLabel
allowURIOnly lb = case isUri lb of
                    False -> error "The label is not a URI."
                    True -> lb

rdfLabelToURI :: RDFLabel -> URI
rdfLabelToURI lb = getQNameURI $ fromJust $ fromRDFLabel lb

-- expands triples
qnameToURILabel :: QName -> RDFLabel
qnameToURILabel lb = toRDFLabel $ getQNameURI lb

strToLabel :: String -> RDFLabel
strToLabel s = case parseURI s of
                    Nothing -> toRDFLabel s
                    Just x -> toRDFLabel x

intValue :: RDFLabel -> String -> Integer
intValue lb errorTrace = case fromRDFLabel lb of
                  Nothing -> error ("The label provided in range isn't a number. ErrorTrace: " ++ errorTrace)
                  Just i -> i

mergeGraphs :: RDFGraph -> RDFGraph -> RDFGraph
mergeGraphs g g' = merge g g'
------------------------------

--------- EXPANDING ----------
expandTriples :: NSGraph RDFLabel -> NSGraph RDFLabel
expandTriples graph = fmapNSGraph convert graph
    where
      -- QName = Namespace + Local Name
      -- literals can be strings, numbers, or true/false
      convert a = case (fromRDFLabel a :: Maybe QName) of
                            Nothing -> convertToLiteral a graph
                            -- get qnames of labels inside the graph
                            Just x -> qnameToURILabel x

-- check if the value is an object. If so, it can be a literal or URI.
convertToLiteral :: RDFLabel -> NSGraph RDFLabel -> RDFLabel
convertToLiteral lb graph = case isLbInGraphObjects lb graph of
                        True -> case (fromRDFLabel lb) of
                                      Nothing -> error "Couldn't parse literal."
                                      Just x -> x
                        False -> error "The provided arc label is not a URI, but can't be a literal. Only object labels can be converted to a literal."

isLbInGraphObjects :: RDFLabel -> NSGraph RDFLabel -> Bool
isLbInGraphObjects match graph = or [match == arcObj arc | arc <- S.toList $ getArcs graph]
------------------------------

--------- FILTERING ----------
filterBySubj :: RDFLabel -> RDFGraph -> RDFGraph
filterBySubj subj graph = extract s graph
            where s arc = arcSubj arc == allowURIOnly subj

filterByPred :: RDFLabel -> RDFGraph -> RDFGraph
filterByPred pred graph = extract s graph
            where s arc = arcPred arc == allowURIOnly pred

filterByObj :: RDFLabel -> RDFGraph -> RDFGraph
filterByObj obj graph = extract s graph
            where s arc = arcObj arc == toRDFLabel obj

{-
filterByAll :: Arc RDFLabel -> RDFGraph -> RDFGraph
filterByAll (subj, pred, obj) graph = extract s graph
            where s arc = arcSubj arc == allowURIOnly subj && arcPred arc == allowURIOnly pred && arcObj arc == toRDFLabel obj
-}

filterRangesIn :: Integer -> Integer -> RDFGraph -> RDFGraph
filterRangesIn n n' graph = extract s graph
            where
              val arc = (intValue (arcObj arc) "filterRangesIn first")
              rangeVal i = (intValue (valToLabel i) "filterRangesIn second")
              s arc = val arc >= rangeVal n && val arc <= rangeVal n'

filterRangesOut :: Integer -> Integer -> RDFGraph -> RDFGraph
filterRangesOut n n' graph = extract s graph
            where
              val arc = (intValue (arcObj arc) "filterRangesOut first")
              rangeVal i = (intValue (valToLabel i) "filterRangesOut second")
              s arc = val arc < rangeVal n || val arc > rangeVal n'

filterLTGT :: Integer -> (Bool, Bool) -> RDFGraph -> RDFGraph
filterLTGT n (False, False) graph = extract s graph
            where
              val arc = (intValue (arcObj arc) "filterRangesLT first")
              rangeVal i = (intValue (valToLabel i) "filterRangesLT second")
              s arc = val arc < rangeVal n
filterLTGT n (False, True) graph = extract s graph
            where
              val arc = (intValue (arcObj arc) "filterRangesLT first")
              rangeVal i = (intValue (valToLabel i) "filterRangesLT second")
              s arc = val arc <= rangeVal n
filterLTGT n (True, False) graph = extract s graph
            where
              val arc = (intValue (arcObj arc) "filterRangesLT first")
              rangeVal i = (intValue (valToLabel i) "filterRangesLT second")
              s arc = val arc > rangeVal n
filterLTGT n (True, True) graph = extract s graph
            where
              val arc = (intValue (arcObj arc) "filterRangesLT first")
              rangeVal i = (intValue (valToLabel i) "filterRangesLT second")
              s arc = val arc >= rangeVal n

handleFilterRanges :: Direction -> Integer -> Integer -> RDFGraph -> RDFGraph
handleFilterRanges (In) n n' g = filterRangesIn n n' g
handleFilterRanges (Out) n n' g = filterRangesOut n n' g

filterMultiple :: [LabelTypeTuple] -> Combinator -> RDFGraph -> RDFGraph
filterMultiple filters c g = toRDFGraph $ S.fromList $ filterIterateGraphs c filteredGraphs
                  where
                    filteredGraphs = [filterWithLabelTypes labelCat lb g | (labelCat, lb) <- filters]

filterWithLabelTypes :: Category -> RDFLabel -> RDFGraph -> RDFGraph
filterWithLabelTypes (Subj) lb g = filterBySubj lb g
filterWithLabelTypes (Pred) lb g = filterByPred lb g
filterWithLabelTypes (Obj) lb g = filterByObj lb g

filterIterateGraphs :: Combinator -> [RDFGraph] -> [Arc RDFLabel]
filterIterateGraphs (And) gs = getDuplicates $ combinedArcs gs
filterIterateGraphs (Or) gs = combinedArcs gs

combinedArcs :: [RDFGraph] -> [Arc RDFLabel]
combinedArcs gs = [arc | g <- gs, arc <- S.toList $ getArcs g]

getDuplicates :: [Arc RDFLabel] -> [Arc RDFLabel]
getDuplicates arcs = arcs \\ nub arcs

fil :: Arc RDFLabel -> Arc RDFLabel -> Bool
fil o arc = arc == o
------------------------------

------ GRAPH PROPERTIES ------
-- get labels of a graph
_labels :: (LDGraph lg lb, Ord lb) => lg lb -> S.Set lb
_labels graph = labels graph

-- get prefixes of a graph (such as p, s, and t)
_prefixes :: NSGraph lb -> NamespaceMap
_prefixes graph = getNamespaces graph

-- get nodes of a graph (all the triple elements of all triples, but without predicates)
_nodes :: (LDGraph lg lb, Ord lb) => lg lb -> S.Set lb
_nodes graph = nodes graph

-- get arcs of a graph (all the triples of a graph)
_arcs :: LDGraph lg lb => lg lb -> ArcSet lb
_arcs graph = getArcs graph

-- get graph size
_graphSize :: LDGraph lg lb => lg lb -> Int
_graphSize graph = S.size (getArcs graph)
------------------------------

--------- CONVERTING ---------
-- changes String into Text
strToText :: String -> T.Text
strToText c = T.pack c

-- changes Text into String
textToStr :: T.Text -> String
textToStr c = T.unpack c

-- changes String into Lazy Text
strToLText :: String -> TL.Text
strToLText c = TL.pack c

-- changes Lazy Text into String
lTextToStr :: TL.Text -> String
lTextToStr c = TL.unpack c
------------------------------

----------- TESTS ------------
-- print the Properties of label subject (used for testing)
printSubjLabelTypesOfArc :: Arc RDFLabel -> IO ()
printSubjLabelTypesOfArc arc = printWrapper "ARC PROPERTIES" $ do
                  let subj = arcSubj arc
                  let qname = fromJust $ fromRDFLabel $ subj
                  putStrLn "\nQname:"
                  print qname
                  putStrLn "\nQnameURI:"
                  print $ getQNameURI $ qname
                  putStrLn "\nNamespace:"
                  print $ getNamespace qname
                  putStrLn "\nLocal name:"
                  print $ getLocalName qname
                  putStrLn "\nScoped name:"
                  print $ getScopedName subj

printLabelTypesOfGraph :: NSGraph RDFLabel -> IO ()
printLabelTypesOfGraph rdfGraph = printWrapper "GRAPH PROPERTIES" $ do
                  putStrLn "NAMESPACE:"
                  print $ _prefixes rdfGraph
                  -- putStrLn "\nNODES:"
                  -- print $ _nodes rdfGraph
                  putStrLn "\nARCS:"
                  print $ _arcs rdfGraph
                  -- putStrLn "\nGRAPH:"
                  -- printGraph rdfGraph
                  -- putStrLn "\nLABELS:"
                  -- print $ _labels rdfGraph

printFilteringTests :: NSGraph RDFLabel -> IO ()
printFilteringTests rdfGraph = printWrapper "FILTERING TESTS" $ do
                    putStrLn "FILTERED BY OBJECT:"
                    putStrLn "ValToLabel tests:"
                    putStrLn "Int"
                    let v = valToLabel (-1 :: Int)
                    print v
                    putStrLn "String"
                    print $ valToLabel "String value"
                    putStrLn "URI to be"
                    print $ valToLabel "http://www.cw.org/prob4B"
                    putStrLn "Booleans"
                    print $ valToLabel True
                    print $ valToLabel False

                    putStrLn "First arc from graph:"
                    let eh = S.elemAt 2 (getArcs rdfGraph)
                    print eh
                    putStrLn "\nTestArc:"
                    let testArc = arc (allowURIOnly $ valToLabel "http://www.cw.org/subjectA") (allowURIOnly $ valToLabel "http://www.cw.org/predicateA") (valToLabel "http://www.cw.org/objectA")
                    print testArc
                    putStrLn "\nTestLabels:"
                    let testSubjLb = valToLabel "http://www.cw.org/prob4B"
                    let testPredLb = valToLabel "http://www.cw.org/testPredB"
                    let testObjLb = arcObj testArc
                    print testSubjLb
                    print testPredLb
                    print testObjLb
                    -- printGraph $ filterBySubj testSubjLb expanded
                    -- printGraph $ filterByPred testPredLb expanded
                    -- printGraph $ filterByObj testObjLb expanded

                    putStrLn "\nEquality between test and graph arc?:"
                    print $ fil testArc eh

                    putStrLn "\n\nEXPANDED:"
                    let expanded = expandTriples rdfGraph
                    printGraph expanded

-- plain strings dont parse to URI, numbers (in strings) dont either, plain numbers give an error
-- true/false (in strings) dont parse, plain true/false throw an error

printWrapper :: String -> IO a -> IO ()
printWrapper msg io = do
                putStrLn ("--------- " ++ msg ++ "----------")
                io
                putStrLn "-----------------------------"
------------------------------
-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing


-- REFERENCES
-- REF1, creating an existential type, author: Fyodor Soikin, accessed April 2022: https://stackoverflow.com/a/52267346/18413650
-- REF2, trimming whitespace from end of string, author: spopejoy, accessed April 2022, https://stackoverflow.com/a/38283069/18413650
