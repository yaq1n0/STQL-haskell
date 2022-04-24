{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import Data.List (nub, (\\), sort, intersperse)

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
main = putStrLn "hello, swish!"

testing :: IO String
testing = importFile "../inputs/bar.ttl" "../inputs/foo.ttl" "../inputs/fooProb5.ttl"

importFile :: FilePath -> FilePath -> FilePath -> IO String
importFile filepath filepath' filepath'' = do
      file <- readFile filepath
      file' <- readFile filepath'
      file'' <- readFile filepath''

      let graphs = getGraphs [file, file', file'']
      let expanded = expandGraphs graphs
      let cleaned = cleanGraphs expanded
      let (g, g', g'') = (cleaned !! 0, cleaned !! 1, cleaned !! 2)
      printP1 g g'
      printP2 g'
      printP3 g'
      printP4 g g'
      
      -- putStrLn $ graphToText (expanded !! 1)

      -- printProblem5 (cleaned !! 0) (cleaned !! 1) (cleaned !! 2)
      -- printGraphPairManipulations (cleaned !! 0) (cleaned !! 1)
      -- printLabelTypesOfGraph barGraph
      -- printFilteringTests barGraph
      -- printFilteringTests fooGraph

      return ""

-- out :: RDFGraph -> IO ()
-- out g = putStrLn $ textToStr $ formatGraphAsTextTP g
-- out g = putStrLn $ textToStr $ graphToText g



graphToString :: RDFGraph -> String
graphToString g = unlines triplesStringified
        where
          -- intoLines = intersperse "\n" triplesStringified
          triplesStringified = [(tripleToOut triple) ++ " ." | triple <- sorted]
          sorted = sort $ triples g

tripleToOut :: (RDFLabel, RDFLabel, RDFLabel) -> String
tripleToOut (lb, lb', lb'') = lbToOut lb ++ lbToOut lb' ++ lbToOut lb''

lbToOut :: RDFLabel -> String
lbToOut lb = case isUri lb of
              True -> show lb
              False -> " " ++ show lb
                -- case fromRDFLabel lb :: Maybe Integer of
                --         Just x -> " " ++ show x
                --         Nothing -> " " ++ show lb
                          -- case (show lb == "true" || show lb == "false") of
                          --           True -> " " ++ show lb
                          --           False -> " \"" ++ show lb ++ "\""

triples :: RDFGraph -> [(RDFLabel, RDFLabel, RDFLabel)]
triples g = [(arcSubj arc, arcPred arc, arcObj arc) | arc <- S.toList $ getArcs g]

------- EDITING GRAPHS -------
editGraphs :: RDFGraph -> Direction -> Integer -> Integer -> SubTriple -> RDFGraph
editGraphs g dir startInt endInt triple = substituted
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
compareGraphs :: RDFGraph -> Category -> RDFGraph -> Category -> RDFGraph
compareGraphs g catg g' catg' = merged
                      where
                        -- merge all the filtered graphs
                        merged = mergeMultiple filtered
                        filtered = tuplesToList filteredTupled
                        -- filter both graphs by the matches between those graphs' lists of (subj OR pred OR obj)
                        filteredTupled = [(handleFilterLabelTypes catg d g, handleFilterLabelTypes catg' d g') | d <- duplicates firstCatgs sndCatgs]
                        -- all the matches between the (subj OR pred OR obj) lists of both graphs
                        duplicates xs ys = filter (\x -> (x `elem` ys)) xs
                        -- all (subj OR pred OR obj) of the first and second graphs, respectively
                        firstCatgs = [getCategoryLabel catg arcg | arcg <- S.toList $ getArcs g]
                        sndCatgs = [getCategoryLabel catg' arcg' | arcg' <- S.toList $ getArcs g']

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

handleFilterRanges :: Direction -> Integer -> Integer -> RDFGraph -> RDFGraph       
handleFilterRanges (In) n n' g = filterRangesIn n n' g
handleFilterRanges (Out) n n' g = filterRangesOut n n' g

filterMultiple :: [LabelTypeTuple] -> Combinator -> RDFGraph -> RDFGraph
filterMultiple as c g = toRDFGraph $ S.fromList $ filterIterateGraphs c filteredGraphs
                  where
                    filteredGraphs = [handleFilterLabelTypes labelCat lb g | (labelCat, lb) <- as]

handleFilterLabelTypes :: Category -> RDFLabel -> RDFGraph -> RDFGraph
handleFilterLabelTypes (Subj) lb g = filterBySubj lb g
handleFilterLabelTypes (Pred) lb g = filterByPred lb g
handleFilterLabelTypes (Obj) lb g = filterByObj lb g

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

---------- PRINTING ----------
-- print a fully expanded graph
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn $ graphToString g

printGraphToOut :: RDFGraph -> String
printGraphToOut g = textToStr $ formatGraphAsText g

-- print graph with indentations and predicate/object lists
printGraphIndented :: RDFGraph -> IO ()
printGraphIndented g = putStrLn $ printGraphToOut g

  ---------- PROBLEMS ----------
  -- P1
p1 :: RDFGraph -> RDFGraph -> RDFGraph
p1 g g' = mergeGraphs g g'

printP1 :: NSGraph RDFLabel -> NSGraph RDFLabel -> IO ()
printP1 g g' = printWrapper "PROBLEM 1" $ do
                        printGraph $ p1 g g'

  -- P2
p2 :: [LabelTypeTuple] -> Combinator ->  RDFGraph -> RDFGraph
p2 = filterMultiple

printP2 :: RDFGraph -> IO ()
printP2 g = printWrapper "PROBLEM 2" $ do
                      let fs = p2Test
                      printGraph $ p2 fs And g

p2Test :: [LabelTypeTuple]
p2Test = do
        let problem2Subj = valToLabel "http://www.cw.org/#problem2"
        let problem2Obj = valToLabel True
        let filters = [(Subj, problem2Subj), (Obj, problem2Obj)]
        filters

printP2Extended ::  RDFGraph -> IO ()
printP2Extended g = printWrapper "PROBLEM 2" $ do
                      let fs = p2Test
                      putStrLn "1)"
                      let graphFil1 = filterBySubj (snd $ head fs) g
                      printGraph graphFil1
                      putStrLn "2)"
                      let graphFil2 = filterByObj (snd $ last fs) g
                      printGraph graphFil2
                      putStrLn "Combined)"
                      let combined = p2 fs And g
                      printGraph combined

  -- P3
p3 :: [LabelTypeTuple] -> Combinator -> RDFGraph -> RDFGraph
p3 = filterMultiple

printP3 :: RDFGraph -> IO ()
printP3 g = printWrapper "PROBLEM 3" $ do
                let fs = p3Test
                printGraph $ p3 fs Or g

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
p4 :: RDFGraph -> Category -> RDFGraph -> Category -> RDFGraph
p4 = compareGraphs

printP4 :: RDFGraph -> RDFGraph -> IO ()
printP4 g g' = printWrapper "PROBLEM 4" $ do
                      printGraph $ p4 g Subj g' Obj

  -- P5
-- p5 g = 

-- printP5 :: RDFGraph -> RDFGraph -> RDFGraph -> IO ()
-- printP5 g g' g'' = printWrapper "PROBLEM 5" $ do
--                           putStrLn "P5 GRAPH"
--                           printGraph g''
                          
--                           let triple = (Nothing, Just (LabelTypeSubTriple "http://www.cw.org/problem5/#inRange"), Just (LabelTypeSubTriple False))
--                           let triple' = (Nothing, Nothing, Just (Incr 1))
--                           let triple'' = (Nothing, Just (LabelTypeSubTriple "http://www.cw.org/problem5/#inRange"), Just (LabelTypeSubTriple True))
                          
--                           let edited = editGraphs g'' Out 0 99 triple
--                           let edited' = editGraphs g'' In 0 99 triple'
--                           let edited'' = editGraphs g'' In 0 99 triple''
--                           putStrLn "EDITED"
--                           printGraph edited
--                           -- printToFile edited "first.ttl"
--                           putStrLn "EDITED'"
--                           -- printToFile edited' "snd.ttl"
--                           printGraph edited'
                          
--                           putStrLn "EDITED''"
--                           printGraph edited''
                          -- printToFile edited'' "third.ttl"


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

printToFile :: RDFGraph -> String -> IO ()
printToFile g filename = writeFile filename (printGraphToOut g)

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
-- REF2, Swish documentation, not exported functions from Formatter Turtle module: https://hackage.haskell.org/package/swish-0.10.1.0/docs/src/Swish.RDF.Formatter.Turtle.html#formatGraphAsText