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
import Swish.Monad (SwishState, SwishStatus, SwishStateIO, emptyState, setFormat, setInfo, SwishFormat (Turtle), NamedGraphMap, format, base, graph, graphs, rules, rulesets, infomsg, errormsg, exitcode)
import Swish.RDF.Graph (RDFGraph, RDFLabel(Res), NamespaceMap, NSGraph, Arc, Selector, ToRDFLabel, addArc, merge, toRDFGraph, fmapNSGraph, traverseNSGraph, arc, emptyRDFGraph, toRDFLabel, nodes, getNamespaces, extract, arcSubj, arcPred, arcObj, allLabels, fromRDFLabel, update, isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral, isDatatyped, isUri, getLiteralText, getScopedName, remapLabels, labels)
import Swish.RDF.Formatter.Turtle (formatGraphAsText, formatGraphIndent, formatGraphAsBuilder)
import Swish.Commands (swishInput)
import Swish.QName (QName, getQNameURI, getNamespace, getLocalName, getQNameURI)
import Swish.RDF.Ruleset (RDFRuleMap, RDFRulesetMap)
import Swish.GraphClass (ArcSet, LDGraph, setArcs, getArcs, arcLabels, arcToTriple)
import Swish.Namespace (Namespace, ScopedName, getScopeNamespace, makeURIScopedName, getScopeLocal, getScopePrefix, getScopeURI, getQName, getScopedNameURI)
  -- MTL IMPORTS
import Control.Monad.State as CMS (get, put, runStateT, evalStateT, StateT)
import Control.Monad.State.Lazy (get, return, liftIO, execState, forever)
import Control.Monad.Reader.Class (ask)
  -- TEXT IMPORTS
import qualified Data.Text.Lazy as TL (Text, pack, unpack)
import qualified Data.Text as T (Text, pack, unpack, stripPrefix, stripSuffix)
import Data.List (nub, (\\))
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
-- wrap LabelTypeTuple in an existential type
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
-- main = putStrLn "hello, swish!"
main = print $ printState emptyState

testing :: IO String
testing = importFile "../inputs/bar.ttl" "../inputs/foo.ttl" "../inputs/fooProb5.ttl"

-- Swish version
importFile :: FilePath -> FilePath -> FilePath -> IO String
importFile filepath filepath' filepath'' = do
      file <- readFile filepath
      file' <- readFile filepath'
      file'' <- readFile filepath''

      let g = getGraph file
      let g' = getGraph file'
      let g'' = getGraph file''
      putStrLn "P5 GRAPH"
      printGraph g''
      
      let triple = (Nothing, Just (LabelTypeSubTriple "http://www.cw.org/problem5/#inRange"), Just (LabelTypeSubTriple False))
      let triple' = (Nothing, Nothing, Just (Incr 1))
      let triple'' = (Nothing, Just (LabelTypeSubTriple "http://www.cw.org/problem5/#inRange"), Just (LabelTypeSubTriple True))
      
      let edited = editGraphs g'' Out 0 99 triple
      let edited' = editGraphs g'' In 0 99 triple'
      let edited'' = editGraphs g'' In 0 99 triple''
      putStrLn "EDITED"
      edited
      putStrLn "EDITED'"
      edited'
      putStrLn "EDITED''"
      edited''
      -- printGraph edited'

      -- printGraphPairManipulations g g'
      -- printLabelTypesOfGraph barGraph
      -- printFilteringTests barGraph
      -- printFilteringTests fooGraph

      return ""

------- EDITING GRAPHS -------
editGraphs :: RDFGraph -> Direction -> Integer -> Integer -> SubTriple -> IO ()
-- editGraphs g (In) startInt endInt triple = putStrLn "works"
editGraphs g dir startInt endInt triple = do
                                            -- putStrLn "FILTERED"
                                            -- printGraph filteredGraph
                                            -- putStrLn "ARCS TO SUB"
                                            -- print arcsToSub
                                            putStrLn "SUBSTITUTED"
                                            printGraph substituted
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

-- checkIfURIRDFLabel :: RDFLabel -> RDFLabel
-- checkIfURIRDFLabel lb = toRDFLabel $ parseToURIOnly lb

allowURIOnly :: RDFLabel -> RDFLabel
allowURIOnly lb = case isUri lb of
                    False -> error "The label is not a URI."
                    True -> lb

-- parseToURIOnly :: RDFLabel -> URI
-- parseToURIOnly lb = case fromRDFLabel lb of
--                       Nothing -> error "Couldn't parse the label to a URI."
--                       Just x -> case parseURI x of
--                                     Nothing -> error "Couldn't parse URI. This triple property can only be of type URI."
--                                     Just x -> x

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

-- TODO: doesn't preserve graph prefixes yet
filterMultiple :: [LabelTypeTuple] -> Combinator -> RDFGraph -> RDFGraph
filterMultiple as c g = toRDFGraph $ S.fromList $ filterIterateGraphs c [handleFilterLabelTypes labelCat lb g | (labelCat, lb) <- as]

handleFilterLabelTypes :: Category -> RDFLabel -> RDFGraph -> RDFGraph
handleFilterLabelTypes (Subj) lb g = filterBySubj lb g
handleFilterLabelTypes (Pred) lb g = filterByPred lb g
handleFilterLabelTypes (Obj) lb g = filterByObj lb g

filterIterateGraphs :: Combinator -> [RDFGraph] -> [Arc RDFLabel]
filterIterateGraphs (And) gs = getDuplicates [arc | g <- gs, arc <- S.toList $ getArcs g]
filterIterateGraphs (Or) gs = error "lol"

getDuplicates :: [Arc RDFLabel] -> [Arc RDFLabel]
getDuplicates arcs = arcs \\ nub arcs

fil :: Arc RDFLabel -> Arc RDFLabel -> Bool
fil o arc = arc == o
------------------------------

---------- PRINTING ----------
printState :: SwishState -> String
printState s = "format: " ++ show (format s) ++ ", base: " ++ show (base s) ++ ", graph: " ++ show (graph s) ++ ", rules: " ++ show (rules s) ++ ", infomsg: " ++ show (infomsg s) ++ ", errormsg: " ++ show (errormsg s) ++ ", exitcode: " ++ show (exitcode s)

-- print Graph into output (with newlines)
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn (textToStr $ formatGraphAsText g)

-- TODO: print graph out with predicate/object lists expanded
-- printGraphWPrefixes g = formatGraphIndent (formatGraphAsBuilder g) True g

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

printGraphPairManipulations :: NSGraph RDFLabel -> NSGraph RDFLabel -> IO ()
printGraphPairManipulations rdfGraph rdfGraph' = printWrapper "GRAPH PAIR MANIPULATIONS" $ do
                            let gExpanded = expandTriples rdfGraph
                            let g'Expanded = expandTriples rdfGraph'
                            -- putStrLn "BAR"
                            -- printGraph gExpanded
                            -- putStrLn "FOO"
                            -- printGraph g'Expanded
                            
                            putStrLn "COMPARED GRAPHS"
                            let compared = compareGraphs g'Expanded Obj gExpanded Subj
                            printGraph compared
                            
                            -- putStrLn "MERGED GRAPHS"
                            -- let merged = mergeGraphs gExpanded g'Expanded
                            -- printGraph $ merged

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

                    -- custom-written filtering
                    putStrLn "\nCustom-written filtering:"
                    let filtered = S.filter (fil testArc) (getArcs rdfGraph)
                    let lol = update (S.filter (fil testArc)) rdfGraph
                    print filtered

                    putStrLn "\nEquality between test and graph arc?:"
                    print $ fil testArc eh
                    
                    putStrLn "\n\nEXPANDED:"
                    let expanded = expandTriples rdfGraph
                    printGraph expanded

                    putStrLn "\nFILTERED BY OBJECT WITH EXTRACT:"
                    let problem2Subj = valToLabel "http://www.cw.org/#problem2"
                    let problem2Obj = valToLabel True
                    let problem3Pred1 = valToLabel "http://www.cw.org/problem3/#predicate1"
                    let problem3Pred2 = valToLabel "http://www.cw.org/problem3/#predicate2"
                    let problem3Pred3 = valToLabel "http://www.cw.org/problem3/#predicate3"
                    putStrLn "PROBLEM 2"
                    putStrLn "1)"
                    let graphFil1 = filterBySubj problem2Subj expanded
                    printGraph graphFil1
                    putStrLn "2)"
                    let graphFil2 = filterByObj problem2Obj expanded
                    printGraph graphFil2
                    putStrLn "Combined)"
                    let combined = filterByObj problem2Obj graphFil1
                    printGraph combined
                    
                    putStrLn "\nPROBLEM 3"
                    putStrLn "1)"
                    printGraph $ filterByPred problem3Pred1 expanded
                    putStrLn "2)"
                    printGraph $ filterByPred problem3Pred2 expanded
                    putStrLn "3)"
                    printGraph $ filterByPred problem3Pred3 expanded

                    putStrLn "\nFILTER WITH MULTIPLE FILTERS:"
                    let testLabelTypeTupleAr = [(Subj, problem2Subj), (Obj, problem2Obj)]
                    printGraph $ filterMultiple testLabelTypeTupleAr And rdfGraph

-- plain strings dont parse to URI, numbers (in strings) dont either, plain numbers give an error
-- true/false (in strings) dont parse, plain true/false throw an error

printWrapper :: String -> IO a -> IO ()
printWrapper msg io = do
                putStrLn ("--------- " ++ msg ++ "----------")
                io
                putStrLn "-----------------------------------"
                putStrLn ""
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

fst :: (a, b, c) -> a
fst (a, _, _) = a

thrd :: (a, b, c) -> c
thrd (_, _, c) = c
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

-- REFERENCES
-- REF1, creating an existential type, author: Fyodor Soikin, accessed April 2022: https://stackoverflow.com/a/52267346/18413650



-- graphFromFileString :: String -> RDFGraph
-- graphFromFileString foo = do

-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing

-- readInGraph :: SwishStateIO ()
-- readInGraph = do
--           lol <- importFile
--           swishInput (Just lol)

-- fileToLabel :: String
-- fileToLabel s = toRDFLabel s

-- intt :: (Num a, Eq a, Integral a) => a -> RDFLabel
-- intt i = do 
--         let m = toInteger i
--         toRDFLabel m

----------- STATE ------------
-- main = CMS.evalStateT setState emptyState
-- CMS.evalStateT (forever createState) emptyState

-- emit :: Show a => a -> SwishStateIO ()
-- emit = SwishState . liftIO . print . show

-- add10Points :: SwishStateIO
-- add10Points = do state <- get
                 
      -- state <- setState $ do 
      --       s <- emptyState
      --       y <- setFormat Turtle s
      --       k <- setInfo "lol" s
      --       return s
      -- print "."

-- createState :: SwishStateIO SwishState
-- createState = do
--       let state = emptyState
--       -- setFormat Turtle state
--       return state


-- setState :: SwishStateIO ()
-- setState = do
--           n <- get
--           -- swishInput (Just "../inputs/bar.ttl")
--           liftIO $ print n
          