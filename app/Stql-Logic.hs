-- IMPORTS
  -- HASKELL BASE IMPORTS
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)
import qualified Data.Set as S (Set, filter, size, elemAt, toList, fromList)
  -- SWISH IMPORTS
import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle, parseTurtlefromText)
import Swish.Monad (SwishState, SwishStatus, SwishStateIO, emptyState, setFormat, setInfo, SwishFormat (Turtle), NamedGraphMap, format, base, graph, graphs, rules, rulesets, infomsg, errormsg, exitcode)
import Swish.RDF.Graph (RDFGraph, RDFLabel(Res), NamespaceMap, NSGraph, Arc, Selector, fmapNSGraph, traverseNSGraph, arc, emptyRDFGraph, toRDFLabel, nodes, getNamespaces, extract, arcSubj, arcPred, arcObj, allLabels, fromRDFLabel, update, isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral, isDatatyped, isUri, getLiteralText, getScopedName, remapLabels, labels)
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
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
  -- NETWORK IMPORTS
import Network.URI (URI, parseURI)

main :: IO ()
-- main = putStrLn "hello, swish!"
main = print $ printState emptyState

-- Swish version
importFile :: IO String
importFile = do
      contents <- readFile "../inputs/bar.ttl"
      -- putStrLn contents

      -- printGraph $ graphFromFileString contents
      let base = getBase contents
      putStrLn "BASE: "
      print base
      case (parseIntoTurtle contents base) of
          Left err -> putStrLn "Can't parse the file."
          Right rdfGraph -> do
              putStrLn "\nNAMESPACE:"
              print $ _prefixes rdfGraph
              putStrLn "\nNODES:"
              print $ _nodes rdfGraph
              putStrLn "\nARCS:"
              print $ _arcs rdfGraph
              putStrLn "\nGRAPH:"
              printGraph rdfGraph
              putStrLn "\nLABELS:"
              print $ _labels rdfGraph
              
              putStrLn "\nFILTERED BY OBJECT:"
              putStrLn "\nFirst arc from graph:"
              let eh = S.elemAt 2 (getArcs rdfGraph)
              print eh
              putStrLn "\nTestArc:"
              let testArc = arc (strToURIRDFLabel "http://www.cw.org/subjectA") (strToURIRDFLabel "http://www.cw.org/predicateA") (strToURIRDFLabel "http://www.cw.org/objectA")
              print testArc
              putStrLn "\nTestLabels:"
              let testSubjLb = "http://www.cw.org/prob4B"
              let testPredLb = "http://www.cw.org/testPredB"
              let testObjLb = show $ rdfLabelToURI $ arcObj testArc
              print testSubjLb
              print testPredLb
              print testObjLb

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
              -- printGraph rdfGraph

              putStrLn "\nFILTERED BY OBJECT WITH EXTRACT:"
              printGraph $ filterBySubj testSubjLb expanded
              printGraph $ filterByPred testPredLb expanded
              printGraph $ filterByObj testObjLb expanded

      -- let printComment = "RETURNING: " ++ contents
      -- return printComment
      return ""

-- parse input into the turtle format
parseIntoTurtle :: String -> Maybe URI -> ParseResult
parseIntoTurtle contents base = parseTurtle (strToLText contents) base

-- get the base value from a turtle file 
getBase :: String -> Maybe URI
getBase contents = do
            let prefix = "@base <"
            let suffix = "> ."

            let bases = Prelude.filter (isBase prefix suffix) $ lines contents
            
            case bases of
              [] -> Nothing
              [a] -> Just (toURI $ extractBase a prefix suffix)
              (a:as) -> error "The document contains too many @base properties (more than one)"
      -- checks whether the provided text is a base property
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

createGraph :: RDFGraph
createGraph = emptyRDFGraph

-- TODO: Fails at foo.ttl and fooProb5.ttl
expandTriples :: NSGraph RDFLabel -> NSGraph RDFLabel
expandTriples graph = fmapNSGraph conv graph
    where
      toQName a = fromJust $ fromRDFLabel a
      toLabel a = toRDFLabel a
      -- get qnames of labels inside the graph 
      -- QName = Namespace + Local Name
      conv label = toLabel $ getQNameURI $ toQName label

--------- FILTERING ----------
filterBySubj :: String -> RDFGraph -> RDFGraph
filterBySubj subj graph = extract s graph
            where s arc = arcSubj arc == strToURIRDFLabel subj

filterByPred :: String -> RDFGraph -> RDFGraph
filterByPred pred graph = extract s graph
            where s arc = arcPred arc == strToURIRDFLabel pred

filterByObj :: String -> RDFGraph -> RDFGraph
filterByObj obj graph = extract s graph
            where s arc = arcObj arc == strToURIRDFLabel obj

fil :: Arc RDFLabel -> Arc RDFLabel -> Bool
fil o arc = arc == o
------------------------------

---------- PRINTING ----------
printState :: SwishState -> String
printState s = "format: " ++ show (format s) ++ ", base: " ++ show (base s) ++ ", graph: " ++ show (graph s) ++ ", rules: " ++ show (rules s) ++ ", infomsg: " ++ show (infomsg s) ++ ", errormsg: " ++ show (errormsg s) ++ ", exitcode: " ++ show (exitcode s)

-- print Graph into output (with newlines)
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn (textToStr $ formatGraphAsText g)

-- print the properties of label subject (used for testing)
printSubjPropsOfArc :: Arc RDFLabel -> IO ()
printSubjPropsOfArc arc = do
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

strToURIRDFLabel :: String -> RDFLabel
strToURIRDFLabel lb = toRDFLabel $ fromJust $ parseURI lb

rdfLabelToURI :: RDFLabel -> URI
rdfLabelToURI lb = getQNameURI $ fromJust $ fromRDFLabel lb

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

-- graphFromFileString :: String -> RDFGraph
-- graphFromFileString contents = do

-- mergeGraphs :: RDFGraph -> RDFGraph -> RDFGraph
-- mergeGraphs g g' =

-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing

-- readInGraph :: SwishStateIO ()
-- readInGraph = do
--           lol <- importFile
--           swishInput (Just lol)

-- fileToLabel :: String
-- fileToLabel s = toRDFLabel s

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
          