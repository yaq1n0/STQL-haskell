-- IMPORTS
  -- HASKELL BASE IMPORTS
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)
import qualified Data.Set as S (Set, filter, size, elemAt, toList, fromList)
  -- SWISH IMPORTS
import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle, parseTurtlefromText)
import Swish.Monad (SwishState, SwishStatus, SwishStateIO, emptyState, setFormat, setInfo, SwishFormat (Turtle), NamedGraphMap, format, base, graph, graphs, rules, rulesets, infomsg, errormsg, exitcode)
import Swish.RDF.Graph (RDFGraph, RDFLabel(Res), NamespaceMap, NSGraph, Arc, fmapNSGraph, traverseNSGraph, arc, emptyRDFGraph, toRDFLabel, nodes, getNamespaces, extract, arcSubj, arcPred, arcObj, allLabels, fromRDFLabel, update, isLiteral, isUntypedLiteral, isTypedLiteral, isXMLLiteral, isDatatyped, isUri, getLiteralText, getScopedName, remapLabels, labels)
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
import Swish.Commands (swishInput)
import Swish.QName (QName, getQNameURI, getNamespace, getLocalName, getQNameURI)
import Swish.RDF.Ruleset (RDFRuleMap, RDFRulesetMap)
import Swish.GraphClass (ArcSet, LDGraph, setArcs, getArcs, arcLabels, arcToTriple)
import Swish.Namespace (getScopeNamespace, makeURIScopedName, getScopeLocal, getScopePrefix, getScopeURI, getQName, getScopedNameURI)
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
              let tempLabel = "http://www.cw.org/prob4B"
              let subFilterProp = toRDFLabel $ fromJust $ parseURI tempLabel
              let filtered = S.filter (fil mine) (getArcs rdfGraph)
              print filtered
              let lol = update (S.filter (fil mine)) rdfGraph
              putStrLn "\nFirst:"
              let eh = S.elemAt 1 (getArcs rdfGraph)
              print eh
              -- expands a namespace+uri
              putStrLn "\nQname:"
              let qname = fromJust $ fromRDFLabel $ arcSubj eh
              print qname
              let qnameURI = getQNameURI $ qname
              putStrLn "\nNamespace:"
              print $ getNamespace qname
              putStrLn "\nLocal name:"
              print $ getLocalName qname
              putStrLn "\nMine:"
              print mine
              putStrLn "\nEquality?:"
              print $ fil mine eh
              putStrLn "\nExpanded"
              let expanded = expandTriples rdfGraph
              printGraph expanded
              -- printGraph rdfGraph
              
             
              -- print $ S.size (getArcs rdfGraph)
              -- putStrLn "\nFILTERED BY OBJECT WITH EXTRACT:"
              -- printGraph $ filterByObj tempLabel rdfGraph
              -- filterByObj tempLabel rdfGraph
      
      putStrLn "\n"
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

-- expandTriples :: (Applicative f) => NSGraph RDFLabel -> f (NSGraph RDFLabel)
expandTriples graph = fmapNSGraph (conv) graph
              where
                ns = S.toList $ _nodes graph

toQName a = fromJust $ fromRDFLabel a
toLabel a = toRDFLabel a

f label = pure label <*> conv

conv :: RDFLabel -> RDFLabel
conv label = toLabel $ getQNameURI $ toQName label

-- fm :: (RDFLabel -> RDFLabel) -> NSGraph RDFLabel -> NSGraph RDFLabel
-- fm c g = setArcs g (S.fromList [arc (c a) (c b) (c c) | a <- S.toList $ getArcs g, (a, b, c) <- arcToTriple a])
-- data CB l = NSGraph l

-- NSGraph is our Functor; we need to apply a function to a value inside it
-- instance Applicative NSGraph where
  -- pure l = NSGraph l
  -- f <*> graph = [f l | a <- S.toList $ getArcs graph, l <- arcLabels a]
  -- pure l = toLabel $ getQNameURI $ toQName label
  -- f <*> graph = [f l | l <- S.toList $ _labels graph]

-- ap :: (RDFLabel -> RDFLabel) -> NSGraph -> NSGraph
-- ap f g = 

--------- FILTERING ----------
-- filterByObj :: String -> RDFGraph -> RDFGraph
-- filterByObj o graph = extract f graph
filterByObj o graph = getArcs graph
-- doesnt filter correctly; no results

-- f arc = arcSubj arc == toRDFLabel "<http://www.cw.org/prob4B>"

fil :: Arc RDFLabel -> Arc RDFLabel -> Bool
fil o arc = arc == o
-- (fromJust $ labelToString (arcSubj arc))

mine :: Arc RDFLabel
mine = arc (toRDFLabel $ fromJust $ parseURI "http://www.cw.org/subjectA") (toRDFLabel $ fromJust $ parseURI "http://www.cw.org/predicateA") (toRDFLabel $ fromJust $ parseURI "http://www.cw.org/objectA")

-- Namespace + Local Name = QName

labelToString :: RDFLabel -> Maybe URI
labelToString lb = fromRDFLabel lb

fst :: (a, b, c) -> a
fst (a, _, _) = a

thd :: (a, b, c) -> c
thd (_, _, c) = c

-- filterBySubj = 
------------------------------

---------- PRINTING ----------
printState :: SwishState -> String
printState s = "format: " ++ show (format s) ++ ", base: " ++ show (base s) ++ ", graph: " ++ show (graph s) ++ ", rules: " ++ show (rules s) ++ ", infomsg: " ++ show (infomsg s) ++ ", errormsg: " ++ show (errormsg s) ++ ", exitcode: " ++ show (exitcode s)

-- print labels of a graph
_labels :: (LDGraph lg lb, Ord lb) => lg lb -> S.Set lb
_labels graph = labels graph

-- print prefixes of a graph (such as p, s, and t)
_prefixes :: NSGraph lb -> NamespaceMap
_prefixes graph = getNamespaces graph

-- print nodes of a graph (all the triple elements of all triples, but without predicates)
_nodes :: (LDGraph lg lb, Ord lb) => lg lb -> S.Set lb
_nodes graph = nodes graph

-- print arcs of a graph (all the triples of a graph)
_arcs :: LDGraph lg lb => lg lb -> ArcSet lb
_arcs graph = getArcs graph

-- print Graph into output (with newlines)
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn (textToStr $ formatGraphAsText g)

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
          