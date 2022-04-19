import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle, parseTurtlefromText)
import Swish.Monad (SwishState, SwishStatus, SwishStateIO, emptyState, setFormat, setInfo, SwishFormat (Turtle), NamedGraphMap, format, base, graph, graphs, rules, rulesets, infomsg, errormsg, exitcode)
import qualified Data.Text.Lazy as TL (Text, pack, unpack)
import qualified Data.Text as T (Text, pack, unpack, stripPrefix, stripSuffix)
import Swish.RDF.Graph (RDFGraph, RDFLabel(Res), NamespaceMap, NSGraph, emptyRDFGraph, toRDFLabel, nodes, getNamespaces)
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
import Swish.Commands (swishInput)
import Swish.QName (QName)
import Swish.RDF.Ruleset (RDFRuleMap, RDFRulesetMap)
import Swish.GraphClass (ArcSet, LDGraph, getArcs)
import Control.Monad.State as CMS (get, put, runStateT, evalStateT, StateT)
import Control.Monad.State.Lazy (get, return, liftIO, execState, forever)
import Control.Monad.Reader.Class (ask)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)
import Data.Set (Set)
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
              printPrefixes rdfGraph
              putStrLn "\nNODES:"
              printNodes rdfGraph
              putStrLn "\nARCS:"
              printArcs rdfGraph
              putStrLn "\nGRAPH:"
              printGraph rdfGraph
              
      
      putStrLn "\n"
      let printComment = "RETURNING: " ++ contents
      return printComment

-- parse input into the turtle format
parseIntoTurtle :: String -> Maybe URI -> ParseResult
parseIntoTurtle contents base = parseTurtle (strToLText contents) base

-- get the base value from a turtle file 
getBase :: String -> Maybe URI
getBase contents = do
            let prefix = "@base <"
            let suffix = "> ."

            let bases = filter (isBase prefix suffix) $ lines contents
            
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

printState :: SwishState -> String
printState s = "format: " ++ show (format s) ++ ", base: " ++ show (base s) ++ ", graph: " ++ show (graph s) ++ ", rules: " ++ show (rules s) ++ ", infomsg: " ++ show (infomsg s) ++ ", errormsg: " ++ show (errormsg s) ++ ", exitcode: " ++ show (exitcode s)

-- print prefixes of a graph (such as p, s, and t)
printPrefixes :: NSGraph lb -> IO ()
printPrefixes graph = print $ getNamespaces graph

-- print nodes of a graph (all the triple elements of all triples, but without predicates)
printNodes :: (Show lb, Swish.GraphClass.LDGraph lg lb, Ord lb) => lg lb -> IO ()
printNodes graph = print $ nodes graph

-- print arcs of a graph (all the triples of a graph)
printArcs :: (Show lb, LDGraph lg lb) => lg lb -> IO ()
printArcs graph = print $ getArcs graph 

-- print Graph into output (with newlines)
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn (textToStr $ formatGraphAsText g)

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

-------------------------------------------------- STATE --------------------------------------------------
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
          