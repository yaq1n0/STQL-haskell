module Main where
import Swish.RDF.Graph (RDFGraph, RDFLabel(Res), NamespaceMap, NSGraph, Arc, arc, emptyRDFGraph, toRDFLabel, nodes, getNamespaces, extract, arcSubj, arcPred, arcObj, allLabels, fromRDFLabel)
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle, parseTurtlefromText)
import qualified Data.Text as T (Text, pack, unpack, stripPrefix, stripSuffix)
import qualified Data.Text.Lazy as TL (Text, pack, unpack)
import Network.URI (URI, parseURI)

main :: IO ()
main = do
  contents <- readFile "./inputs/bar.ttl"
  case (parseIntoTurtle contents) of
      Left err -> putStrLn "Can't parse the file."
      Right rdfGraph -> do
                printGraph rdfGraph
  putStrLn "LOL WORKS!"

-- print Graph into output (with newlines)
printGraph :: RDFGraph -> IO ()
printGraph g = putStrLn (textToStr $ formatGraphAsText g)

-- parse input into the turtle format
parseIntoTurtle :: String -> ParseResult
parseIntoTurtle contents = parseTurtle (strToLText contents) Nothing

-- changes Text into String
textToStr :: T.Text -> String
textToStr c = T.unpack c

-- changes String into Lazy Text
strToLText :: String -> TL.Text
strToLText c = TL.pack c