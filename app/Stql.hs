import Swish.RDF.Parser.Turtle
  (
    ParseResult,
    parseTurtle
    -- parseTurtlefromText  
  )

import Swish.Monad
  (
    SwishState,
    SwishStateIO,
    emptyState,
    setFormat,
    SwishFormat (Turtle)
  )

-- import Data.Text

import Data.Text.Lazy as TL 
  (
    Text,
    pack
  )

import Swish.RDF.Graph
    (
      RDFGraph,
      emptyRDFGraph
    )

import Swish.RDF.Formatter.Turtle
    (
      formatGraphAsText
    ) 

createGraph :: RDFGraph
createGraph = emptyRDFGraph

main :: IO ()
main = putStrLn "hello, swish!"

-- Swish version
importFile :: IO String
importFile = do
      contents <- readFile "../inputs/bar.ttl"
      -- res <- parseInSwish contents
      -- case contents of
      --   Left err -> error "Can't parse the file."
      --   Right rdfGraph -> do
      --       let o = formatGraphAsText rdfGraph
      --       print o
      return contents

-- parseInSwish :: String -> ParseResult
-- parseInSwish turtleString = parseTurtle (TL.pack turtleString) Nothing

createState :: SwishState
createState = do
      let state = emptyState
      setFormat Turtle state
      
-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing