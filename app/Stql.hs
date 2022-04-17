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
      emptyRDFGraph,
      toRDFLabel
    )

import Swish.RDF.Formatter.Turtle
    (
      formatGraphAsText
    )

import Swish.Commands
  (
    swishInput
  )

import Control.Monad.State.Lazy
  (
    get,
    return
  )

createGraph :: RDFGraph
createGraph = emptyRDFGraph

main :: IO ()
main = putStrLn "hello, swish!"

-- Swish version
importFile :: IO String
importFile = do
      contents <- readFile "../inputs/bar.ttl"
      let j = swishInput (Just contents)
      -- putStrLn (return j)
      putStrLn contents
      -- res <- parseInSwish contents
      -- case contents of
      --   Left err -> error "Can't parse the file."
      --   Right rdfGraph -> do
      --       let o = formatGraphAsText rdfGraph
      --       print o
      return contents

-- fileToLabel :: String
-- fileToLabel s = toRDFLabel s

-- parseInSwish :: String -> ParseResult
-- parseInSwish turtleString = parseTurtle (TL.pack turtleString) Nothing

createState :: SwishState
createState = do
      let state = emptyState
      setFormat Turtle state
      
-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing