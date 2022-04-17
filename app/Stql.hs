import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle)
import Swish.Monad (SwishState, SwishStateIO, emptyState, setFormat, SwishFormat (Turtle))
import Data.Text.Lazy as TL (Text, pack, unpack)
import Swish.RDF.Graph (RDFGraph, emptyRDFGraph, toRDFLabel)
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
import Swish.Commands (swishInput)
import Control.Monad.State.Lazy (get, return, liftIO)

createGraph :: RDFGraph
createGraph = emptyRDFGraph

main :: IO ()
main = putStrLn "hello, swish!"

-- Swish version
-- importFile :: IO String
importFile = do
      contents <- readFile "../inputs/bar.ttl"
      
      
      -- putStrLn contents
      case (parseIntoTurtle contents) of
        Left err -> error "Can't parse the file."
        Right rdfGraph -> do
            let o = formatGraphAsText rdfGraph
            print o
      
      putStrLn "\n"
      let printComment = "RETURNING: " ++ contents
      return printComment

-- readInGraph :: SwishStateIO ()
-- readInGraph = do
--           lol <- importFile
--           swishInput (Just lol)

-- fileToLabel :: String
-- fileToLabel s = toRDFLabel s

-- changes String into Text
fromStrToText :: String -> Text
fromStrToText s = TL.pack s

-- changes Text into String
fromTextToStr :: Text -> String
fromTextToStr s = TL.unpack s

-- change input into Text and parse it into the turtle format
parseIntoTurtle :: String -> ParseResult
parseIntoTurtle turtleString = parseTurtle (fromStrToText turtleString) Nothing

createState :: SwishState
createState = do
      let state = emptyState
      setFormat Turtle state
      
-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing