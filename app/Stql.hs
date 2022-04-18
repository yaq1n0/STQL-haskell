import Swish.RDF.Parser.Turtle (ParseResult, parseTurtle, parseTurtlefromText)
import Swish.Monad (SwishState, SwishStateIO, emptyState, setFormat, SwishFormat (Turtle))
import qualified Data.Text.Lazy as TL (Text, pack, unpack)
import qualified Data.Text as T (Text, pack, unpack, stripPrefix, stripSuffix)
import Swish.RDF.Graph (RDFGraph, emptyRDFGraph, toRDFLabel)
import Swish.RDF.Formatter.Turtle (formatGraphAsText)
import Swish.Commands (swishInput)
import Control.Monad.State.Lazy (get, return, liftIO)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)

createGraph :: RDFGraph
createGraph = emptyRDFGraph

main :: IO ()
main = putStrLn "hello, swish!"

-- Swish version
importFile :: IO String
importFile = do
      contents <- readFile "../inputs/bar.ttl"
      -- putStrLn contents

      -- printGraph $ graphFromFileString contents
      putStrLn "BASE: "
      print (getBase contents)
      case (parseIntoTurtle contents) of
                                  Left err -> putStrLn "Can't parse the file."
                                  Right rdfGraph -> do
                                      printGraph rdfGraph
      
      putStrLn "\n"
      let printComment = "RETURNING: " ++ contents
      return printComment

-- graphFromFileString :: String -> RDFGraph
-- graphFromFileString contents = do

-- get the base value from a turtle file 
getBase :: String -> Maybe String
getBase content = do
            let bases = filter isBase $ lines content
            
            -- let bases = matchAllBases content
            
            case bases of
              [] -> Nothing
              [a] -> Just (extractBase a)
              (a:as) -> error "The document contains too many @base properties (more than one)"

-- extract the actual base string
extractBase :: String -> String
extractBase base = do
            let maybesP = T.stripPrefix (strToText "@base ") (strToText base)
            let sP = fromJust maybesP
            let maybesS = T.stripSuffix (strToText " .") sP
            let sS = fromJust maybesS
            textToStr sS

isBase :: String -> Bool
isBase c = "@base " `isPrefixOf` c && " ." `isSuffixOf` c

-- matchAllBases :: String -> [String]
-- matchAllBases c = getAllTextMatches $ c =~ "(^@base)(.*)( \\.$)"
-- "(?<=@base )(.*)(?= .)")

-- parse input into the turtle format
parseIntoTurtle :: String -> ParseResult
parseIntoTurtle turtleString = parseTurtle (strToLText turtleString) Nothing

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

createState :: SwishState
createState = do
      let state = emptyState
      setFormat Turtle state
      
-- toText :: Text -> ParseResult
-- toText p = parseTurtle p Nothing

-- readInGraph :: SwishStateIO ()
-- readInGraph = do
--           lol <- importFile
--           swishInput (Just lol)

-- fileToLabel :: String
-- fileToLabel s = toRDFLabel s