import Swish.RDF.Parser.Turtle
  (
    ParseResult,
    parseTurtle
    -- parseInSwish
  )

-- import Data.Text

-- import Swish.RDF.Parser.Turtle
--     ( 
--       ParseResult,
--       parseTurtle,  
--       parseTurtlefromText  
--     )

import Data.Text.Lazy

import Swish.RDF.Formatter.Turtle
    (
      formatGraphAsText
    ) 

main :: IO ()
main = putStrLn "hello, swish!"

-- Swish version
-- importFile :: IO String
-- importFile = do
--       contents <- readFile "../inputs/bar.ttl"
--       res <- parseInSwish contents
--       case res of
--         Left err -> error "Can't parse the file."
--         Right rdfGraph -> do
--             let o = formatGraphAsText rdfGraph
--             print o
--       return contents

-- parseInSwish :: String -> ParseResult
-- parseInSwish turtleString = parseTurtle (TL.pack turtleString) Nothing

toText :: Text -> ParseResult
toText p = parseTurtle p Nothing