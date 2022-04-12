{-
COMP2212 Programming Language Concepts Group Coursework
Charlie Bowe
Yaqin Hasan
Patrycja Wanat
-}
{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings #-}

module Stql where

import System.IO ()  
import Control.Monad ()
import qualified Data.Text as T
import Data.Text.Lazy as TL ()

import Swish.RDF.Parser.Turtle
    ( 
      ParseResult,
      parseTurtle,  
      parseTurtlefromText  
    )

import Swish.RDF.Formatter.Turtle
    (
      formatGraphAsText
    ) 

import Data.RDF
  (
    Rdf,
    RDF,
    Triples,
    TList,
    ParseFailure,
    TurtleParser (TurtleParser),
    parseFile,
    fromEither,
    query,
    objectOf
  )

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

-- toText :: ParseResult -> Text
-- toText p = do
--             res <- 
--             Right formatGraphAsText p

-- Rdf4h version
parseInRdf4 :: IO ()
parseInRdf4 = do
  result <- parseFile (TurtleParser Nothing Nothing) "../inputs/bar.ttl" :: IO (Either ParseFailure (RDF TList))
  print result

main :: IO ()
main = parseInRdf4