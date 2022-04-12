{-
COMP2212 Programming Language Concepts Group Coursework
Charlie Bowe
Yaqin Hasan
Patrycja Wanat
-}
{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings #-}

module Main where

import System.IO  
import Control.Monad
import qualified Data.Text as T
import Data.Text.Lazy as TL

import Swish.RDF.Parser.Turtle
    ( 
      ParseResult,
      parseTurtle,  
      parseTurtlefromText      
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
importFile :: IO String
importFile = do
      contents <- readFile "../inputs/bar.ttl"
      let res = parseInSwish contents
      print res
      return contents

parseInSwish :: String -> ParseResult
parseInSwish turtleString = parseTurtle (TL.pack turtleString) Nothing

-- Rdf4h version
parseInRdf4 :: IO ()
parseInRdf4 = do
  result <- parseFile (TurtleParser Nothing Nothing) "../inputs/bar.ttl" :: IO (Either ParseFailure (RDF TList))
  print result

main :: IO ()
main = parseInRdf4