{-
COMP2212 Programming Language Concepts Group Coursework
Charlie Bowe
Yaqin Hasan
Patrycja Wanat
-}
{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings #-}

module Main where

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
import qualified Data.Text as T

timBernersLee :: IO ()
timBernersLee = do
  result <- parseFile (TurtleParser Nothing Nothing) "../inputs/bar.ttl" :: IO (Either ParseFailure (RDF TList))
  print result

lol :: ParseResult
lol = parseTurtle "<http://www.cw.org/subjectA> <http://www.cw.org/predicateA> <http://www.cw.org/objectA> ." Nothing

main :: IO ()
main = timBernersLee

-- main :: IO TList
-- main = fmap fromEither (parseFile TurtleParser "../inputs/bar.ttl")
-- rdfGraph1 :: IO TriplesList
-- rdfGraph1 = fmap fromEither (parseFile TurtleParser "../inputs/bar.ttl")

-- rdfGraph2 :: IO TriplesList
-- rdfGraph2 = fmap fromEither (parseFile TurtleParser "../inputs/foo.ttl")

-- example :: IO ()
-- example = do
--   g1 <- rdfGraph1
--   g2 <- rdfGraph2
--   let node1 = lnode $ PlainL "foo"
--   putStrLn $ "Subjects of g1: " ++ show (map subjectOf (triplesOf g1))
--   putStrLn $ "RDF contains literal 'foo': " ++ show (rdfContainsNode g1 node1)
--   putStrLn $ "Isomorphism test: " ++ show (isIsomorphic g1 g2)
--   putStrLn $ "Unsorted triples: " ++ show (triplesOf g2)
--   putStrLn $ "Sorted triples: "   ++ show ((sort . triplesOf) g2)
--   putStrLn $ "Query: " ++ show (query g1 Nothing Nothing (Just node1))
