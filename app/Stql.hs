{-
COMP2212 Programming Language Concepts Group Coursework
Charlie Bowe
Yaqin Hasan
Patrycja Wanat
-}
{-# LANGUAGE ScopedTypeVariables,
             OverloadedStrings #-}

module Main where

import System.IO ()  
import Control.Monad ()
import Control.Monad.State.Lazy
  (
    execStateT
  )
import qualified Data.Text as T
-- import qualified Data.RDF.State 
import Data.RDF.State as RDFState
  (
    RdfST,
    addTriple,
    removeTriple,
    unRdfST,
    showGraph
  )
import Data.Text.Lazy as TL ()

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
    objectOf,
    empty,
    triple,
    unode
  )

-- Rdf4h version
importFile :: IO ()
importFile = do
  result <- parseFile (TurtleParser Nothing Nothing) "../inputs/bar.ttl" :: IO (Either ParseFailure (RDF TList))
  print result

main :: IO ()
main = importFile

--REF1, Rdf4h documentation
-- main :: IO ()
-- main = do
--   let myEmptyGraph = empty :: RDF TList
--   newGraph <- execStateT (unRdfST createGraph) myEmptyGraph
--   putStrLn (showGraph newGraph)

-- createGraph :: (Rdf rdfImpl, Monad m) => RdfST rdfImpl m ()
-- createGraph = do
--   -- add a triple to the empty graph
--   let triple1 = triple (unode "http://www.example.com/rob")
--                        (unode "http://xmlns.com/foaf/0.1/interest")
--                        (unode "http://dbpedia.org/resource/Scotch_whisky")
--   RDFState.addTriple triple1

--   -- add another triple to the graph
--   let triple2 = triple (unode "http://www.example.com/rob")
--                        (unode "http://xmlns.com/foaf/0.1/interest")
--                        (unode "http://dbpedia.org/resource/Haskell_(programming_language)")
--   RDFState.addTriple triple2

--   -- remove one of my interests
--   RDFState.removeTriple triple1
--END OF REF1

-- REFERENCES
-- REF 1, Rdf4h documentation: https://hackage.haskell.org/package/rdf4h-5.0.1/docs/Data-RDF-State.html