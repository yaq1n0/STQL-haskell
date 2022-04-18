# Functions to write for parsing turtle files
- [ ] import file from command line
    - [ ] import file in hardcoded form
- [x] import text into turtle graph
- [ ] be able to manipulate the graph
- [ ] save graph in state maybe?
- [ ] import multiple graphs into state
- [ ] merge graphs (be aware about the clashes; **graphmatch, swishmerge**)
- [ ] filter graphs (select base, prefixes, subject, predicate, object; **fromrdftriple**)
    - [ ] displays all nodes with subjects/objects satisfying a given filter (**allnodes**)
    - [ ] write compare subject,pred, obj functions (**fmapnsgraph**)
- [ ] create new out and out file
- [ ] add graphs to out
- [ ] export (**formatgraphastext**)


separate a turtle file into triples - rdf4h (parsefromfile), swish (parseturtle)
combine graphs - swish (graphmatch, swishmerge)
be able to select base and prefixes - rdf4h (baseUrl, prefixMappings)
be able to easily select subj, pred, object of the triples - rdf4h (subjectOf, predicateof, objectof), swish (fromRDFTriple)
expand triples - rdf4h (expandTriples, resolveQName)
handle name clashes? - rdf4h (compares triples - equalSubjects, equalPredicates, equalObjects)
filter by subj, pred, obj - rdf4h (tripleContainsNode, subjectsWithPredicate, objectsOfPredicate)
validate uRI - rdf4h (validateIRI, IT'S IRI, isUNode (uri)), swish (isURI)
easy way of creating and exporting triples - rdf4h (**addtriple**)
sorting - rdf4h (uordered)
quoting strings - swish (quote)
export triples into a turtle file - rdf4h (writets), swish (formatGraphAsText)
error handling - swish (swishError, setError)