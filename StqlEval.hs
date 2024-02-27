module StqlEval where
import StqlGrammar
import StqlLogic
import Data.Maybe (fromJust)
import Swish.RDF.Graph (RDFLabel, toRDFLabel)
import Network.URI (URI, parseURI)

{-
new substituting triple
we don't know the types of the triple's elements, but we know they belong to the same class; the existential type packs all those class types into one
type SubTriple = (Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple)
-}

{-
REF1
wrap LabelType in an existential type
data LabelTypeSubTriple = forall a. LabelType a => LabelTypeSubTriple a | Incr Integer
type LabelTypeTuple = (Category, RDFLabel)
-}

-- Stql Category to Swish backend category
catToCat :: StqlCat -> Category
catToCat SubjCat = Subj
catToCat PredCat = Pred
catToCat ObjCat = Obj

strToLB :: StqlLit -> String -> RDFLabel
strToLB LitStr s = toRDFLabel (fromJust $ parseURI s)
strToLB LitNum s = toRDFLabel (read s::Int)
strToLB LitBool "true" = toRDFLabel True
strToLB LitBool "false" = toRDFLabel False

-- execute single expr
exec :: StqlExp -> IO ()
exec (New s) = writeFile s ""
exec (Print s) = printFile s

-- <FilterByCategory> <Subj|Pred|Obj> <String:inputFilePath> <ComparatorEquivalence> <Subj|Pred|Obj> <String:inputFilePath> <String:><String:outputFilePath>
exec (FilterC c0 s0 CompEQ c1 s1 out) = _unwrap2 (compareGraphs (catToCat c0) (catToCat c1)) s0 s1 out

-- <FilterByLiteral> <Subj|Pred|Obj> <String:inputFilePath> <ComparatorEquivalence> <Subj|Pred|Obj> <String:literal> <String:outputFilePath>
exec (FilterL c0 s0 CompEQ l0 s1 out) = _unwrap1 (filterWithLabelTypes (catToCat c0) (strToLB l0 s1)) s0 out

-- <FilterByNumeric> <Obj> <String:inputFilePath> <ComparatorEquivalence> <Subj|Pred|Obj> <String:inputFilePath> <String:outputFilePath>
exec (FilterL ObjCat s0 CompLT LitNum s1 out) = _unwrap1 (filterLTGT (read s1::Integer) (False, False)) s0 out
exec (FilterL ObjCat s0 CompLTE LitNum s1 out) = _unwrap1 (filterLTGT (read s1::Integer) (False, True)) s0 out
exec (FilterL ObjCat s0 CompGT LitNum s1 out) = _unwrap1 (filterLTGT (read s1::Integer) (True, False)) s0 out
exec (FilterL ObjCat s0 CompGTE LitNum s1 out) = _unwrap1 (filterLTGT (read s1::Integer) (True, True)) s0 out

-- <Merge> <Strings[2-3]:inputFilePath> <String:outputFilePath>
exec (Merge2 s0 s1 out) = _unwrap2 mergeGraphs s0 s1 out
exec (Merge3 s0 s1 s2 out) = _unwrap3 mergeMultiple s0 s1 s2 out

-- <SetAll> <Subj/Pred> <String:inputFilepath> <LitStr|LitNum|LitBool> <String:literal> <String: outputFilePath>
exec (SetAll SubjCat s0 l s1 out) = _unwrap1 (editFullGraphs (Just (LabelTypeSubTriple s1), Nothing, Nothing)) s0 out
exec (SetAll PredCat s0 l s1 out) = _unwrap1 (editFullGraphs (Nothing, Just (LabelTypeSubTriple s1), Nothing)) s0 out

-- <SetAll> <Obj> <String:inputFilepath> <LitStr|LitNum|LitBool> <String:literal> <String: outputFilePath>
exec (SetAll ObjCat s0 LitStr s1 out) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (LabelTypeSubTriple s1))) s0 out
exec (SetAll ObjCat s0 LitNum s1 out) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (LabelTypeSubTriple s1))) s0 out

-- handing bool special case
exec (SetAll ObjCat s0 LitBool "true" out) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (LabelTypeSubTriple True))) s0 out
exec (SetAll ObjCat s0 LitBool "false" out) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (LabelTypeSubTriple False))) s0 out

-- <IncrAll> <Obj> <String:inputFilepath> <LitNum> <String:literal> <String: outputFilePath>
exec (IncrAll ObjCat s0 LitNum s1 out) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (Incr (read s1)))) s0 out
