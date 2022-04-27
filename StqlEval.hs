module StqlEval where
import StqlGrammar
import StqlLogic
import Swish.RDF.Graph (RDFLabel, toRDFLabel)

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

catToCat :: StqlCat -> Category
catToCat SubjCat = Subj
catToCat PredCat = Pred
catToCat ObjCat = Obj

strToLB :: StqlLit -> String -> RDFLabel
strToLB LitStr s = toRDFLabel s
strToLB LitNum s = toRDFLabel (read s::String)
strToLB LitBool "TRUE" = toRDFLabel True
strToLB LitBool "FALSE" = toRDFLabel False

compToBools :: StqlComp -> (Bool, Bool)
compToBools CompLT = (False, False)
compToBools CompLTE = (False, True)
compToBools CompGT = (True, False)
compToBools CompGTE = (True, True)

exec :: StqlExp -> IO ()
exec (New s) = writeFile s ""

exec (Print s) = printFile s

exec (FilterC c0 s0 CompEQ c1 s1 out) = _unwrap2 (compareGraphs (catToCat c0) (catToCat c1)) s0 s1 out
exec (FilterL c0 s0 CompEQ l1 s1 out) = _unwrap1 (handleFilterLabelTypes (catToCat c0) (strToLB l1 s1)) s0 out
exec (FilterL ObjCat s0 comp LitNum s1 out) = _unwrap1 (filterLTGT (read s1) (compToBools comp)) s0 out

{-
exec (FilterByAnd (c0 s0 CompEQ c1 s1) (c2 s2 CompEQ c3 s3) out) = _unwrap22 (compareFullGraphs (compareGraphs (catToCat c0) (catToCat c1)) (compareGraphs (catToCat c2) (catToCat c3))) s0 s1 s2 s3 out
exec (FilterByAnd (c0 s0 CompEQ c1 s1) (c2 s2 CompEQ l s3) out) = _unwrap21 (compareFullGraphs (compareGraphs (catToCat c0) (catToCat c1)) (handleFilterLabelTypes (catToCat c2) (strToLB l s3))) s0 s1 s2 out
exec (FilterByAnd (c0 s0 CompEQ c1 s1) (Obj s2 comp LitNum s3) out) = _unwrap21 (compareFullGraphs (compareGraphs (catToCat c0) (catToCat c1)) (filterByLTGT (read s3) (compToBools comp)) s0 s1 s2 out
exec (FilterByAnd (c0 s0 CompEQ l s1) (c1 s2 CompEQ c2 s3) out) = _unwrap21 (compareFullGraphs (compareGraphs (catToCat c0) (catToCat c1)) (handleFilterLabelTypes (catToCat c0) (strToLB l s1))) s0 s2 s3 out
exec (FilterByAnd (c0 s0 CompEQ l0 s1) (c1 s2 CompEQ l1 s3) out) = _unwrap11 (compareFullGraphs (handleFilterLabelTypes (catToCat c0) (strToLB l0 s1)) (handleFilterLabelTypes (catToCat c1) (strToLB l1 s3))) s0 s2 out
exec (FilterByAnd (c s0 CompEQ l s1) (Obj s2 comp LitNum s3) out) = _unwrap11 (compareFullGraphs (handleFilterLabelTypes (catToCat c) (strToLB l s1)) (filterByLTGT (read s3) (compToBools comp))) s0 s2 out
exec (FilterByAnd (Obj s0 comp LitNum s1) (c0 s2 CompEQ c1 s3) out) = _unwrap21 (compareFullGraphs (compareGraphs (catToCat c0) (catToCat c1)) (filterByLTGT (read s1) (compToBools comp))) s0 s2 s3 out
exec (FilterByAnd (Obj s0 comp LitNum s1) (c s2 CompEQ l s3) out) = _unwrap11 (compareFullGraphs (handleFilterLabelTypes (catToCat c) (strToLB l s3)) (filterByLTGT (read s1) (compToBools comp))) s0 s2 out
exec (FilterByAnd (Obj s0 comp0 LitNum s1) (Obj s2 comp1 LitNum s3) out) = _unwrap11 (compareFullGraphs (filterByLTGT (read s1) (compToBools comp0)) (filterByLTGT (read s3) (compToBools comp1))) s0 s2 out

exec (FilterByOr (c0 s0 CompEQ c1 s1) (c2 s2 CompEQ c3 s3) out) = _unwrap22 (mergeGraphs (compareGraphs (catToCat c0) (catToCat c1)) (compareGraphs (catToCat c2) (catToCat c3))) s0 s1 s2 s3 out
exec (FilterByOr (c0 s0 CompEQ c1 s1) (c2 s2 CompEQ l s3) out) = _unwrap21 (mergeGraphs (compareGraphs (catToCat c0) (catToCat c1)) (handleFilterLabelTypes (catToCat c2) (strToLB l s3))) s0 s1 s2 out
exec (FilterByOr (c0 s0 CompEQ c1 s1) (Obj s2 comp LitNum s3) out) = _unwrap21 (mergeGraphs (compareGraphs (catToCat c0) (catToCat c1)) (filterByLTGT (read s3) (compToBools comp)) s0 s1 s2 out
exec (FilterByOr (c0 s0 CompEQ l s1) (c1 s2 CompEQ c2 s3) out) = _unwrap21 (mergeGraphs (compareGraphs (catToCat c0) (catToCat c1)) (handleFilterLabelTypes (catToCat c0) (strToLB l s1))) s0 s2 s3 out
exec (FilterByOr (c0 s0 CompEQ l0 s1) (c1 s2 CompEQ l1 s3) out) = _unwrap11 (mergeGraphs (handleFilterLabelTypes (catToCat c0) (strToLB l0 s1)) (handleFilterLabelTypes (catToCat c1) (strToLB l1 s3))) s0 s2 out
exec (FilterByOr (c s0 CompEQ l s1) (Obj s2 comp LitNum s3) out) = _unwrap11 (mergeGraphs (handleFilterLabelTypes (catToCat c) (strToLB l s1)) (filterByLTGT (read s3) (compToBools comp))) s0 s2 out
exec (FilterByOr (Obj s0 comp LitNum s1) (c0 s2 CompEQ c1 s3) out) = _unwrap21 (mergeGraphs (compareGraphs (catToCat c0) (catToCat c1)) (filterByLTGT (read s1) (compToBools comp))) s0 s2 s3 out
exec (FilterByOr (Obj s0 comp LitNum s1) (c s2 CompEQ l s3) out) = _unwrap11 (mergeGraphs (handleFilterLabelTypes (catToCat c) (strToLB l s3)) (filterByLTGT (read s1) (compToBools comp))) s0 s2 out
exec (FilterByOr (Obj s0 comp0 LitNum s1) (Obj s2 comp1 LitNum s3) out) = _unwrap11 (mergeGraphs (filterByLTGT (read s1) (compToBools comp0)) (filterByLTGT (read s3) (compToBools comp1))) s0 s2 out
-}

exec (Merge2 s0 s1 out) = _unwrap2 mergeGraphs s0 s1 out
exec (Merge3 s0 s1 s2 out) = _unwrap3 mergeMultiple s0 s1 s2 out

-- s0 file path l is the literal type s1 is the actual literals
exec (SetAll SubjCat s0 l s1) = _unwrap1 (editFullGraphs (Just (LabelTypeSubTriple s1), Nothing, Nothing)) s0 s0
exec (SetAll PredCat s0 l s1) = _unwrap1 (editFullGraphs (Nothing, Just (LabelTypeSubTriple s1), Nothing)) s0 s0
exec (SetAll ObjCat s0 l s1) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (LabelTypeSubTriple s1))) s0 s0

exec (IncrAll SubjCat s0 LitNum s1) = _unwrap1 (editFullGraphs (Just (Incr (read s1)), Nothing, Nothing)) s0 s0
exec (IncrAll PredCat s0 LitNum s1) = _unwrap1 (editFullGraphs (Nothing, Just (Incr (read s1)), Nothing)) s0 s0
exec (IncrAll ObjCat s0 LitNum s1) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (Incr (read s1)))) s0 s0
