module StqlEval where
import Parser
import Stql-Logic

-- Stuff in Parser
-- data StqlCat = SubjCat | PredCat | ObjCat deriving (Show, Eq)

-- data StqlLit = LitStr | LitNum | LitBool deriving (Show, Eq)

-- data StqlComp = CompEQ | CompGT | CompLT | CompGTE | CompLTE deriving (Show, Eq)

-- data StqlFilter = StqlCat String StqlComp StqlCat String
--                   | StqlCat String StqlComp StqlLit String deriving (Show, Eq)

-- data StqlExp = New String | Let String String
--                | FilterBy StqlFilter String
--                | FilterByAnd StqlFilter StqlFilter String
--                | FilterByOr StqlFilter StqlFilter String
--                | FilterByAndAnd StqlFilter StqlFilter String
--                | FilterByOrOr StqlFilter StqlFilter String
--                | Merge2 String String String
--                | Merge3 String String String String
--                | SetAll StqlCat String StqlLit String
--                | IncrAll ObjCat Strig LitNum String


-- Stuff in Stql-Logic

-- data Category = Subj | Pred | Obj
-- data Combinator = And | Or
-- -- direction of filtering a graph's triples' objects with two numbered literals
-- data Direction = In | Out

-- -- new substituting triple
-- -- we don't know the types of the triple's elements, but we know they belong to the same class; the existential type packs all those class types into one
-- type SubTriple = (Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple, Maybe LabelTypeSubTriple)

-- -- REF1
-- -- wrap LabelType in an existential type
-- data LabelTypeSubTriple = forall a. LabelType a => LabelTypeSubTriple a | Incr Integer

-- type LabelTypeTuple = (Category, RDFLabel)

catToCat :: StqlCat -> Category
catToCat Subj = Subj
catToCat Pred = Pred
catToCat Obj = Obj

strToLB :: StqlLit -> String -> RDFLabel
strToLB LitStr s =  s
strToLB LitNum s = read s
strToLB LitBool "TRUE" = True
strToLB LitBool "FALSE" = True

compToBools :: StqlComp -> (Bool, Bool)
compToBools CompLT = (False, False)
compToBools CompLTE = (False, True)
compToBools CompGT = (True, False)
compToBools CompGTE = (True, True)

exec :: StqlExp -> IO ()
exec (New s) = emptyFile s

exec (FilterBy (c0 s0 CompEQ c1 s1) out) = _unwrap2 (compareGraphs (catToCat c0) (catToCat c1)) s0 s1 out
exec (FilterBy (c s0 CompEQ l s1) out) = _unwrap1 (handleFilterLabelTypes (catToCat c) (strToLB l s1)) s0 out
exec (FilterBy (Obj s0 comp LitNum s1) out) = _unwrap1 (filterByLTGT (read s1) (compToBools comp)) s0 out

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

exec (Merge2 s0 s1 out) = _unwrap2 mergeGraphs s0 s1 out
exec (Merge3 s0 s1 s2 out) = _unwrap3 mergeMultiple s0 s1 s2 out

exec (SetAll Subj s0 l s1) = _unwrap1 (editFullGraphs (Just (LabelTypeSubTriple (strToLB l s1)), Nothing, Nothing) s0 s0
exec (SetAll Pred s0 l s1) = _unwrap1 (editFullGraphs (Nothing, Just (LabelTypeSubTriple (strToLB l s1)), Nothing) s0 s0
exec (SetAll Obj s0 l s1) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (LabelTypeSubTriple (strToLB l s1))) s0 s0

exec (IncrAll Subj s0 LitNum s1) = _unwrap1 (editFullGraphs (Just (Incr (read s1)), Nothing, Nothing)) s0 s0
exec (IncrAll Pred s0 LitNum s1) = _unwrap1 (editFullGraphs (Nothing, Just (Incr (read s1)), Nothing)) s0 s0
exec (IncrAll Obj s0 LitNum s1) = _unwrap1 (editFullGraphs (Nothing, Nothing, Just (Incr (read s1)))) s0 s0

