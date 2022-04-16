module Evaluator where
import qualified AST

-- REF1 (REF for reference), Book: Alex and Happy Lexers and Parsers in Haskell
data Value =
  VBool Bool
  |VInt Integer
   deriving Show
            
eval::AST.Term -> Maybe Value

eval AST.STrue = Just (VBool True)
eval AST.SFalse = Just (VBool False)
-- eval AST.SZero = Just (VInt 0)

-- eval (AST.SIsZero t) = 
--   case eval t of
--     Just (VInt i) -> Just (VBool (i == 0))
--     _ -> Nothing

-- eval (AST.SSucc t) =
--   case eval t of
--     Just (VInt i) -> Just (VInt (i+1))
--     _ -> Nothing

-- eval (AST.SPred t) = 
--   case eval t of
--     Just (VInt i) | i>0 -> Just(VInt (i-1))
--     _ -> Nothing

-- eval (AST.SIfThen t1 t2 t3) =
--   case eval t1 of
--     Just (VBool True) -> eval t2
--     Just (VBool False) -> eval t3
--     _ -> Nothing
--END OF REF1

-- REFERENCES
-- REF 1, Book: Alex and Happy Lexers and Parsers in Haskell by Jyotirmoy Bhattacharya, accessed April 2022: https://leanpub.com/alexandhappy/read#leanpub-auto-monadic-parsers-and-lexers