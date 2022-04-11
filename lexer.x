{
module Lexer (Token(..),P,evalP,lexer) where
import Control.Monad.State
import Control.Monad.Error
import Data.Word
}

%wrapper "basic"

-- TODO: implement our version
-- $letter = [a-zA-Z]
-- $nonletter = [~$letter\n]

-- tokens :-
--   $nonletter+     ;
--   $letter+        {id}

{
main::IO ()
main = do
  s <- getContents
  let toks = alexScanTokens s
  mapM_ putStrLn toks
}
