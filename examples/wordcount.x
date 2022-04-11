-- "This is where you put the module declaration for the generated code. 
-- This is also where you need to put any import statements that you may need for the rest of your program. 
-- Do not put any other declarations here since Alex will place its own imports after this section."
{
module Main(main) where
}

-- "specifies the kind of interface Alex should provide to the lexical analyser it generates. 
-- The “basic” wrapper is the simplest option. In this case Alex will provide a function alexScanTokens::String->[Token]"
%wrapper "basic"

-- MACRO DEFINITIONS
-- "A macro is a shortcut specifying a set of characters or a regular expressions. 
-- The names of character set macros begin with $ and those of regular expression macros begin with @."
$letter = [a-zA-Z]
--"~$letter denotes the complement of the character set $letter. 
--The universal set in Alex’s negation operator does not contain the newline, so we specify it separately with the escape sequence \n."
$nonletter = [~$letter\n]

-- RULES
-- "Each rule is a regular expression followed by action.
-- Actions can be of two types: either a bare ; or some Haskell code. 
-- If the action is a bare ; then Alex skips the input which matches the rule.
-- If the action is Haskell code, what Alex does depends on the wrapper. 
-- For the basic wrapper, the code for each action must be a function of the type String->Token 
-- which is called by Alex with the matched input to produce a token.
-- 
-- + is the regular expression operator which matches one or more occurrences of the preceding regular expression."
tokens :-
  $nonletter+     ;
  $letter+        {id}

{
main::IO ()
main = do
  s <- getContents
  -- "alexScanTokens' argument is a string which contains the entire input and the result is a list of tokens, where Token is a user-defined type.
  -- here, tokens are just words. The final line of main prints each of these words in a separate line."
  let toks = alexScanTokens s
  mapM_ putStrLn toks
}
