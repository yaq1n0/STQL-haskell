{-# OPTIONS_GHC -w #-}
module StqlGrammar where
import StqlTokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,78) ([14208,0,8,0,0,2,0,0,512,0,32,448,0,28,49152,1,0,32,0,0,0,0,0,0,32,0,2,0,0,512,0,0,8,2,7936,0,14,57344,0,0,4,0,0,0,0,0,0,4,252,0,0,0,0,0,0,0,0,0,0,32,8,0,8192,0,0,0,64,0,32770,0,2048,0,0,32,0,32770,0,2048,0,0,0,0,2,8192,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseStql","Exp","Comp","Lit","Cat","new","merge","print","filter","to","setall","incrall","subj","pred","obj","str","num","bool","'=='","'>'","'<'","'>='","'<='","path","var","%eof"]
        bit_start = st Prelude.* 28
        bit_end = (st Prelude.+ 1) Prelude.* 28
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..27]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (8) = happyShift action_2
action_0 (9) = happyShift action_4
action_0 (10) = happyShift action_5
action_0 (11) = happyShift action_6
action_0 (13) = happyShift action_7
action_0 (14) = happyShift action_8
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (8) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (26) = happyShift action_17
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (28) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (26) = happyShift action_16
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (26) = happyShift action_15
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (15) = happyShift action_10
action_6 (16) = happyShift action_11
action_6 (17) = happyShift action_12
action_6 (7) = happyGoto action_14
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (15) = happyShift action_10
action_7 (16) = happyShift action_11
action_7 (17) = happyShift action_12
action_7 (7) = happyGoto action_13
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (15) = happyShift action_10
action_8 (16) = happyShift action_11
action_8 (17) = happyShift action_12
action_8 (7) = happyGoto action_9
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (26) = happyShift action_21
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_17

action_11 _ = happyReduce_18

action_12 _ = happyReduce_19

action_13 (26) = happyShift action_20
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (26) = happyShift action_19
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_2

action_16 (26) = happyShift action_18
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_1

action_18 (12) = happyShift action_33
action_18 (26) = happyShift action_34
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (21) = happyShift action_28
action_19 (22) = happyShift action_29
action_19 (23) = happyShift action_30
action_19 (24) = happyShift action_31
action_19 (25) = happyShift action_32
action_19 (5) = happyGoto action_27
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (18) = happyShift action_23
action_20 (19) = happyShift action_24
action_20 (20) = happyShift action_25
action_20 (6) = happyGoto action_26
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (18) = happyShift action_23
action_21 (19) = happyShift action_24
action_21 (20) = happyShift action_25
action_21 (6) = happyGoto action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (27) = happyShift action_40
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_14

action_24 _ = happyReduce_15

action_25 _ = happyReduce_16

action_26 (27) = happyShift action_39
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (15) = happyShift action_10
action_27 (16) = happyShift action_11
action_27 (17) = happyShift action_12
action_27 (18) = happyShift action_23
action_27 (19) = happyShift action_24
action_27 (20) = happyShift action_25
action_27 (6) = happyGoto action_37
action_27 (7) = happyGoto action_38
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_9

action_29 _ = happyReduce_10

action_30 _ = happyReduce_11

action_31 _ = happyReduce_12

action_32 _ = happyReduce_13

action_33 (26) = happyShift action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (12) = happyShift action_35
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (26) = happyShift action_45
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_5

action_37 (27) = happyShift action_44
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (26) = happyShift action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (12) = happyShift action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (12) = happyShift action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (26) = happyShift action_49
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (26) = happyShift action_48
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (12) = happyShift action_47
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (12) = happyShift action_46
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_6

action_46 (26) = happyShift action_51
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (26) = happyShift action_50
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_7

action_49 _ = happyReduce_8

action_50 _ = happyReduce_3

action_51 _ = happyReduce_4

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenPath happy_var_2))
	_
	 =  HappyAbsSyn4
		 (New happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokenPath happy_var_2))
	_
	 =  HappyAbsSyn4
		 (Print happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 8 4 happyReduction_3
happyReduction_3 ((HappyTerminal (TokenPath happy_var_8)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenPath happy_var_6)) `HappyStk`
	(HappyAbsSyn7  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyTerminal (TokenPath happy_var_3)) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (FilterC happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 8 4 happyReduction_4
happyReduction_4 ((HappyTerminal (TokenPath happy_var_8)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_6)) `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyTerminal (TokenPath happy_var_3)) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (FilterL happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 4 happyReduction_5
happyReduction_5 ((HappyTerminal (TokenPath happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenPath happy_var_3)) `HappyStk`
	(HappyTerminal (TokenPath happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Merge2 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 4 happyReduction_6
happyReduction_6 ((HappyTerminal (TokenPath happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenPath happy_var_4)) `HappyStk`
	(HappyTerminal (TokenPath happy_var_3)) `HappyStk`
	(HappyTerminal (TokenPath happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Merge3 happy_var_2 happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 4 happyReduction_7
happyReduction_7 ((HappyTerminal (TokenPath happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_5)) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyTerminal (TokenPath happy_var_3)) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (SetAll happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 7 4 happyReduction_8
happyReduction_8 ((HappyTerminal (TokenPath happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_5)) `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	(HappyTerminal (TokenPath happy_var_3)) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IncrAll happy_var_2 happy_var_3 happy_var_4 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn5
		 (CompEQ
	)

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (CompGT
	)

happyReduce_11 = happySpecReduce_1  5 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn5
		 (CompLT
	)

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn5
		 (CompGTE
	)

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 (CompLTE
	)

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn6
		 (LitStr
	)

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn6
		 (LitNum
	)

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn6
		 (LitBool
	)

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn7
		 (SubjCat
	)

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn7
		 (PredCat
	)

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn7
		 (ObjCat
	)

happyNewToken action sts stk [] =
	action 28 28 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNew -> cont 8;
	TokenMerge -> cont 9;
	TokenPrint -> cont 10;
	TokenFilter -> cont 11;
	TokenTo -> cont 12;
	TokenSetAll -> cont 13;
	TokenIncrAll -> cont 14;
	TokenSubj -> cont 15;
	TokenPred -> cont 16;
	TokenObj -> cont 17;
	TokenString -> cont 18;
	TokenNum -> cont 19;
	TokenBool -> cont 20;
	TokenEQ -> cont 21;
	TokenGT -> cont 22;
	TokenLT -> cont 23;
	TokenGTE -> cont 24;
	TokenLTE -> cont 25;
	TokenPath happy_dollar_dollar -> cont 26;
	TokenVar happy_dollar_dollar -> cont 27;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 28 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseStql tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Unknown Parse Error"

data StqlCat = SubjCat | PredCat | ObjCat deriving (Show, Eq)

data StqlLit = LitStr | LitNum | LitBool deriving (Show, Eq)

data StqlComp = CompEQ | CompGT | CompLT | CompGTE | CompLTE deriving (Show, Eq)

data StqlExp = New String | Print String
               | FilterC StqlCat String StqlComp StqlCat String String
               | FilterL StqlCat String StqlComp StqlLit String String
               | Merge2 String String String
               | Merge3 String String String String
               | SetAll StqlCat String StqlLit String String
               | IncrAll StqlCat String StqlLit String String
               deriving (Show, Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
