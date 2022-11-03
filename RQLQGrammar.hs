{-# OPTIONS_GHC -w #-}
module RQLQGrammar where 
import RQLQTokens 
import Data.List
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (RQLQToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,115) ([32768,195,0,7168,6,0,0,0,0,0,1024,0,0,32,0,0,1,0,2048,0,0,64,0,0,0,1564,0,0,0,0,0,0,0,128,0,0,0,0,0,1024,0,0,0,0,49152,0,0,0,0,3584,0,0,0,0,0,4096,0,0,0,0,32,0,0,0,64,0,56,0,0,4096,0,0,128,0,0,4,0,0,0,0,256,0,224,0,0,16384,0,0,0,0,0,0,0,31873,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,256,0,0,8192,0,384,505,0,0,0,0,15936,0,0,512,0,0,8,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,2048,0,4096,0,0,32768,0,0,0,0,0,7,0,0,4,0,36864,15,0,0,0,0,1,0,8192,31,0,0,0,0,2,0,16384,62,0,2048,0,0,3984,0,0,2,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseQuery","QueList","Que","SelectQue","GeneralWhereQue","WhereQue","NormalWhereQue","GeneralUpdateQue","UpdateQue","PrintQue","CloneQue","Literal","Triplets","SELECT","WHERE","PRINT","IS","AS","AND","OR","UPDATE","CLONE","BETWEEN","NOT","TO","subject","predicate","object","semiColon","comma","dollar","paren","lBracket","rBracket","minus","plus","true","false","int","str","%eof"]
        bit_start = st Prelude.* 43
        bit_end = (st Prelude.+ 1) Prelude.* 43
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..42]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (16) = happyShift action_3
action_0 (17) = happyShift action_4
action_0 (18) = happyShift action_5
action_0 (23) = happyShift action_6
action_0 (24) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_9
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_3
action_1 (17) = happyShift action_4
action_1 (18) = happyShift action_5
action_1 (23) = happyShift action_6
action_1 (24) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 (42) = happyShift action_22
action_3 (6) = happyGoto action_21
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (42) = happyShift action_20
action_4 (7) = happyGoto action_17
action_4 (8) = happyGoto action_18
action_4 (9) = happyGoto action_19
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (42) = happyShift action_16
action_5 (12) = happyGoto action_15
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (42) = happyShift action_14
action_6 (10) = happyGoto action_13
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (42) = happyShift action_12
action_7 (13) = happyGoto action_11
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (43) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (16) = happyShift action_3
action_9 (17) = happyShift action_4
action_9 (18) = happyShift action_5
action_9 (23) = happyShift action_6
action_9 (24) = happyShift action_7
action_9 (4) = happyGoto action_10
action_9 (5) = happyGoto action_9
action_9 _ = happyReduce_1

action_10 _ = happyReduce_2

action_11 _ = happyReduce_7

action_12 (20) = happyShift action_31
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_5

action_14 (33) = happyShift action_30
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_6

action_16 (31) = happyShift action_28
action_16 (32) = happyShift action_29
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_4

action_18 (20) = happyShift action_25
action_18 (21) = happyShift action_26
action_18 (22) = happyShift action_27
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_11

action_20 (33) = happyShift action_24
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_3

action_22 (20) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (42) = happyShift action_43
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (28) = happyShift action_35
action_24 (29) = happyShift action_36
action_24 (30) = happyShift action_37
action_24 (15) = happyGoto action_42
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (42) = happyShift action_41
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (42) = happyShift action_20
action_26 (8) = happyGoto action_40
action_26 (9) = happyGoto action_19
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (42) = happyShift action_20
action_27 (8) = happyGoto action_39
action_27 (9) = happyGoto action_19
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_21

action_29 (42) = happyShift action_16
action_29 (12) = happyGoto action_38
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_35
action_30 (29) = happyShift action_36
action_30 (30) = happyShift action_37
action_30 (11) = happyGoto action_33
action_30 (15) = happyGoto action_34
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (42) = happyShift action_32
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_23

action_33 _ = happyReduce_18

action_34 (27) = happyShift action_47
action_34 (34) = happyShift action_48
action_34 (37) = happyShift action_49
action_34 (38) = happyShift action_50
action_34 (39) = happyShift action_51
action_34 (40) = happyShift action_52
action_34 (41) = happyShift action_53
action_34 (14) = happyGoto action_46
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_30

action_36 _ = happyReduce_31

action_37 _ = happyReduce_32

action_38 _ = happyReduce_22

action_39 _ = happyReduce_12

action_40 _ = happyReduce_13

action_41 _ = happyReduce_10

action_42 (19) = happyShift action_45
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (32) = happyShift action_44
action_43 _ = happyReduce_8

action_44 (42) = happyShift action_22
action_44 (6) = happyGoto action_62
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (25) = happyShift action_59
action_45 (26) = happyShift action_60
action_45 (34) = happyShift action_48
action_45 (37) = happyShift action_49
action_45 (38) = happyShift action_50
action_45 (39) = happyShift action_51
action_45 (40) = happyShift action_52
action_45 (41) = happyShift action_53
action_45 (42) = happyShift action_61
action_45 (14) = happyGoto action_58
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_20

action_47 (34) = happyShift action_48
action_47 (37) = happyShift action_49
action_47 (38) = happyShift action_50
action_47 (39) = happyShift action_51
action_47 (40) = happyShift action_52
action_47 (41) = happyShift action_53
action_47 (14) = happyGoto action_57
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (42) = happyShift action_56
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (41) = happyShift action_55
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (41) = happyShift action_54
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_28

action_52 _ = happyReduce_29

action_53 _ = happyReduce_24

action_54 _ = happyReduce_26

action_55 _ = happyReduce_25

action_56 (34) = happyShift action_66
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_19

action_58 _ = happyReduce_14

action_59 (35) = happyShift action_65
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (25) = happyShift action_64
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (33) = happyShift action_63
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_9

action_63 (28) = happyShift action_35
action_63 (29) = happyShift action_36
action_63 (30) = happyShift action_37
action_63 (15) = happyGoto action_69
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (35) = happyShift action_68
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (34) = happyShift action_48
action_65 (37) = happyShift action_49
action_65 (38) = happyShift action_50
action_65 (39) = happyShift action_51
action_65 (40) = happyShift action_52
action_65 (41) = happyShift action_53
action_65 (14) = happyGoto action_67
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_27

action_67 (32) = happyShift action_71
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (34) = happyShift action_48
action_68 (37) = happyShift action_49
action_68 (38) = happyShift action_50
action_68 (39) = happyShift action_51
action_68 (40) = happyShift action_52
action_68 (41) = happyShift action_53
action_68 (14) = happyGoto action_70
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_15

action_70 (32) = happyShift action_73
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (34) = happyShift action_48
action_71 (37) = happyShift action_49
action_71 (38) = happyShift action_50
action_71 (39) = happyShift action_51
action_71 (40) = happyShift action_52
action_71 (41) = happyShift action_53
action_71 (14) = happyGoto action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (36) = happyShift action_75
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (34) = happyShift action_48
action_73 (37) = happyShift action_49
action_73 (38) = happyShift action_50
action_73 (39) = happyShift action_51
action_73 (40) = happyShift action_52
action_73 (41) = happyShift action_53
action_73 (14) = happyGoto action_74
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (36) = happyShift action_76
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_16

action_76 _ = happyReduce_17

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1:happy_var_2)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Select happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Where happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Update happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Print happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Clone happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyTerminal (TokenString happy_var_3))
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn6
		 ([(happy_var_3,happy_var_1)]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 5 6 happyReduction_9
happyReduction_9 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (((happy_var_3,happy_var_1):happy_var_5)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyTerminal (TokenString happy_var_3))
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ((happy_var_3,happy_var_1)
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (NormalWhereRequest happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (OrWhereRequest happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (AndWhereRequest happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 9 happyReduction_14
happyReduction_14 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (IsLit (happy_var_1, happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 7 9 happyReduction_15
happyReduction_15 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Is (happy_var_1, happy_var_3) (happy_var_5, happy_var_7)
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 10 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (IsBetween (happy_var_1, happy_var_3) (happy_var_7, happy_var_9)
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 11 9 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (IsNotBetween (happy_var_1, happy_var_3) (happy_var_8, happy_var_10)
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  10 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn10
		 ((happy_var_1, happy_var_3)
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn11
		 (NormalUpdate (happy_var_1, happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  11 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn11
		 (CalcUpdate (happy_var_1, happy_var_2)
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  12 happyReduction_21
happyReduction_21 _
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1:happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyTerminal (TokenString happy_var_3))
	_
	(HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn13
		 ((happy_var_1, happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn14
		 (QInt happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 (HappyTerminal (TokenInt happy_var_2))
	_
	 =  HappyAbsSyn14
		 (QMinusInt happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 (HappyTerminal (TokenInt happy_var_2))
	_
	 =  HappyAbsSyn14
		 (QPlusInt happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 _
	(HappyTerminal (TokenString happy_var_2))
	_
	 =  HappyAbsSyn14
		 (QString happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn14
		 (QBool True
	)

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn14
		 (QBool False
	)

happyReduce_30 = happySpecReduce_1  15 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn15
		 (Subject
	)

happyReduce_31 = happySpecReduce_1  15 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn15
		 (Predicate
	)

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn15
		 (Object
	)

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenSelect -> cont 16;
	TokenWhere -> cont 17;
	TokenPrint -> cont 18;
	TokenIs -> cont 19;
	TokenAs -> cont 20;
	TokenAnd -> cont 21;
	TokenOr -> cont 22;
	TokenUpdate -> cont 23;
	TokenClone -> cont 24;
	TokenBetween -> cont 25;
	TokenNot -> cont 26;
	TokenTo -> cont 27;
	TokenSubject -> cont 28;
	TokenPredicate -> cont 29;
	TokenObject -> cont 30;
	TokenSemiColon -> cont 31;
	TokenComma -> cont 32;
	TokenDollar -> cont 33;
	TokenParen -> cont 34;
	TokenLBracket -> cont 35;
	TokenRBracket -> cont 36;
	TokenMinus -> cont 37;
	TokenPlus -> cont 38;
	TokenTrue -> cont 39;
	TokenFalse -> cont 40;
	TokenInt happy_dollar_dollar -> cont 41;
	TokenString happy_dollar_dollar -> cont 42;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 43 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(RQLQToken)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseQuery tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [RQLQToken] -> a
parseError ts = error ("Error on tokens: " ++ (show ts))


data Query = Select [(String, String)]
           | Where (String, WhereType)
           | Update (String, UpdateType)
           | Print [String]
           | Clone (String, String)
        deriving Show

data WhereType = NormalWhereRequest ConditionalType
               | OrWhereRequest WhereType WhereType
               | AndWhereRequest WhereType WhereType
            deriving Show

data ConditionalType = Is (String, Triplet) (String, Triplet)
                     | IsLit (String, Triplet) LiteralType
                     | IsBetween (String, Triplet) (LiteralType, LiteralType)
                     | IsNotBetween (String, Triplet) (LiteralType, LiteralType)
                deriving Show

data UpdateType = NormalUpdate (Triplet, LiteralType)
                | CalcUpdate (Triplet, LiteralType)
           deriving Show

data Triplet = Subject
             | Predicate
             | Object
           deriving Show

data LiteralType = QInt Int
                 | QMinusInt Int
                 | QPlusInt Int
                 | QString String
                 | QBool Bool
               deriving Show
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
