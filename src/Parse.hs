{-# OPTIONS_GHC -w #-}
module Parse where
import AST
import Data.Maybe
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
	= HappyTerminal (Token)
	| HappyErrorToken Int
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
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,189) ([0,1024,31,0,0,1024,31,0,0,1024,31,0,0,32,0,0,0,0,0,4096,0,0,0,8192,0,0,0,16384,0,0,0,8192,0,0,0,4096,0,0,0,0,0,1024,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,8192,0,0,0,4096,0,0,0,2048,0,0,0,0,0,0,0,0,0,2048,0,0,0,8,0,0,0,0,0,8192,0,64,0,0,0,128,0,0,0,4096,0,0,0,16384,0,0,0,16,0,0,0,0,0,4096,0,8192,0,0,0,16,0,0,0,0,0,4096,0,256,0,0,0,16,0,0,0,0,0,0,0,0,0,4096,0,8,0,0,0,0,0,8192,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,2048,0,32768,0,4096,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,4096,0,16,0,0,0,0,0,0,0,32,13056,9223,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,8192,0,512,0,24,0,0,49152,992,0,32,13056,9223,0,0,1024,0,0,0,1024,0,0,0,0,4096,0,0,0,4096,0,32,13056,9223,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2176,0,0,0,2176,0,0,0,0,8192,0,0,0,8192,0,64,0,24,0,64,49152,992,0,32,12288,9216,0,32,12288,9216,0,32,12288,9216,0,32,12288,9216,0,0,0,8192,0,32,12288,9216,0,32,12288,9216,0,0,96,0,0,32,13056,9223,0,32,13056,9223,0,0,0,8192,0,0,0,0,0,0,0,24,0,0,0,24,0,0,0,0,0,0,0,4096,0,0,0,4096,0,0,0,480,0,32,12288,9216,0,0,0,0,0,0,0,0,0,0,0,384,0,0,0,384,0,0,0,480,0,0,0,480,0,0,0,0,0,0,0,0,0,8,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,8192,0,0,0,4096,0,0,0,4096,0,64,0,480,0,32,12288,9216,0,0,0,0,0,0,0,480,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseSim","Comms","DefComm","DefAttributes","Attributes","Attribute","DefStates","States","State","DefColor","DefRules","Rules","Rule","Result","BoolExp","IntExp","Neighbors","'='","':'","'('","')'","'{'","'}'","'->'","'define'","'sight'","'attributes'","'rules'","'states'","'makeColor'","'setAgent'","'start'","'startPath'","'setIterations'","'unsetAgent'","'newState'","'changeAttribute'","'allNeighs'","'type'","'status'","'neigh'","'neighs'","'countStatus'","'countTypes'","'<'","'>'","'not'","'true'","'false'","'and'","'or'","'+'","'-'","'/'","'*'","'=='","'attribute'","','","STRING","NUM","FILE","%eof"]
        bit_start = st * 64
        bit_end = (st + 1) * 64
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..63]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (27) = happyShift action_3
action_0 (33) = happyShift action_4
action_0 (34) = happyShift action_5
action_0 (35) = happyShift action_6
action_0 (36) = happyShift action_7
action_0 (37) = happyShift action_8
action_0 (4) = happyGoto action_9
action_0 (5) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (27) = happyShift action_3
action_1 (33) = happyShift action_4
action_1 (34) = happyShift action_5
action_1 (35) = happyShift action_6
action_1 (36) = happyShift action_7
action_1 (37) = happyShift action_8
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (27) = happyShift action_3
action_2 (33) = happyShift action_4
action_2 (34) = happyShift action_5
action_2 (35) = happyShift action_6
action_2 (36) = happyShift action_7
action_2 (37) = happyShift action_8
action_2 (4) = happyGoto action_11
action_2 (5) = happyGoto action_10
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (22) = happyShift action_17
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (61) = happyShift action_16
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (62) = happyShift action_15
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (63) = happyShift action_14
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (62) = happyShift action_13
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (61) = happyShift action_12
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (64) = happyAccept
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (27) = happyShift action_3
action_10 (33) = happyShift action_4
action_10 (34) = happyShift action_5
action_10 (35) = happyShift action_6
action_10 (36) = happyShift action_7
action_10 (37) = happyShift action_8
action_10 (4) = happyGoto action_11
action_10 (5) = happyGoto action_10
action_10 _ = happyReduce_2

action_11 _ = happyReduce_1

action_12 _ = happyReduce_8

action_13 _ = happyReduce_5

action_14 _ = happyReduce_7

action_15 (62) = happyShift action_20
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (62) = happyShift action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (61) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (60) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_4

action_20 _ = happyReduce_6

action_21 (28) = happyShift action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (20) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (62) = happyShift action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (23) = happyShift action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (24) = happyShift action_26
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (29) = happyShift action_28
action_26 (6) = happyGoto action_27
action_26 _ = happyReduce_10

action_27 (31) = happyShift action_31
action_27 (9) = happyGoto action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (21) = happyShift action_29
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (61) = happyShift action_37
action_29 (7) = happyGoto action_35
action_29 (8) = happyGoto action_36
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (30) = happyShift action_34
action_30 (13) = happyGoto action_33
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (21) = happyShift action_32
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (61) = happyShift action_44
action_32 (10) = happyGoto action_42
action_32 (11) = happyGoto action_43
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (25) = happyShift action_41
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (21) = happyShift action_40
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_9

action_36 (61) = happyShift action_37
action_36 (7) = happyGoto action_39
action_36 (8) = happyGoto action_36
action_36 _ = happyReduce_12

action_37 (20) = happyShift action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (62) = happyShift action_52
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_11

action_40 (61) = happyShift action_51
action_40 (14) = happyGoto action_49
action_40 (15) = happyGoto action_50
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_3

action_42 _ = happyReduce_14

action_43 (60) = happyShift action_48
action_43 _ = happyReduce_16

action_44 (32) = happyShift action_46
action_44 (61) = happyShift action_47
action_44 (12) = happyGoto action_45
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_17

action_46 (62) = happyShift action_56
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_18

action_48 (61) = happyShift action_44
action_48 (10) = happyGoto action_55
action_48 (11) = happyGoto action_43
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_20

action_50 (61) = happyShift action_51
action_50 (14) = happyGoto action_54
action_50 (15) = happyGoto action_50
action_50 _ = happyReduce_22

action_51 (21) = happyShift action_53
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_13

action_53 (22) = happyShift action_60
action_53 (41) = happyShift action_61
action_53 (42) = happyShift action_62
action_53 (45) = happyShift action_63
action_53 (46) = happyShift action_64
action_53 (49) = happyShift action_65
action_53 (50) = happyShift action_66
action_53 (51) = happyShift action_67
action_53 (59) = happyShift action_68
action_53 (62) = happyShift action_69
action_53 (17) = happyGoto action_58
action_53 (18) = happyGoto action_59
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_21

action_55 _ = happyReduce_15

action_56 (62) = happyShift action_57
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (62) = happyShift action_88
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (26) = happyShift action_85
action_58 (52) = happyShift action_86
action_58 (53) = happyShift action_87
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (47) = happyShift action_78
action_59 (48) = happyShift action_79
action_59 (54) = happyShift action_80
action_59 (55) = happyShift action_81
action_59 (56) = happyShift action_82
action_59 (57) = happyShift action_83
action_59 (58) = happyShift action_84
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (22) = happyShift action_60
action_60 (41) = happyShift action_61
action_60 (42) = happyShift action_62
action_60 (45) = happyShift action_63
action_60 (46) = happyShift action_64
action_60 (49) = happyShift action_65
action_60 (50) = happyShift action_66
action_60 (51) = happyShift action_67
action_60 (59) = happyShift action_68
action_60 (62) = happyShift action_69
action_60 (17) = happyGoto action_76
action_60 (18) = happyGoto action_77
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (43) = happyShift action_75
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (43) = happyShift action_74
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (61) = happyShift action_73
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (61) = happyShift action_72
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (22) = happyShift action_60
action_65 (41) = happyShift action_61
action_65 (42) = happyShift action_62
action_65 (45) = happyShift action_63
action_65 (46) = happyShift action_64
action_65 (49) = happyShift action_65
action_65 (50) = happyShift action_66
action_65 (51) = happyShift action_67
action_65 (59) = happyShift action_68
action_65 (62) = happyShift action_69
action_65 (17) = happyGoto action_71
action_65 (18) = happyGoto action_59
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_26

action_67 _ = happyReduce_27

action_68 (61) = happyShift action_70
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_37

action_70 _ = happyReduce_40

action_71 _ = happyReduce_30

action_72 (40) = happyShift action_108
action_72 (44) = happyShift action_109
action_72 (19) = happyGoto action_110
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (40) = happyShift action_108
action_73 (44) = happyShift action_109
action_73 (19) = happyGoto action_107
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (62) = happyShift action_106
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (62) = happyShift action_105
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (23) = happyShift action_104
action_76 (52) = happyShift action_86
action_76 (53) = happyShift action_87
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (23) = happyShift action_103
action_77 (47) = happyShift action_78
action_77 (48) = happyShift action_79
action_77 (54) = happyShift action_80
action_77 (55) = happyShift action_81
action_77 (56) = happyShift action_82
action_77 (57) = happyShift action_83
action_77 (58) = happyShift action_84
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (22) = happyShift action_96
action_78 (45) = happyShift action_63
action_78 (46) = happyShift action_64
action_78 (59) = happyShift action_68
action_78 (62) = happyShift action_69
action_78 (18) = happyGoto action_102
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (22) = happyShift action_96
action_79 (45) = happyShift action_63
action_79 (46) = happyShift action_64
action_79 (59) = happyShift action_68
action_79 (62) = happyShift action_69
action_79 (18) = happyGoto action_101
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (22) = happyShift action_96
action_80 (45) = happyShift action_63
action_80 (46) = happyShift action_64
action_80 (59) = happyShift action_68
action_80 (62) = happyShift action_69
action_80 (18) = happyGoto action_100
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (22) = happyShift action_96
action_81 (45) = happyShift action_63
action_81 (46) = happyShift action_64
action_81 (59) = happyShift action_68
action_81 (62) = happyShift action_69
action_81 (18) = happyGoto action_99
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (62) = happyShift action_98
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (22) = happyShift action_96
action_83 (45) = happyShift action_63
action_83 (46) = happyShift action_64
action_83 (59) = happyShift action_68
action_83 (62) = happyShift action_69
action_83 (18) = happyGoto action_97
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (22) = happyShift action_96
action_84 (45) = happyShift action_63
action_84 (46) = happyShift action_64
action_84 (59) = happyShift action_68
action_84 (62) = happyShift action_69
action_84 (18) = happyGoto action_95
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (38) = happyShift action_93
action_85 (39) = happyShift action_94
action_85 (16) = happyGoto action_92
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (22) = happyShift action_60
action_86 (41) = happyShift action_61
action_86 (42) = happyShift action_62
action_86 (45) = happyShift action_63
action_86 (46) = happyShift action_64
action_86 (49) = happyShift action_65
action_86 (50) = happyShift action_66
action_86 (51) = happyShift action_67
action_86 (59) = happyShift action_68
action_86 (62) = happyShift action_69
action_86 (17) = happyGoto action_91
action_86 (18) = happyGoto action_59
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (22) = happyShift action_60
action_87 (41) = happyShift action_61
action_87 (42) = happyShift action_62
action_87 (45) = happyShift action_63
action_87 (46) = happyShift action_64
action_87 (49) = happyShift action_65
action_87 (50) = happyShift action_66
action_87 (51) = happyShift action_67
action_87 (59) = happyShift action_68
action_87 (62) = happyShift action_69
action_87 (17) = happyGoto action_90
action_87 (18) = happyGoto action_59
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (62) = happyShift action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_19

action_90 (52) = happyShift action_86
action_90 (53) = happyShift action_87
action_90 _ = happyReduce_29

action_91 (52) = happyShift action_86
action_91 (53) = happyShift action_87
action_91 _ = happyReduce_28

action_92 _ = happyReduce_23

action_93 (61) = happyShift action_116
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (61) = happyShift action_115
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (54) = happyShift action_80
action_95 (55) = happyShift action_81
action_95 (56) = happyShift action_82
action_95 (57) = happyShift action_83
action_95 _ = happyReduce_33

action_96 (22) = happyShift action_96
action_96 (45) = happyShift action_63
action_96 (46) = happyShift action_64
action_96 (59) = happyShift action_68
action_96 (62) = happyShift action_69
action_96 (18) = happyGoto action_114
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_44

action_98 _ = happyReduce_43

action_99 (56) = happyShift action_82
action_99 (57) = happyShift action_83
action_99 _ = happyReduce_42

action_100 (56) = happyShift action_82
action_100 (57) = happyShift action_83
action_100 _ = happyReduce_41

action_101 (54) = happyShift action_80
action_101 (55) = happyShift action_81
action_101 (56) = happyShift action_82
action_101 (57) = happyShift action_83
action_101 _ = happyReduce_35

action_102 (54) = happyShift action_80
action_102 (55) = happyShift action_81
action_102 (56) = happyShift action_82
action_102 (57) = happyShift action_83
action_102 _ = happyReduce_34

action_103 _ = happyReduce_45

action_104 _ = happyReduce_36

action_105 (20) = happyShift action_113
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (20) = happyShift action_112
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_39

action_108 _ = happyReduce_46

action_109 (62) = happyShift action_111
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_38

action_111 (62) = happyShift action_120
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (61) = happyShift action_119
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (61) = happyShift action_118
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (23) = happyShift action_103
action_114 (54) = happyShift action_80
action_114 (55) = happyShift action_81
action_114 (56) = happyShift action_82
action_114 (57) = happyShift action_83
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (22) = happyShift action_96
action_115 (45) = happyShift action_63
action_115 (46) = happyShift action_64
action_115 (59) = happyShift action_68
action_115 (62) = happyShift action_69
action_115 (18) = happyGoto action_117
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_24

action_117 (54) = happyShift action_80
action_117 (55) = happyShift action_81
action_117 (56) = happyShift action_82
action_117 (57) = happyShift action_83
action_117 _ = happyReduce_25

action_118 _ = happyReduce_32

action_119 _ = happyReduce_31

action_120 _ = happyReduce_47

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (SeqComm happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happyReduce 13 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_12) `HappyStk`
	(HappyAbsSyn9  happy_var_11) `HappyStk`
	(HappyAbsSyn6  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNum happy_var_7)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TString happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (DefAgent happy_var_3 happy_var_7 happy_var_10 happy_var_11 happy_var_12
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyTerminal (TNum happy_var_3))
	(HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn5
		 (SetAgent happy_var_2 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyTerminal (TNum happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Iterations happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyTerminal (TNum happy_var_3))
	(HappyTerminal (TNum happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Setup happy_var_2 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyTerminal (TFile happy_var_2))
	_
	 =  HappyAbsSyn5
		 (SetupPath happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  5 happyReduction_8
happyReduction_8 (HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn5
		 (UnsetAgent happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn6
		 (happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  6 happyReduction_10
happyReduction_10  =  HappyAbsSyn6
		 (NoAtt
	)

happyReduce_11 = happySpecReduce_2  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (SeqAtt happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyTerminal (TNum happy_var_3))
	_
	(HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn8
		 (Attribute happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	_
	 =  HappyAbsSyn9
		 (happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (SeqSt happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn11
		 (DefState happy_var_1 happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyTerminal (TString happy_var_1))
	 =  HappyAbsSyn12
		 (ColorName happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 5 12 happyReduction_19
happyReduction_19 ((HappyTerminal (TNum happy_var_5)) `HappyStk`
	(HappyTerminal (TNum happy_var_4)) `HappyStk`
	(HappyTerminal (TNum happy_var_3)) `HappyStk`
	(HappyTerminal (TNum happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (ColorMake happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_3)
	_
	_
	 =  HappyAbsSyn13
		 (happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (Seq happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 15 happyReduction_23
happyReduction_23 ((HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TString happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (DefRule happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2  16 happyReduction_24
happyReduction_24 (HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn16
		 (Left happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn18  happy_var_3)
	(HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn16
		 (Right (happy_var_2,happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn17
		 (ExpTrue
	)

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn17
		 (ExpFalse
	)

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (And happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Or happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  17 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (Not happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 5 17 happyReduction_31
happyReduction_31 ((HappyTerminal (TString happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (EqState happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 17 happyReduction_32
happyReduction_32 ((HappyTerminal (TString happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TNum happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (EqAgent happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_3  17 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  17 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  17 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  18 happyReduction_37
happyReduction_37 (HappyTerminal (TNum happy_var_1))
	 =  HappyAbsSyn18
		 (Const happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn18
		 (TypeCount happy_var_2 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  18 happyReduction_39
happyReduction_39 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn18
		 (StateCount happy_var_2 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  18 happyReduction_40
happyReduction_40 (HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn18
		 (Att happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  18 happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  18 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 (HappyTerminal (TNum happy_var_3))
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Div happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (Times happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  18 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  19 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn19
		 (AllNeighbors
	)

happyReduce_47 = happySpecReduce_3  19 happyReduction_47
happyReduction_47 (HappyTerminal (TNum happy_var_3))
	(HappyTerminal (TNum happy_var_2))
	_
	 =  HappyAbsSyn19
		 (Neighbors happy_var_2 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 64 64 tk (HappyState action) sts stk;
	TEquals -> cont 20;
	TColon -> cont 21;
	TOpen -> cont 22;
	TClose -> cont 23;
	TOpenBracket -> cont 24;
	TCloseBracket -> cont 25;
	TArrow -> cont 26;
	TDefine -> cont 27;
	TSight -> cont 28;
	TAttributes -> cont 29;
	TRules -> cont 30;
	TStates -> cont 31;
	TMakeColor -> cont 32;
	TSetAgent -> cont 33;
	TStart -> cont 34;
	TStartPath -> cont 35;
	TSetIterations -> cont 36;
	TUnsetAgent -> cont 37;
	TNewState -> cont 38;
	TChangeAttribute -> cont 39;
	TAllNeighs -> cont 40;
	TType -> cont 41;
	TStatus -> cont 42;
	TNeigh -> cont 43;
	TNeighs -> cont 44;
	TCountStatus -> cont 45;
	TCountTypes -> cont 46;
	TLt -> cont 47;
	TGt -> cont 48;
	TNot -> cont 49;
	TTrue -> cont 50;
	TFalse -> cont 51;
	TAnd -> cont 52;
	TOr -> cont 53;
	TPlus -> cont 54;
	TMinus -> cont 55;
	TDiv -> cont 56;
	TTimes -> cont 57;
	TIsEqual -> cont 58;
	TAttribute -> cont 59;
	TComma -> cont 60;
	TString happy_dollar_dollar -> cont 61;
	TNum happy_dollar_dollar -> cont 62;
	TFile happy_dollar_dollar -> cont 63;
	_ -> happyError' (tk, [])
	})

happyError_ explist 64 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
parseSim = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data ParseResult a = Ok a | Failed String
                     deriving Show
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TString String
             | TEquals
             | TColon
             | TOpen
             | TClose
             | TOpenBracket
             | TCloseBracket
             | TArrow
             | TDefine
             | TSight
             | TAttributes
             | TRules
             | TSet
             | TUnsetAgent
             | TStart
             | TStartPath
             | TSetIterations
             | TNewState
             | TStates
             | TMakeColor
             | TChangeAttribute
             | TTrue
             | TFalse
             | TNum Int
             | TComma
             | TAllNeighs
             | TType
             | TStatus
             | TNeigh
             | TNeighs
             | TCountStatus
             | TCountTypes
             | TLt
             | TGt
             | TAnd
             | TOr
             | TSetAgent
             | TNot
             | TEOF
             | TFile String
             | TPlus
             | TMinus
             | TDiv
             | TTimes
             | TIsEqual
             | TAttribute
             deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                          | isDigit c -> lexNum (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    ('=':('=':cs)) -> cont TIsEqual cs
                    (')':cs) -> cont TClose cs
                    ('{':cs) -> cont TOpenBracket cs
                    ('}':cs) -> cont TCloseBracket cs
                    (':':cs) -> cont TColon cs
                    (',':cs) -> cont TComma cs
                    ('=':cs) -> cont TEquals cs
                    ('<':cs) -> cont TLt cs
                    ('>':cs) -> cont TGt cs
                    ('+':cs) -> cont TPlus cs
                    ('-':cs) -> cont TMinus cs
                    ('*':cs) -> cont TTimes cs
                    ('/':cs) -> cont TDiv cs
                    ('"':cs) -> lexFile cs
                    unknown -> \line -> Failed $
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlphaNum cs of
                              ("define",rest)  -> cont TDefine rest
                              ("states",rest)  -> cont TStates rest
                              ("makeColor",rest)  -> cont TMakeColor rest
                              ("sight",rest)  -> cont TSight rest
                              ("attributes",rest)  -> cont TAttributes rest
                              ("attribute",rest)  -> cont TAttribute rest
                              ("rules",rest)  -> cont TRules rest
                              ("setAgent",rest)  -> cont TSetAgent rest
                              ("unsetAgent",rest)  -> cont TUnsetAgent rest
                              ("start",rest)  -> cont TStart rest
                              ("startPath",rest)  -> cont TStartPath rest
                              ("setIterations",rest)  -> cont TSetIterations rest
                              ("newState",rest)  -> cont TNewState rest
                              ("changeAttribute",rest)  -> cont TChangeAttribute rest
                              ("allNeighs",rest)  -> cont TAllNeighs rest
                              ("type",rest)  -> cont TType rest
                              ("status",rest)  -> cont TStatus rest
                              ("neigh",rest)  -> cont TNeigh rest
                              ("neighs",rest)  -> cont TNeighs rest
                              ("countStatus",rest)  -> cont TCountStatus rest
                              ("countTypes",rest)  -> cont TCountTypes rest
                              ("not",rest)  -> cont TNot rest
                              ("true",rest)  -> cont TTrue rest
                              ("false",rest)  -> cont TFalse rest
                              ("and",rest)  -> cont TAnd rest
                              ("or",rest)  -> cont TOr rest
                              (str,rest)    -> cont (TString str) rest
                          lexNum cs = let (num,rest) = span isDigit cs
                                      in cont (TNum (read num)) rest
                          lexFile cs = let (file,rest) = span ((/=) '"') cs
                                       in if null file then lexer cont (tail rest)
                                          else cont (TFile file) (tail rest)
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs

sim_parse s = parseSim s 1
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































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
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

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
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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
