module PP (renderComm) where

import AST
import Text.PrettyPrint
import Prelude hiding ((<>))

tabW = 2

pRes :: Result (Exp Int) -> Doc
pRes (NewState st)             = text "newState" <+> text st
pRes (ChangeAttribute att exp) = text "changeAttribute" <+> text att <+> parens (pExp exp)

pNeighs :: Neighbors -> Doc
pNeighs AllNeighbors    = text "all"
pNeighs (Neighbors n m) = text "neighbors" <+> int n <+> int m

pExp :: Exp a -> Doc
pExp (TypeCount name neighs)   = text "countTypes" <+> text name <+> pNeighs neighs
pExp (StateCount state neighs) = text "countStates" <+> text state <+> pNeighs neighs
pExp (Att att)                 = text "attribute" <+> text att
pExp (Const i)                 = int i
pExp (Plus a b)                = pExp a <+> text "+" <+> pExp b
pExp (Times a b)               = pExp a <+> text "*" <+> pExp b
pExp (Minus a b)               = pExp a <+> text "-" <+> pExp b
pExp (Div a b)                 = pExp a <+> text "/" <+> int b
pExp ExpTrue                   = text "true"
pExp ExpFalse                  = text "false"
pExp (EqState n state)         = text "neighborState" <+> int n <+> text "==" <+> text state
pExp (EqAgent n name)          = text "neighborType" <+> int n <+> text "==" <+> text name
pExp (Eq a b)                  = pExp a <+> text "==" <+> pExp b
pExp (Lt a b)                  = pExp a <+> text "<" <+> pExp b
pExp (Gt a b)                  = pExp a <+> text ">" <+> pExp b
pExp (And a b)                 = pExp a <+> text "and" <+> pExp b
pExp (Or a b)                  = pExp a <+> text "or" <+> pExp b
pExp (Not b)                   = text "not" <+> pExp b

pRules :: RulesComm -> Doc
pRules rules = text "rules:" $$ nest tabW (pRulesAux rules)
  where pRulesAux (Seq r1 r2)           = pRulesAux r1 $$ pRulesAux r2
        pRulesAux (DefRule st bexp res) = text st <+> colon <+> pExp bexp <+> text "->" <+> pRes res

pStates :: StatesComm -> Doc
pStates st = text "states:" $$ nest tabW (pStatesAux st)
  where pStatesAux (SeqSt s1 s2)   = pStatesAux s1 <> comma <+> pStatesAux s2
        pStatesAux (DefState st _) = text st

pAtts :: Attributes -> Doc
pAtts NoAtt = empty
pAtts atts = text "attributes:" $$ nest tabW (pAttsAux atts)
  where pAttsAux (Attribute name value) = text name <+> int value
        pAttsAux (SeqAtt a1 a2)         = pAttsAux a1 <> comma <+> pAttsAux a2

pComm :: Comm -> Doc
pComm (SeqComm c1 c2) = pComm c1 $$ text "" $$ pComm c2
pComm (DefAgent name sight atts states rules)
  = text "define" <+>
    parens (text name <> comma <+> text "sight" <+> int sight) <+>
    lbrace $$
    nest tabW (pStates states) $$
    nest tabW (pAtts atts) $$
    nest tabW (pRules rules) $$
    rbrace

pComm (SetAgent name n) = text "setAgent" <+> text name <+> int n
pComm (UnsetAgent name) = text "unSetAgent" <+> text name
pComm (Iterations n)    = text "iterations" <+> int n
pComm (Setup n m)       = text "start" <+> int n <+> text "x" <+> int m
pComm (SetupPath path)  = text "startPath" <+> text path

renderComm :: Comm -> String
renderComm = render . pComm