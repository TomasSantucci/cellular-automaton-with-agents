{
module Parse where
import AST
import Data.Maybe
import Data.Char
}

%monad { P } { thenP } { returnP }
%name parseSim

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='               { TEquals }
    ':'               { TColon }
    '('               { TOpen }
    ')'               { TClose }
    '{'               { TOpenBracket }
    '}'               { TCloseBracket }
    '->'              { TArrow }
    'define'          { TDefine }
    'sight'           { TSight }
    'attributes'      { TAttributes }
    'rules'           { TRules }
    'states'          { TStates }
    'makeColor'       { TMakeColor }
    'setAgent'        { TSetAgent }
    'start'           { TStart }
    'startPath'       { TStartPath }
    'setIterations'   { TSetIterations}
    'unsetAgent'      { TUnsetAgent }
    'newState'        { TNewState }
    'changeAttribute' { TChangeAttribute }
    'allNeighs'       { TAllNeighs }
    'type'            { TType }
    'status'          { TStatus }
    'neigh'           { TNeigh }
    'neighs'          { TNeighs }
    'countStatus'     { TCountStatus }
    'countTypes'      { TCountTypes }
    '<'               { TLt }
    '>'               { TGt }
    'not'             { TNot }
    'true'            { TTrue }
    'false'           { TFalse }
    'and'             { TAnd }
    'or'              { TOr }
    '+'               { TPlus }
    '-'               { TMinus }
    '/'               { TDiv }
    '*'               { TTimes }
    '=='              { TIsEqual }
    'attribute'       { TAttribute }
    VAR               { TVar $$ }
    NUM               { TNum $$ }
    FILE              { TFile $$ }
    ','               { TComma }

%right 'and' 'or'
%left 'not'
%left '+' '-'
%left '*' '/'

%%

Comms         : DefComm Comms                            { SeqComm $1 $2 }
              | DefComm                                  { $1 }

DefComm       : 'define' '(' VAR ',' 'sight' '=' NUM ')'
                '{' DefAttributes DefStates DefRules '}' { DefAgent $3 $7 $10 $11 $12 }
              | 'setAgent' VAR NUM                       { SetAgent $2 $3 }
              | 'setIterations' NUM                      { Iterations $2 }
              | 'start' NUM NUM                          { Setup $2 $3 }
              | 'startPath' FILE                         { SetupPath $2 }
              | 'unsetAgent' VAR                         { UnsetAgent $2 }

DefAttributes : 'attributes' ':' Attributes              { $3 }
              |                                          { NoAtt }

Attributes    : Attribute Attributes                     { SeqAtt $1 $2 }
              | Attribute                                { $1 }

Attribute     : VAR '=' NUM                              { Attribute $1 $3 }

DefStates     : 'states' ':' States                      { $3 }

States        : State ',' States                         { SeqSt $1 $3 }
              | State                                    { $1 }

State         : VAR DefColor                             { State $1 $2 }

DefColor      : VAR                                      { ColorName $1 }
              | 'makeColor' NUM NUM NUM NUM              { ColorMake $2 $3 $4 $5 }

DefRules      : 'rules' ':' Rules                        { $3 }

Rules         : Rule Rules                               { Seq $1 $2 }
              | Rule                                     { $1 }

Rule          : VAR ':' BoolExp '->' Result              { Transition $1 $3 $5 }

Result        : 'newState' VAR                           { Left $2 }
              | 'changeAttribute' VAR IntExp             { Right ($2,$3) }

BoolExp       : 'true'                                   { ExpTrue }
              | 'false'                                  { ExpFalse }
              | BoolExp 'and' BoolExp                    { And $1 $3 }
              | BoolExp 'or' BoolExp                     { Or $1 $3 }
              | 'not' BoolExp                            { Not $2 }
              | 'status' 'neigh' NUM '=' VAR             { EqState $3 $5 }
              | 'type' 'neigh' NUM '=' VAR               { EqAgent $3 $5 }
              | IntExp '==' IntExp                       { Eq $1 $3 }
              | IntExp '<' IntExp                        { Lt $1 $3 }
              | IntExp '>' IntExp                        { Gt $1 $3 }
              | '(' BoolExp ')'                          { $2 }

IntExp        : NUM                                      { Const $1 }
              | 'countTypes' VAR Neighbors               { TypeCount $2 $3 }
              | 'countStatus' VAR Neighbors              { StateCount $2 $3 }
              | 'attribute' VAR                          { Att $2 }
              | IntExp '+' IntExp                        { Plus $1 $3 }
              | IntExp '-' IntExp                        { Minus $1 $3 }
              | IntExp '/' NUM                           { Div $1 $3 }
              | IntExp '*' IntExp                        { Times $1 $3 }
              | '(' IntExp ')'                           { $2 }

Neighbors     : 'allNeighs'                              { AllNeighbors }
              | 'neighs' NUM NUM                         { Neighbors $2 $3 }


{

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

data Token = TVar String
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
                              (var,rest)    -> cont (TVar var) rest
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
}
