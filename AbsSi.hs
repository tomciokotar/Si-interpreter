module AbsSi where

-- Haskell module generated by the BNF converter


newtype Ident = Ident String deriving (Eq,Ord,Show)
data Program =
   Prog [Stm]
  deriving (Eq,Ord,Show)

data Dec =
   DFunc Type Ident [Param] [Stm]
 | DVar Type Ident
 | DInit Type Ident Exp
 | DAuto Ident Exp
 | DArr Type Ident Exp
 | DStruct Ident [Dec]
  deriving (Eq,Ord,Show)

data Param =
   Param ParamType Ident
  deriving (Eq,Ord,Show)

data ParamType =
   ParamTypeVal Type
 | ParamTypeRef Type
  deriving (Eq,Ord,Show)

data Stm =
   SDec Dec
 | SExp Exp
 | SBlock [Stm]
 | SIf Exp Stm
 | SIfElse Exp Stm Stm
 | SWhile Exp Stm
 | SFor Dec Exp Exp Stm
 | SRet
 | SRetExp Exp
 | SPrint Exp
 | SPrintLn Exp
  deriving (Eq,Ord,Show)

data Exp =
   EAss Var Exp
 | EIncL Var
 | EIncR Var
 | EDecL Var
 | EDecR Var
 | EAddAss Var Exp
 | ESubAss Var Exp
 | EMulAss Var Exp
 | EDivAss Var Exp
 | EModAss Var Exp
 | ENot Exp
 | EAnd Exp Exp
 | EOr Exp Exp
 | EEq Exp Exp
 | ENeq Exp Exp
 | ELt Exp Exp
 | EElt Exp Exp
 | EGt Exp Exp
 | EEgt Exp Exp
 | EConc Exp Exp
 | EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EMod Exp Exp
 | EInt Integer
 | ETrue
 | EFalse
 | EChar Char
 | EStr String
 | EVar Var
 | ECall Var [Exp]
 | EFunc Type [Param] [Stm]
 | EArr [Exp]
 | EIntToChar Exp
 | ECharToInt Exp
  deriving (Eq,Ord,Show)

data Type =
   TInt
 | TBool
 | TChar
 | TString
 | TVoid
 | TArr Type
 | TStruct Ident
 | TFunc Type [ParamType]
  deriving (Eq,Ord,Show)

data Var =
   VarArr Var Exp
 | VarStruct Var Ident
 | VarIdent Ident
  deriving (Eq,Ord,Show)

