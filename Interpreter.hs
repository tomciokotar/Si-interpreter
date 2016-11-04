module Interpreter where

import qualified Data.Map as Map
import qualified Data.Char as Char
import Environment
import AbsSi

assignParams :: [Param] -> Env -> [Exp] -> Interp Env

assignParams [] env [] = return env

assignParams ((Param (ParamTypeRef _) (Ident paramName)):params) env ((EVar varName):exps) = do
	varLoc <- getVarLoc varName
	assignParams params (Map.insert paramName varLoc env) exps

assignParams ((Param (ParamTypeVal _) (Ident paramName)):params) env (exp:exps) = do
	expVal <- eval exp
	loc <- newLoc expVal
	assignParams params (Map.insert paramName loc env) exps


getVarLoc :: Var -> Interp Loc

getVarLoc (VarIdent (Ident x)) = readEnv x

getVarLoc (VarArr varName posExp) = do
	(VArr _ locs) <- eval (EVar varName)
	(VInt val) <- eval posExp
	let n = fromInteger val
	if n < 0 || n >= (length locs)
		then fail "index of array is out of bounds"
		else return (locs !! n)

getVarLoc (VarStruct varName (Ident elemName)) = do
	(VStruct _ structEnv) <- eval (EVar varName)
	return (structEnv Map.! elemName)


evalList :: [Exp] -> Interp [Value]
evalList [] = return []
evalList (exp:exps) = do
	val <- eval exp
	vals <- evalList exps
	return (val:vals)


writeToLocs :: [Loc] -> [Value] -> Interp ()
writeToLocs [] [] = return ()
writeToLocs (loc:locs) (val:vals) = do
	writeStore loc val
	writeToLocs locs vals


eval :: Exp -> Interp Value

eval (EInt n) = return (VInt n)

eval ETrue = return (VBool True)

eval EFalse = return (VBool False)

eval (EChar c) = return (VChar c)

eval (EStr str) = return (VString str)

eval (EVar varName) = do
	varLoc <- getVarLoc varName
	readStore varLoc

eval (ECall varName expList) = do
	(VFunc retType paramList env stmList) <- eval (EVar varName)
	funcEnv <- assignParams paramList env expList
	oldEnv <- getEnv
	setEnv funcEnv
	execList stmList
	retVal <- readValue "@retVal"
	setEnv oldEnv
	return retVal

eval (EFunc retType paramList stmList) = do
	oldEnv <- getEnv
	defRetVal <- initDefValue retType
	newValue "@retVal" defRetVal
	newValue "@retDone" (VBool False)
	funcEnv <- getEnv
	setEnv oldEnv
	return (VFunc retType paramList funcEnv stmList)

eval (EArr expList) = do
	valList <- evalList expList
	let typeName = getType (head valList)
	locs <- makeLocs (toInteger (length valList)) typeName
	writeToLocs locs valList
	return (VArr typeName locs)

eval (EConc e1 e2) = do
	v1 <- eval e1
	v2 <- eval e2
	return (VString ((show v1) ++ (show v2)))

eval (EIntToChar e) = do
	(VInt val) <- eval e
	return (VChar (Char.chr (fromInteger val)))

eval (ECharToInt e) = do
	(VChar c) <- eval e
	return (VInt (toInteger (Char.ord c)))

eval (EIncL varName) = eval (EAddAss varName (EInt 1))

eval (EIncR varName) = do
	val <- eval (EVar varName)
	eval (EAddAss varName (EInt 1))
	return val

eval (EDecL varName) = eval (ESubAss varName (EInt 1))

eval (EDecR varName) = do
	val <- eval (EVar varName)
	eval (ESubAss varName (EInt 1))
	return val

eval (EAddAss varName exp) = eval (EAss varName (EAdd (EVar varName) exp))

eval (ESubAss varName exp) = eval (EAss varName (ESub (EVar varName) exp))

eval (EMulAss varName exp) = eval (EAss varName (EMul (EVar varName) exp))

eval (EDivAss varName exp) = eval (EAss varName (EDiv (EVar varName) exp))

eval (EModAss varName exp) = eval (EAss varName (EMod (EVar varName) exp))

eval (EAdd e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VInt (v1 + v2))

eval (ESub e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VInt (v1 - v2))

eval (EMul e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VInt (v1 * v2))

eval (EDiv e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	if v2 == 0
		then fail "division by zero"
		else return (VInt (div v1 v2))

eval (EMod e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VInt (mod v1 v2))

eval (ENot e) = do
	(VBool val) <- eval e
	return (VBool (not val))

eval (EAnd e1 e2) = do
	(VBool v1) <- eval e1
	(VBool v2) <- eval e2
	return (VBool (v1 && v2))

eval (EOr e1 e2) = do
	(VBool v1) <- eval e1
	(VBool v2) <- eval e2
	return (VBool (v1 || v2))

eval (EAss varName e) = do
	varLoc <- getVarLoc varName
	expVal <- eval e
	writeStore varLoc expVal
	return expVal

eval (EEq e1 e2) = do
	v1 <- eval e1
	v2 <- eval e2
	return (VBool (v1 == v2))

eval (ENeq e1 e2) = do
	v1 <- eval e1
	v2 <- eval e2
	return (VBool (not (v1 == v2)))

eval (ELt e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VBool (v1 < v2))

eval (EElt e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VBool (v1 <= v2))

eval (EGt e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VBool (v1 > v2))

eval (EEgt e1 e2) = do
	(VInt v1) <- eval e1
	(VInt v2) <- eval e2
	return (VBool (v1 >= v2))


execList :: [Stm] -> Interp ()
execList [] = return ()
execList (stm:stms) = do
	exec stm
	retDone <- readValue "@retDone"
	if retDone == (VBool True)
		then return ()
		else execList stms


makeLocs :: Integer -> Type -> Interp [Loc]

makeLocs 0 _ = return []

makeLocs n typeName = do
	val <- initDefValue typeName
	loc <- newLoc val
	locs <- makeLocs (n-1) typeName
	return (loc:locs)


exec :: Stm -> Interp ()

exec (SDec (DVar typeName (Ident x))) = do
	defValue <- initDefValue typeName
	newValue x defValue

exec (SDec (DInit _ (Ident x) exp)) = exec (SDec (DAuto (Ident x) exp))

exec (SDec (DAuto (Ident x) exp)) = do
	expVal <- eval exp
	newValue x expVal
	
exec (SDec (DArr typeName (Ident x) exp)) = do
	(VInt n) <- eval exp
	if n < 1
		then fail "array's size can't be below 1"
		else do
			locs <- makeLocs n typeName
			newValue x (VArr typeName locs)

exec (SDec (DStruct (Ident structName) decList)) = do
	oldEnv <- getEnv
	structEnv <- addDecsToStruct decList emptyEnv exec
	setEnv oldEnv
	newValue ("struct@" ++ structName) (VStruct (Ident structName) structEnv)

exec (SDec (DFunc retType (Ident funcName) paramList stmList)) = do
	oldEnv <- getEnv
	newValue funcName Void
	defRetVal <- initDefValue retType
	newValue "@retVal" defRetVal
	newValue "@retDone" (VBool False)
	funcEnv <- getEnv
	setValue funcName (VFunc retType paramList funcEnv stmList)
	setEnv oldEnv
	newValue funcName (VFunc retType paramList funcEnv stmList)

exec (SExp e) = do
	eval e
	return ()

exec (SBlock stmList) = do
	env <- getEnv
	execList stmList
	setEnv env

exec (SIf e stm) = do
	val <- eval e
	case val of
		(VBool False) -> return ()
		(VBool True) -> exec (SBlock [stm])

exec (SIfElse e ifStm elseStm) = do
	val <- eval e
	case val of
		(VBool False) -> exec (SBlock [elseStm])
		(VBool True) -> exec (SBlock [ifStm])

exec (SWhile e stm) = do
	val <- eval e
	case val of
		(VBool False) -> return ()
		(VBool True) -> do
			exec (SBlock [stm])
			retDone <- readValue "@retDone"
			if retDone == (VBool True)
				then return ()
				else exec (SWhile e stm)

exec (SFor dec exp lastExp stm) = do
	oldEnv <- getEnv
	exec (SDec dec)
	exec (SWhile exp (SBlock [stm, (SExp lastExp)]))
	setEnv oldEnv

exec SRet = do
	setValue "@retVal" Void
	setValue "@retDone" (VBool True)

exec (SRetExp e) = do
	val <- eval e
	setValue "@retVal" val
	setValue "@retDone" (VBool True)

exec (SPrint e) = do
	val <- eval e
	(VString currOutput) <- readStore (-1)
	writeStore (-1) (VString (currOutput ++ (show val)))

exec (SPrintLn e) = do
	val <- eval e
	(VString currOutput) <- readStore (-1)
	writeStore (-1) (VString (currOutput ++ (show val) ++ "\n"))
