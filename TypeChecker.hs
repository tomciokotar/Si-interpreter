module TypeChecker where

import qualified Data.Map as Map
import Environment
import AbsSi


checkParamTypes :: [ParamType] -> [Exp] -> Interp ()

checkParamTypes [] [] = return ()
checkParamTypes [] _ = fail "too many parameters"
checkParamTypes _ [] = fail "too few parameters"

checkParamTypes ((ParamTypeRef paramType):params) ((EVar varName):exps) = do
	varType <- checkVar varName
	if paramType == varType
		then checkParamTypes params exps
		else fail "parameter has invalid type"

checkParamTypes ((ParamTypeVal paramType):params) (exp:exps) = do
	expType <- checkExp exp
	if paramType == expType
		then checkParamTypes params exps
		else fail "parameter has invalid type"

checkParamTypes _ _ = fail "only variable can be passed by reference"


getExpTypes :: [Exp] -> Interp [Type]
getExpTypes [] = return []
getExpTypes (currExp:exps) = do
	expTypes <- getExpTypes exps
	currExpType <- checkExp currExp
	return (currExpType:expTypes)


checkStructElem :: Value -> String -> Interp Type
checkStructElem (VStruct (Ident structType) structEnv) elemName = case Map.lookup elemName structEnv of
	Nothing -> fail ("structure " ++ structType ++ " doesn't contain element " ++ elemName)
	Just locId -> do
		val <- readStore locId
		return (getType val)


allEqualTo :: Type -> [Type] -> Bool
allEqualTo _ [] = True
allEqualTo typeName (t:types) = if typeName == t
	then allEqualTo typeName types
	else False


checkVar :: Var -> Interp Type

checkVar (VarIdent (Ident x)) = do
	val <- readValue x
	return (getType val)

checkVar (VarArr arrName posExp) = do
	posType <- checkExp posExp
	arrType <- checkVar arrName
	case (arrType, posType) of
		(TArr typeName, TInt) -> return typeName
		_ -> fail "an array or it's position has invalid type"

checkVar (VarStruct structName (Ident elemName)) = do
	structType <- checkVar structName
	case structType of
		(TStruct typeIdent) -> do
			defStruct <- getDefValue (TStruct typeIdent)
			checkStructElem defStruct elemName
		_ -> fail "variable before . is not a structure"


checkExp :: Exp -> Interp Type

checkExp (EInt _) = return TInt

checkExp ETrue = return TBool

checkExp EFalse = return TBool

checkExp (EChar _) = return TChar

checkExp (EStr _) = return TString

checkExp (EVar varName) = checkVar varName

checkExp (ECall varName expList) = do
	varType <- checkVar varName
	case varType of
		(TFunc retType paramTypes) -> do
			checkParamTypes paramTypes expList
			return retType
		_ -> fail "trying to call variable that is not a function"

checkExp (EFunc retType paramList stmList) = do
	oldEnv <- getEnv
	setParams paramList
	defRetVal <- getDefValue retType
	newValue "@retVal" defRetVal
	newValue "@retDone" (VBool False)
	funcEnv <- getEnv
	checkExecList stmList
	setEnv oldEnv
	return (TFunc retType (getParamTypes paramList))

checkExp (EArr expList) = case expList of
	[] -> fail "trying to construct an empty array"
	_ -> do
		(typeName:typeList) <- getExpTypes expList
		if allEqualTo typeName typeList
			then return (TArr typeName)
			else fail "array's constructor contains elements of different types"

checkExp (EConc e1 e2) = do
	checkExp e1
	checkExp e2
	return TString

checkExp (EIntToChar e) = do
	expType <- checkExp e
	if expType == TInt
		then return TChar
		else fail "only int can be converted to char"

checkExp (ECharToInt e) = do
	expType <- checkExp e
	if expType == TChar
		then return TInt
		else fail "only char can be converted to int"

checkExp (EIncL varName) = do
	varType <- checkVar varName
	if varType == TInt
		then return TInt
		else fail "trying to use ++/-- operator on a non-int variable"

checkExp (EIncR varName) = checkExp (EIncL varName)

checkExp (EDecL varName) = checkExp (EIncL varName)

checkExp (EDecR varName) = checkExp (EIncL varName)

checkExp (EAddAss varName exp) = do
	varType <- checkVar varName
	expType <- checkExp exp
	case (varType, expType) of
		(TInt, TInt) -> return TInt
		_ -> fail "type error in equation (int expected)"

checkExp (ESubAss varName exp) = checkExp (EAddAss varName exp)

checkExp (EMulAss varName exp) = checkExp (EAddAss varName exp)

checkExp (EDivAss varName exp) = checkExp (EAddAss varName exp)

checkExp (EModAss varName exp) = checkExp (EAddAss varName exp)

checkExp (EAdd e1 e2) = do
	t1 <- checkExp e1
	t2 <- checkExp e2
	case (t1, t2) of
		(TInt, TInt) -> return TInt
		_ -> fail "type error in equation (int expected)"

checkExp (ESub e1 e2) = checkExp (EAdd e1 e2)

checkExp (EMul e1 e2) = checkExp (EAdd e1 e2)

checkExp (EDiv e1 e2) = checkExp (EAdd e1 e2)

checkExp (EMod e1 e2) = checkExp (EAdd e1 e2)

checkExp (ENot e) = do
	expType <- checkExp e
	if expType == TBool
		then return TBool
		else fail "type error in equation (bool expected)"

checkExp (EAnd e1 e2) = do
	t1 <- checkExp e1
	t2 <- checkExp e2
	case (t1, t2) of
		(TBool, TBool) -> return TBool
		_ -> fail "type error in equation (bool expected)"

checkExp (EOr e1 e2) = checkExp (EAnd e1 e2)

checkExp (EAss varName e) = do
	expType <- checkExp e
	varType <- checkVar varName
	if varType == expType
		then return expType
		else fail "trying to assign an expression of incorrect type"

checkExp (EEq e1 e2) = do
	t1 <- checkExp e1
	t2 <- checkExp e2
	if t1 == t2
		then return TBool
		else fail "trying to compare expressions of different types"

checkExp (ENeq e1 e2) = checkExp (EEq e1 e2)

checkExp (ELt e1 e2) = do
	t1 <- checkExp e1
	t2 <- checkExp e2
	case (t1, t2) of
		(TInt, TInt) -> return TBool
		_ -> fail "type error in equation (int expected)"

checkExp (EElt e1 e2) = checkExp (ELt e1 e2)

checkExp (EGt e1 e2) = checkExp (ELt e1 e2)

checkExp (EEgt e1 e2) = checkExp (ELt e1 e2)


checkExecList :: [Stm] -> Interp ()

checkExecList [] = return ()

checkExecList (stm:stms) = do
	checkExec stm
	checkExecList stms


getParamNames :: [Param] -> [String]
getParamNames [] = []
getParamNames ((Param _ (Ident paramName)):params) = (paramName:(getParamNames params))


checkParamNames :: String -> [String] -> Interp ()

checkParamNames _ [] = return ()

checkParamNames funcName (paramName:params) =
	if (funcName == paramName) || (elem paramName params)
		then fail ("parameter's name " ++ paramName ++ " is not unique")
		else checkParamNames funcName params


setParams :: [Param] -> Interp ()

setParams [] = return ()

setParams ((Param typeName (Ident paramName)):params) = case typeName of
	(ParamTypeVal t) -> do
		defValue <- getDefValue t
		newValue paramName defValue
		setParams params
	(ParamTypeRef t) -> do
		defValue <- getDefValue t
		newValue paramName defValue
		setParams params


checkExec :: Stm -> Interp ()

checkExec (SDec (DVar typeName (Ident varName))) = do
	defValue <- getDefValue typeName
	newValue varName defValue

checkExec (SDec (DInit typeName (Ident varName) exp)) = do
	expType <- checkExp exp
	if typeName == expType
		then checkExec (SDec (DVar typeName (Ident varName)))
		else fail "trying to initialize variable with expression of wrong type"

checkExec (SDec (DAuto (Ident varName) exp)) = do
	expType <- checkExp exp
	checkExec (SDec (DVar expType (Ident varName)))

checkExec (SDec (DArr typeName (Ident varName) exp)) = do
	expType <- checkExp exp
	if expType == TInt
		then checkExec (SDec (DVar (TArr typeName) (Ident varName)))
		else fail "array's size must be an int"

checkExec (SDec (DStruct (Ident structName) decList)) = do
	oldEnv <- getEnv
	structEnv <- addDecsToStruct decList emptyEnv checkExec
	setEnv oldEnv
	newValue ("struct@" ++ structName) (VStruct (Ident structName) structEnv)

checkExec (SDec (DFunc retType (Ident funcName) paramList stmList)) = do
	checkParamNames funcName (getParamNames paramList)
	oldEnv <- getEnv
	setParams paramList
	newValue funcName Void
	defRetVal <- getDefValue retType
	newValue "@retVal" defRetVal
	newValue "@retDone" (VBool False)
	funcEnv <- getEnv
	setValue funcName (VFunc retType paramList funcEnv stmList)
	checkExecList stmList
	setEnv oldEnv
	newValue funcName (VFunc retType paramList funcEnv stmList)

checkExec (SExp e) = do
	checkExp e
	return ()

checkExec (SBlock stmList) = do
	env <- getEnv
	checkExecList stmList
	setEnv env

checkExec (SIf e stm) = do
	expType <- checkExp e
	case expType of
		TBool -> checkExec (SBlock [stm])
		_ -> fail "expression type in if/while should be bool"

checkExec (SIfElse e ifStm elseStm) = do
	checkExec (SIf e ifStm)
	checkExec (SBlock [elseStm])

checkExec (SWhile e stm) = checkExec (SIf e stm)

checkExec (SFor dec exp lastExp stm) = do
	oldEnv <- getEnv
	checkExec (SDec dec)
	checkExec (SWhile exp (SBlock [stm, (SExp lastExp)]))
	setEnv oldEnv

checkExec SRet = do
	retVal <- readValue "@retVal"
	if retVal == Void
		then return ()
		else fail "trying to return void where impossible"
	
checkExec (SRetExp e) = do
	expType <- checkExp e
	retVal <- readValue "@retVal"
	if expType == (getType retVal)
		then return ()
		else fail "trying to return value of incorrect type"

checkExec (SPrint e) = do
	checkExp e
	return ()

checkExec (SPrintLn e) = checkExec (SPrint e)
