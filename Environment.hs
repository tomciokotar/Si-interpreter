module Environment where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import AbsSi
import Control.Monad

type Loc = Integer
type Env = Map.Map String Loc
type Store = Map.Map Loc Value

data Value =
	VInt Integer
  |	VBool Bool
  |	VChar Char
  |	VString String
  |	VArr Type [Loc]
  |	VStruct Ident Env
  |	VFunc Type [Param] Env [Stm]
  |	Void
	deriving (Eq)

instance Show Value where
	show val = case val of
		VInt n -> show n
		VBool True -> "true"
		VBool False -> "false"
		VChar c -> show c
		VString str -> str
		VArr _ _ -> "array"
		VStruct _ _ -> "struct"
		VFunc _ _ _ _ -> "function"
		Void -> "void"

data Result a =
	Error String
  |	Succ a Env Store
	deriving (Show)

newtype Interp a = Interp { runInterp :: Env -> Store -> Result a }

instance Monad Interp where
	return x = Interp (\env -> (\store -> Succ x env store))
	
	i >>= instr = Interp (\env -> (\store -> case runInterp i env store of
		Error msg			-> Error msg
		Succ x env' store'	-> runInterp (instr x) env' store'))
	
	fail msg = Interp (\_ -> (\_ -> Error msg))


emptyEnv :: Env
emptyEnv = Map.empty

initialEnv :: Env
initialEnv = Map.fromList [("@retVal", 1), ("@retDone", 2)]


emptyStore :: Store
emptyStore = Map.singleton 0 (VInt 1)

initialStore :: Store
initialStore = Map.fromList [(-1, (VString "")), (0, (VInt 3)), (1, Void), (2, (VBool False))]


readEnv :: String -> Interp Loc
readEnv str = Interp (\env -> (\store -> case Map.lookup str env of
	Nothing   -> Error ("object " ++ str ++ " doesn't exist")
	Just loc  -> Succ loc env store))


writeEnv :: String -> Loc -> Interp ()
writeEnv str loc = Interp (\env -> (\store -> case Map.lookup str env of
	Just _ -> if List.isPrefixOf "struct@" str
		then Error "name of struct can't be redefined"
		else Succ () (Map.insert str loc env) store
	Nothing -> Succ () (Map.insert str loc env) store))


getEnv :: Interp Env
getEnv = Interp (\env -> (\store -> Succ env env store))


setEnv :: Env -> Interp ()
setEnv env = Interp (\_ -> (\store -> Succ () env store))


readStore :: Loc -> Interp Value
readStore loc = Interp (\env -> (\store -> case Map.lookup loc store of
	Nothing    -> Error "object uninitialized"
	Just val   -> Succ val env store))


writeStore :: Loc -> Value -> Interp ()
writeStore loc val = Interp (\env -> (\store -> case store Map.! 0 of
	VInt firstFreeLoc -> if (loc < firstFreeLoc) || (loc == -1)
		then Succ () env (Map.insert loc val store)
		else Error "trying to write to uninitialized loc"))


readValue :: String -> Interp Value
readValue str = do
	loc <- readEnv str
	readStore loc


setValue :: String -> Value -> Interp ()
setValue str val = do
	loc <- readEnv str
	writeStore loc val


newValue :: String -> Value -> Interp ()
newValue str val = do
	loc <- newLoc val
	writeEnv str loc


newLoc :: Value -> Interp Loc
newLoc val = Interp (\env -> (\store -> let (Just (VInt id)) = Map.lookup 0 store in
	Succ id env (Map.insert id val (Map.insert 0 (VInt (id + 1)) store))))


getParamTypes :: [Param] -> [ParamType]
getParamTypes [] = []
getParamTypes ((Param paramType _):params) = (paramType:(getParamTypes params))


getType :: Value -> Type
getType val = case val of
	VInt _ -> TInt
	VBool _ -> TBool
	VChar _ -> TChar
	VString _ -> TString
	VArr typeName _ -> TArr typeName
	VStruct typeName _ -> TStruct typeName
	VFunc retType params _ _ -> TFunc retType (getParamTypes params)
	Void -> TVoid


getDefParams :: [ParamType] -> Interp [Param]
getDefParams [] = return []
getDefParams (typeName:typeNames) = do
	params <- getDefParams typeNames
	return ((Param typeName (Ident ("param" ++ (show (length typeNames))))):params)


getDefValue :: Type -> Interp Value
getDefValue t = case t of
	TInt -> return (VInt 0)
	TBool -> return (VBool False)
	TChar -> return (VChar (Char.chr 0))
	TString -> return (VString "")
	TArr typeName -> return (VArr typeName [])
	TStruct (Ident typeName) -> readValue ("struct@" ++ typeName)
	TFunc retType paramTypes -> do
		params <- getDefParams paramTypes
		retVal <- initDefValue retType
		loc <- newLoc retVal
		return (VFunc retType params (Map.singleton "@retVal" loc) [])
	TVoid -> return Void


copyStructEnv :: [String] -> Env -> Interp Env
copyStructEnv [] structEnv = return structEnv
copyStructEnv (elemName:elems) structEnv = do
	val <- readStore (structEnv Map.! elemName)
	loc <- newLoc val
	copyStructEnv elems (Map.insert elemName loc structEnv)


initDefValue :: Type -> Interp Value

initDefValue (TStruct (Ident structName)) = do
	(VStruct typeName structEnv) <- readValue ("struct@" ++ structName)
	newEnv <- copyStructEnv (Map.keys structEnv) structEnv
	return (VStruct typeName newEnv)

initDefValue typeName = getDefValue typeName


addDecsToStruct :: [Dec] -> Env -> (Stm -> Interp ()) -> Interp Env
addDecsToStruct [] env _ = return env
addDecsToStruct (currDec:decs) env exec = do
	exec (SDec currDec)
	case currDec of
		DVar _ (Ident x) -> addToStruct decs x env exec
		DInit _ (Ident x) _ -> addToStruct decs x env exec
		DAuto (Ident x) _ -> addToStruct decs x env exec
		DArr _ (Ident x) _ -> addToStruct decs x env exec
		DFunc _ (Ident x) _ _ -> addToStruct decs x env exec


addToStruct :: [Dec] -> String -> Env -> (Stm -> Interp ()) -> Interp Env
addToStruct decs x env exec = case Map.lookup x env of
	Just _ -> fail ("multiple declaration of " ++ x ++ " in a struct")
	Nothing -> do
		loc <- readEnv x
		addDecsToStruct decs (Map.insert x loc env) exec
