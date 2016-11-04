import Environment
import TypeChecker
import Interpreter

import AbsSi
import LexSi
import ParSi
import ErrM

import Data.Map

run :: Program -> Result ()
run (Prog stmList) = case runInterp (checkExecList stmList) initialEnv initialStore of
	Error str -> Error str
	Succ _ _ _ -> runInterp (execList stmList) initialEnv initialStore


envToString :: [(String, Loc)] -> Store -> [String]
envToString [] _ = []
envToString ((name, loc):env) store = (name ++ " = " ++ (show (store ! loc))):(envToString env store)


main = do
	input <- getContents
	let program = pProgram (myLexer input)
	case program of
		Bad str -> putStrLn ("Parse error: " ++ str)
		Ok prog -> case run prog of
			Error str -> putStrLn ("Error: " ++ str)
			Succ _ env store -> do
				let (VString output) = store ! (-1)
				if output == ""
					then putStrLn (unlines (envToString (toList env) store))
					else putStrLn output
