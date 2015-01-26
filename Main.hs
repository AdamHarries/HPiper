{-# LANGUAGE PatternGuards #-}
module Main where
import System.IO
import System.Environment
import System.Eval.Haskell
import Mueval.Context (defaultPackages)
import Mueval.ArgsParse (Options(..))
import Mueval.Interpreter as MVL
import Language.Haskell.Interpreter (runInterpreter)
 
main = do
	--get the arguments from the command line - the haskell code
	args <- getArgs
	--get the (piped) inputs to the program
	piped <- getFullStdIn []
	--transform them to a string that looks like a haskell list
	let msPiped = map (\s -> "\"" ++ s ++ "\"") piped
	let sPiped = "[" ++ (foldl (\x y -> x++","++y) (head msPiped) (tail msPiped)) ++"]"
	--get the first arg, our haskell function as a string, and add the arguments
	let sHFunc = "(\\h -> "++(head args)++")"++sPiped
	--apply our function
	results <- interpretFunction (constructOption sHFunc)
   --final <- map (\x -> putStrLn x) (read results :: [String])
	printList (read results :: [String])

--get all lines from stdin until EOF, return as a list of files
getFullStdIn :: [String] -> IO [String]
getFullStdIn xs = do 
	eof <- hIsEOF stdin
	if eof 
		then do
			return xs
		else do 
			line <- getLine
			getFullStdIn (xs ++ [line])

interpretFunction :: Options -> IO String
interpretFunction opts = do r <- runInterpreter (interpreter opts)
                            case r of
                                 Left err -> do 
                                 		printInterpreterError err
                                 		return ""
                                 Right (e,et,val) -> do when (printType opts)
                                                             (putStr e >> putStrLn et)
                                                        return val
--print out a list of strings
printList :: [String] -> IO ()
printList [] = return ()
printList (s:ss) = do
		putStrLn s
		printList ss

constructOption :: String -> Options
constructOption expr = Options { expression = expr
						   , modules = Just myPackages
                           , timeLimit = 5
                           , user = ""
                           , loadFile = ""
                           , printType = False
                           --, typeOnly = False
                           , extensions = False
                           , namedExtensions = []
                           , noImports = False
                           , rLimits = False
                           , packageTrust = False
                           , trustedPackages = myPackages
                           , help = False }

myPackages :: [String]
myPackages = ["Prelude",
                  "ShowFun",
                  "Debug.SimpleReflect",
                  "Data.Function",
                  "Control.Applicative",
                  "Control.Arrow",
                  "Data.Array",
                  "Data.Bits",
                  "Data.Bool",
                  "Data.Char",
                  "Data.Complex",
                  "Data.Dynamic",
                  "Data.Either",
                  "Data.Eq",
                  "Data.Fixed",
                  "Data.Graph",
                  "Data.Int",
                  "Data.Ix",
                  "Data.List",
                  "Data.Maybe",
                  "Data.Monoid",
               "Data.Ord",
               "Data.Ratio",
               "Data.Tree",
               "Data.Tuple",
               "Data.Typeable",
               "Data.Word",
               "System.Random",
               "Text.PrettyPrint.HughesPJ",
               "Text.Printf"]
               --,
               --"Data.String.Utils"]