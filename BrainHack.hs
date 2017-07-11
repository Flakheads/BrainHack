import Interpreter
import MiniInterpreter
import Debugger
import System.Environment
import Data.List
import Data.Char

version :: String
version = "BrainHack 0.0"

isFlag :: String -> Bool
isFlag ('-':c:x)
 | elem c "0123456789" = False
 | otherwise           = True
isFlag x = False

split :: String -> [String]
split [] = []
split (x:xs) = (x:[]):(split xs)

parseFile :: String -> [String]
parseFile [] = [""]
parseFile ('"':xs) = parseString xs '"'
parseFile ('\'':xs) = parseString xs '\''
parseFile (' ':xs) = "":parseFile xs
parseFile ('\t':xs) = "":parseFile xs
parseFile ('\n':xs) = "":parseFile xs
parseFile (s:xs) = (\(a:xa)->(a++[s]):xa)$ parseFile xs

parseString :: String -> Char -> [String]
parseString (s:xs) a
 | s == a = "":parseFile xs
 | otherwise = (\(a:xa)->(a++[s]):xa)$ parseString xs a

parseFlags :: [String] -> [String] -> [String]
parseFlags [] a = a
parseFlags (('-':'-':x):xs) a = parseFlags xs (x:a)
parseFlags (('-':x):xs)     a = parseFlags xs ((split x) ++ a)

processInput :: [String] -> [Integer]
processInput (('"':ys):xs)
 | (last ys) == '"' = (map (toInteger.ord) (init ys))++processInput xs
 | otherwise = error "Invalid input"
processInput (('\'':ys):xs)
 | (last ys) == '\'' = (map (toInteger.ord) (init ys))++processInput xs
 | otherwise = error "Invalid input"
processInput ([]:xs) = processInput xs
processInput (x:xs)
 | (elem (head x) "0123456789-") && (all (isDigit)$tail x) = (read x :: Integer):processInput xs
 | otherwise = error "Invalid input"
processInput [] = []

helpMenu :: String
helpMenu = unlines ["",
  "Brain-Flak Haskell Interpreter",
  "Usage:",
  "\tBrainHack [options] source_file args...",
  "",
  "   -a, --ascii-out\t\tOutputs as ascii character codes.",
  "   -d, --debug\t\t\tEnters into Debug mode.",
  "   -e, --execute\t\tExecutes the first commandline argument as Brain-Flak code.",
  "   -f, --file\t\t\tInput is read from file instead of commandline.  (Provide the filename as the last argument instead of input)",
  "   -h, --help\t\t\tPrints this menu and exits.",
  "   -H, --debug-help\t\tPrints the debug help menu and exits.",
  "   -m, --mini\t\t\tRun in an interpreter optimized for miniflak,",
  "   -v, --version\t\tPrints the version number of the interpreter and exits.",
  "   -x, --cycles\t\t\tPrints the number of cycles elapsed upon termination."]

debugMenu :: String
debugMenu = unlines $ ["",
  "To use debug mode run the interpreter with the flag -d.",
  "Debug flags are two character codes prepended with a @ symbol.  Each one will output some information.",
  "Here are their functionalities:",
  "   ac:\tOutputs the current stack as a String.",
  "   dc:\tOutputs the current stack as a list of Integers.",
  "   ao:\tOutputs the off stack as a String.",
  "   do:\tOutputs the off stack as a list of Integers.",
  "   af:\tOutputs both stacks as Strings.",
  "   df:\tOutputs both stacks as lists of Integers.",
  "   av:\tOutputs the current scope as a Character.",
  "   dv:\tOutputs the current scope as an Integer.",
  "   dh:\tOutputs the height of the current stack as an Integer."]

displayInteger :: Integer -> String
displayInteger x = show x ++ " "

main :: IO ()
main = do 
 cla <- getArgs
 let flags = parseFlags (filter isFlag cla) []
 let args  = filter (not.isFlag) cla
 putStr  (if ((elem "h" flags)||(elem "help" flags)) then (helpMenu++"\n") else [])
 putStr  (if ((elem "H" flags)||(elem "debug-help" flags)) then (debugMenu++"\n") else [])
 putStr  (if ((elem "v" flags)||(elem "version" flags)) then (version++"\n") else [])
 let exiting = any (`elem`["h","help","v","version","H","debug-help"]) flags
 source <- (if exiting then
             (return "")
           else
             (if ((elem "e" flags)||(elem "execute" flags)) then
               (return (head args)) --TODO ensure head
             else
               (readFile (head args)) --TODO ensure head
             )
           )
 let formatOutput = if (elem "a" flags)||(elem "ascii-out" flags) then (\x -> (chr.fromInteger) x:[]) else displayInteger
 let cycles = (elem "x" flags)||(elem "cycles" flags)
 input <- if ((elem "f" flags)||(elem "file" flags)) then (do file <- readFile$ last args; return $ parseFile file) else (return$ tail args)
 (if exiting then
         (return ())
        else
          (if ((elem "d" flags)||(elem "debug" flags)) then
           (debugger source (processInput input))
          else
           (putStr $ (\ (a,b) -> (concat (map formatOutput a)) ++ (if cycles then ("\nCycles: " ++ show b) else "")) (if (elem "m" flags)||(elem "mini" flags) then (miniflak source (processInput input) cycles) else (brainflak source (processInput input) cycles)))
          )
        )
