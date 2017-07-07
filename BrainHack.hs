import Interpreter
import MiniInterpreter
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
  "   -e, --execute\t\tExecutes the first commandline argument as Brain-Flak code.",
  "   -h, --help\t\t\tPrints this menu and exits.",
  "   -m, --mini\t\t\tRun in an interpreter optimized for miniflak,",
  "   -v, --version\t\tPrints the version number of the interpreter and exits.",
  "   -x, --cycles\t\t\tPrints the number of cycles elapsed upon termination.",
  "   -f, --file\t\t\tInput is read from file instead of commandline.  (Provide the filename as the last argument instead of input)"]

displayInteger :: Integer -> String
displayInteger x = show x ++ " "

main :: IO ()
main = do 
 cla <- getArgs
 let flags = parseFlags (filter isFlag cla) []
 let args  = filter (not.isFlag) cla
 putStr  (if ((elem "h" flags)||(elem "help" flags)) then (helpMenu++"\n") else [])
 putStr  (if ((elem "v" flags)||(elem "version" flags)) then (version++"\n") else [])
 let exiting = any (`elem`["h","help","v","version"]) flags
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
 putStr$ (if exiting then
           ""
          else
           (\ (a,b) -> (concat (map formatOutput a)) ++ (if cycles then ("\nCycles: " ++ show b) else "")) (if (elem "m" flags)||(elem "mini" flags) then (miniflak source (processInput input) cycles) else (brainflak source (processInput input) cycles))
          )
