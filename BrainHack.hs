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

parseFlags :: [String] -> [String] -> [String]
parseFlags [] a = a
parseFlags (('-':'-':x):xs) a = parseFlags xs (x:a)
parseFlags (('-':x):xs)     a = parseFlags xs ((split x) ++ a)

processInput :: [String] -> [Integer]
processInput (('"':ys):xs)
 | (last ys) == '"' = map (toInteger.ord) (init ys)
 | otherwise = error "Invalid input"
processInput (('\'':ys):xs)
 | (last ys) == '\'' = map (toInteger.ord) (init ys)
 | otherwise = error "Invaid input"
processInput x = [read a :: Integer | a <- x]

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
  "   -x, --cycles\t\t\tPrints the number of cycles elapsed upon termination."]

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
               (return (head args))
             else
               (readFile (head args))
             )
           )
 let formatOutput = if (elem "a" flags)||(elem "ascii-out" flags) then (\x -> (chr.fromInteger) x:[]) else displayInteger
 let cycles = (elem "x" flags)||(elem "cycles" flags)
 putStr$ (if exiting then
           ""
          else
           (\ (a,b) -> (concat (map formatOutput a)) ++ (if cycles then ("\nCycles: " ++ show b) else "")) (if (elem "m" flags)||(elem "mini" flags) then (miniflak source (processInput (tail args)) cycles) else (brainflak source (processInput (tail args)) cycles))
          )
