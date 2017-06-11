import Interpreter
import System.Environment

main :: IO ()
main = do 
 cla <- getArgs
 source <- (readFile (head cla))
 print (brainflak source [read x :: Integer|x <- (tail cla)])
