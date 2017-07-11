import Expression
import Data.List

topadd :: [Expression] -> Expression -> [Expression]
topadd [] x = [x]
topadd (a:[]) x = [a+++x]
topadd (a:b) x = (a+++x):b

algebrize :: String -> ([Expression], [Expression], [Expression], Int, Int, Int, Bool) -> ([Expression], [Expression], [Expression], Int, Int, Int, Bool)
algebrize [] x = x
algebrize ('(':')':a) (lstack, rstack, scope,   ld, rd, sd, s) = algebrize a (lstack, rstack, (topadd scope (Expression 1 [] [] [])), ld, rd, sd, s)
algebrize ('<':'>':a) (lstack, rstack, scope,   ld, rd, sd, s) = algebrize a (rstack, lstack, scope, ld, rd, sd, not s)

algebrize ('{':'}':a) (l:lstack, rstack, scope, ld, rd, sd, s) = algebrize a (lstack, rstack, (topadd scope l), ld, rd, sd, s)
algebrize ('{':'}':a) ([], rstack, scope,   ld, rd, sd, True)  = algebrize a ([], rstack, (topadd scope (Expression 0 ((replicate ld 0)++[1]) [] [])), ld+1, rd, sd, True)
algebrize ('{':'}':a) ([], rstack, scope,   ld, rd, sd, False) = algebrize a ([], rstack, (topadd scope (Expression 0 [] ((replicate rd 0)++[1]) [])), ld, rd+1, sd, False)
algebrize ('(':a) (lstack, rstack, scope,   ld, rd, sd, s) = algebrize a (lstack, rstack, (Expression 0 [] [] []):scope, ld, rd, sd, s)
algebrize ('[':a) x = algebrize ('(':a) x
algebrize ('<':a) x = algebrize ('(':a) x
algebrize (')':a) (lstack, rstack, v:scope, ld, rd, sd, s) = algebrize a (v:lstack, rstack, (topadd scope v), ld, rd, sd,   s)
algebrize (')':a) (lstack, rstack,   scope, ld, rd, sd, s) = algebrize a ((Expression 0 [] [] ((replicate sd 0)++[1])):lstack, rstack, (topadd scope (Expression 0 [] [] ((replicate sd 0)++[1]))), ld, rd, sd+1, s)
algebrize (']':a) (lstack, rstack, v:scope, ld, rd, sd, s) = algebrize a (lstack, rstack, (topadd scope (neg v)), ld, rd, sd, s)
algebrize (']':a) (lstack, rstack,   scope, ld, rd, sd, s) = algebrize a (lstack, rstack, (topadd scope (Expression 0 [] [] ((replicate sd 0)++[-1]))), ld, rd, sd+1, s)
algebrize ('>':a) (lstack, rstack, _:scope, ld, rd, sd, s) = algebrize a (lstack, rstack, scope, ld, rd, sd,   s)
algebrize ('>':a) (lstack, rstack,   scope, ld, rd, sd, s) = algebrize a (lstack, rstack, scope, ld, rd, sd+1, s)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

base26 :: (Integral a) => a -> String
base26 x
 | x < 26 = (alphabet !! (fromIntegral x)):[]
 | otherwise = (alphabet !! (fromIntegral (mod x 26))):(base26$ (quot x 26)-1)

express :: Expression -> String
express e = '(':(concat$ intersperse "+" ([show$ rawvalue e]++['(':((show x)++'*':'l':base26 i)++")"|(x,i)<-zip (lstack e) [0,1..], x /= 0]++['(':((show x)++'*':'r':base26 i)++")"|(x,i)<-zip (rstack e) [0,1..], x /= 0]))++"))"

variblize :: Char -> [Expression] -> Int -> String
variblize side e 0 = 'x':[side]
variblize side e depth = (concat$ intersperse ":" [side:base26 x|x<-[0..depth-1]])++":x"++[side]

{-

functionize :: String -> ([Expression], [Expression], [Expression], Int, Int, Bool) -> String
functionize name (lstack, rstack, scope, ldepth, rdepth, True) = unlines [name++"::([Integer],[Integer],[Integer])->([Integer],[Integer],[Integer])",
 name++" ("++(variblize 'l' lstack ldepth)++',':(variblize 'r' rstack rdepth)++",xs)=("++(concat$ intersperse ":" (map express lstack++["xl"]))++","++(concat$ intersperse ":" (map express rstack++["xr"]))++",xs)",
 name++" ("++(variblize 'l' lstack ldepth)++",xr,xs)="++name++"("++(variblize 'l' lstack ldepth)++",xr++[0],xs)",
 name++" (xl,xr,xs)="++name++"((xl++[0]),xr,xs)"]
functionize name (lstack, rstack, scope, ldepth, rdepth, False) = unlines [name++"::([Integer],[Integer],[Integer])->([Integer],[Integer],[Integer])",
 name++" ("++(variblize 'l' lstack ldepth)++',':(variblize 'r' rstack rdepth)++",xs)=("++(concat$ intersperse ":" (map express lstack++["xr"]))++","++(concat$ intersperse ":" (map express rstack++["xl"]))++",xs)",
 name++" ("++(variblize 'l' lstack ldepth)++",xr,xs)="++name++"("++(variblize 'l' lstack ldepth)++",xr++[0],xs)",
 name++" (xl,xr,xs)="++name++"((xl++[0]),xr,xs)"]

-}
