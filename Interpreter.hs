module Interpreter (brainflak, lstack, cycles) where

data InterpState = InterpState {
    lstack :: [Integer],
    rstack :: [Integer],
    scope  :: [Integer],
    cycles :: !Int
}

pop :: (Integral a) => [a] -> a
pop [] = 0
pop (x:_) = x

rest :: (Integral a) => [a] -> [a]
rest [] = []
rest (_:x) = x

topadd :: [Integer] -> Integer -> [Integer]
topadd [] x = [x]
topadd (a:[]) x = [a+x]
topadd (a:b) x = (a+x):b

ir :: [Char] -> Integer -> [Char]
ir x 0 = ""
ir ('{':x) y = "{" ++ (ir x (y+1))
ir ('}':x) y = "}" ++ (ir x (y-1))
ir (a:x)   y = [a] ++ (ir x   y  )

interior :: [Char] -> [Char]
interior x = init (ir x 1)

ex :: [Char] -> Integer -> [Char]
ex x 0 = x
ex ('{':x) y = ex x (y+1)
ex ('}':x) y = ex x (y-1)
ex  (a:x)  y = ex x y

exterior :: [Char] -> [Char]
exterior x = ex x 1

bf :: [Char] -> InterpState -> InterpState
bf []          t= t
bf ('(':')':a) t= bf a (InterpState (lstack t) (rstack t) ((pop (scope t)+1):rest (scope t)) (cycles t+1))
bf ('<':'>':a) t= bf a (InterpState (rstack t) (lstack t) (scope t) (cycles t+1))
bf ('{':'}':a) t= bf a (InterpState (rest (lstack t)) (rstack t) (topadd (scope t) (pop (lstack t))) (cycles t+1))
bf ('[':']':a) t= bf a (InterpState (lstack t) (rstack t) (topadd (scope t) (toInteger (length (rstack t)))) (cycles t+1))
bf ('(':a)     t= bf a (InterpState (lstack t) (rstack t) (0:(scope t)) (cycles t+1))
bf ('<':a)     t= bf a (InterpState (lstack t) (rstack t) (0:(scope t)) (cycles t+1))
bf ('[':a)     t= bf a (InterpState (lstack t) (rstack t) (0:(scope t)) (cycles t+1))
bf (')':a)     t= bf a (InterpState (head (scope t):(lstack t)) (rstack t) (topadd  (tail (scope t)) (head (scope t))) (cycles t+1))
bf (']':a)     t= bf a (InterpState (lstack t) (rstack t) (topadd (tail (scope t)) (-(head (scope t)))) (cycles t+1))
bf ('>':a)     t= bf a (InterpState (lstack t) (rstack t) (tail (scope t)) (cycles t+1))
bf ('{':a)     t= bf (exterior a) (run (interior a) t)
bf (_:a)       t= bf a t

run :: [Char] -> InterpState -> InterpState
run s x
 | ((pop (lstack x)) == 0) = x
 |        otherwise        = run s (bf s x)

bl :: [Char] -> [Char] -> Bool
bl [] [] = True
bl [] _  = False
bl ('(':x) y   = bl x (')':y) 
bl ('[':x) y   = bl x (']':y) 
bl ('<':x) y   = bl x ('>':y) 
bl ('{':x) y   = bl x ('}':y) 
bl  (a:x) []
 | elem a ")]>}" = False
 | otherwise     = bl x []
bl (a:x) (b:y)
 | elem a ")]>}" = (a == b) && (bl x y)
 | otherwise     = bl x (b:y)

balanced :: [Char] -> Bool
balanced x = bl x []

clean :: [Char] -> [Char]
clean [] = []
clean ('#':'{':xs) = clean (exterior xs)
clean (x:xs)
 | elem x "()[]<>{}" = x:(clean xs)
 | otherwise         = clean xs

brainflak :: [Char] -> [Integer] -> InterpState
brainflak s x
 | balanced s = bf (clean s) (InterpState x [] [] 0)
 | otherwise  = error "Unbalanced braces."
