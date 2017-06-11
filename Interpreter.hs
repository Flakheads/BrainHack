module Interpreter (brainflak) where

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

bf :: [Char] -> ([Integer],[Integer],[Integer]) -> ([Integer],[Integer],[Integer])
bf []          (x,y,z)= (x,y,z)
bf ('(':')':a) (x,y,z)= bf a (x,y,((pop z+1):rest z))
bf ('<':'>':a) (x,y,z)= bf a (y,x,z)
bf ('{':'}':a) (x,y,z)= bf a ((rest x),y,(topadd z (pop x)))
bf ('[':']':a) (x,y,z)= bf a (x,y,(topadd z (toInteger (length x))))
bf ('(':a)     (x,y,z)= bf a (x,y,(0:z))
bf ('<':a)     (x,y,z)= bf a (x,y,(0:z))
bf ('[':a)     (x,y,z)= bf a (x,y,(0:z))
bf (')':a) (x,y,(h:z))= bf a ((h:x),y,(topadd z h))
bf (']':a) (x,y,(h:z))= bf a (x,y,(topadd z (-h)))
bf ('>':a) (x,y,(_:z))= bf a (x,y,z)
bf ('{':a)      t     = bf (exterior a) (run (interior a) t)
bf (_:a)        t     = bf a t

run :: [Char] -> ([Integer],[Integer],[Integer]) -> ([Integer],[Integer],[Integer])
run s ([],y,z)  = ([],y,z)
run s ([0],y,z) = ([0],y,z)
run s (0:x,y,z) = (0:x,y,z)
run s x         = run s (bf s x)

bl :: [Char] -> [Char] -> Bool
bl [] [] = True
bl [] _  = False
bl ('(':x) y   = bl x (')':y) 
bl ('[':x) y   = bl x (']':y) 
bl ('<':x) y   = bl x ('>':y) 
bl ('{':x) y   = bl x ('}':y) 
bl  _ [] = False
bl (a:x) (b:y) = (a == b) && (bl x y)

balanced :: [Char] -> Bool
balanced x = bl x []

brainflak :: [Char] -> [Integer] -> [Integer]
brainflak s x
 | balanced source = (\(a,_,_) -> a) (bf source (x,[],[]))
 | otherwise  = error "Unbalanced braces."
 where source = [a|a <- s, elem a "()[]<>{}"]
