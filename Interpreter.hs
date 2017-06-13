module Interpreter (brainflak) where

import Helpers

--- bf is the main version of the interpreter ---

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
run s (0:x,y,z) = (0:x,y,z)
run s x         = run s (bf s x)

--- xbf is a slower cycle counting version of bf ---

xbf :: [Char] -> ([Integer],[Integer],[Integer],Int) -> ([Integer],[Integer],[Integer],Int)
xbf _ (_,_,_,c) | c `seq` False = undefined
xbf []          (x,y,z,c)= (x,y,z,c)
xbf ('(':')':a) (x,y,z,c)= xbf a (x,y,((pop z+1):rest z),c+1)
xbf ('<':'>':a) (x,y,z,c)= xbf a (y,x,z,c+1)
xbf ('{':'}':a) (x,y,z,c)= xbf a ((rest x),y,(topadd z (pop x)),c+1)
xbf ('[':']':a) (x,y,z,c)= xbf a (x,y,(topadd z (toInteger (length x))),c+1)
xbf ('(':a)     (x,y,z,c)= xbf a (x,y,(0:z),c+1)
xbf ('<':a)     (x,y,z,c)= xbf a (x,y,(0:z),c+1)
xbf ('[':a)     (x,y,z,c)= xbf a (x,y,(0:z),c+1)
xbf (')':a) (x,y,(h:z),c)= xbf a ((h:x),y,(topadd z h),c+1)
xbf (']':a) (x,y,(h:z),c)= xbf a (x,y,(topadd z (-h)),c+1)
xbf ('>':a) (x,y,(_:z),c)= xbf a (x,y,z,c+1)
xbf ('{':a)      t     = xbf (exterior a) (xrun (interior a) t)
xbf (_:a)        t     = xbf a t

xrun :: [Char] -> ([Integer],[Integer],[Integer],Int) -> ([Integer],[Integer],[Integer],Int)
xrun s ([],y,z,c)  = ([],y,z,c)
xrun s (0:x,y,z,c) = (0:x,y,z,c)
xrun s (x,y,z,c)   = xrun s (xbf s (x,y,z,c+2))

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

brainflak :: [Char] -> [Integer] -> Bool -> ([Integer], Int)
brainflak s x c
 | balance && c       = (\(a,_,_,c) -> (a,c)) (xbf (clean s) (x,[],[],0))
 | balance && (not c) = (\(a,_,_)   -> (a,0)) ( bf (clean s) (x,[],[]  ))
 | otherwise  = error "Unbalanced braces."
 where balance = balanced s
