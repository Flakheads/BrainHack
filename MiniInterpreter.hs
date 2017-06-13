module MiniInterpreter (miniflak) where

import Helpers

bf :: [Char] -> ([Integer],[Integer]) -> ([Integer],[Integer])
bf []          (x,z)= (x,z)
bf ('(':')':a) (x,z)= bf a (x,((pop z+1):rest z))
bf ('{':'}':a) (x,z)= bf a ((rest x),(topadd z (pop x)))
bf ('(':a)     (x,z)= bf a (x,(0:z))
bf ('[':a)     (x,z)= bf a (x,(0:z))
bf (')':a) (x,(h:z))= bf a ((h:x),(topadd z h))
bf (']':a) (x,(h:z))= bf a (x,(topadd z (-h)))
bf ('{':a)      t     = bf (exterior a) (run (interior a) t)
bf (_:a)        t     = bf a t

run :: [Char] -> ([Integer],[Integer]) -> ([Integer],[Integer])
run s ([],z)  = ([],z)
run s (0:x,z) = (0:x,z)
run s x         = run s (bf s x)

--- xbf is a version of bf that counts cycles ---

xbf :: [Char] -> ([Integer],[Integer],Int) -> ([Integer],[Integer],Int)
xbf _ (_,_,c) | c `seq` False = undefined
xbf []          (x,z,c)= (x,z,c)
xbf ('(':')':a) (x,z,c)= xbf a (x,((pop z+1):rest z),c+1)
xbf ('{':'}':a) (x,z,c)= xbf a ((rest x),(topadd z (pop x)),c+1)
xbf ('(':a)     (x,z,c)= xbf a (x,(0:z),c+1)
xbf ('[':a)     (x,z,c)= xbf a (x,(0:z),c+1)
xbf (')':a) (x,(h:z),c)= xbf a ((h:x),(topadd z h),c+1)
xbf (']':a) (x,(h:z),c)= xbf a (x,(topadd z (-h)),c+1)
xbf ('{':a)      t     = xbf (exterior a) (xrun (interior a) t)
xbf (_:a)        t     = xbf a t

xrun :: [Char] -> ([Integer],[Integer],Int) -> ([Integer],[Integer],Int)
xrun s ([],z,c)  = ([],z,c)
xrun s (0:x,z,c) = (0:x,z,c)
xrun s (x,z,c)   = xrun s (xbf s (x,z,c+2))

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
clean ('[':']':xs) = clean xs
clean (x:xs)
 | elem x "()[]{}" = x:(clean xs)
 | otherwise         = clean xs

miniflak :: [Char] -> [Integer] -> Bool -> ([Integer],Int)
miniflak s x c
 | balance &&      c  = (\(a,_,c) -> (a,c)) (xbf (clean s) (x,[],0))
 | balance && (not c) = (\(a,_)   -> (a,0))  (bf (clean s) (x,[]  ))
 | otherwise  = error "Unbalanced braces."
 where balance = balanced s
