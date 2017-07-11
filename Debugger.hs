module Debugger (debugger) where

import Data.Char
import Helpers

padlist s = [until((maximum(map length s)==).length)(++" ")x|x<-s]

evenOut (x,y)
 | length x > length y = (x,until((length x ==).length)([""]++)y)
 | otherwise = (until((length y ==).length)([""]++)x,y)

df :: ([String],[String]) -> IO ()
df (x,y) = putStr $ unlines $ (uncurry$ zipWith ((++).(++" "))) $ (\(x,y)->(padlist x,y)) $ evenOut (x++["^"], y++[""])

--- bf is the main version of the interpreter ---

bf :: [Char] -> ([Integer],[Integer],[Integer]) -> IO ([Integer],[Integer],[Integer])
bf []          (x,y,z)= return (x,y,z)
bf ('@':a:b:as) (x,y,z)
  | flag == "dh" = do print$length x; res <- bf as (x,y,z);return res
  | flag == "dc" = do putStrLn$x>>=(++" ").show; res <- bf as (x,y,z);return res
  | flag == "do" = do putStrLn$y>>=(++" ").show; res <- bf as (x,y,z);return res
  | flag == "dv" = do print$ head z; res <- bf as (x,y,z);return res
  | flag == "df" = do df (map show x,map show y); res <- bf as (x,y,z); return res
  | flag == "ac" = do putStrLn$map (chr.fromIntegral) x; res <- bf as (x,y,z);return res
  | flag == "ao" = do putStrLn$map (chr.fromIntegral) y; res <- bf as (x,y,z);return res
  | flag == "av" = do putStrLn$(:[]).chr.fromIntegral$head z; res <- bf as (x,y,z);return res
  | flag == "af" = do df (map ((:[]).chr.fromIntegral) x,map ((:[]).chr.fromIntegral) y); res <- bf as (x,y,z); return res
  where flag = a:b:[]
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
bf ('{':a)      t     = do res <- run (interior a) t;bf (exterior a) res
bf (_:a)        t     = bf a t

run :: [Char] -> ([Integer],[Integer],[Integer]) -> IO ([Integer],[Integer],[Integer])
run s ([],y,z)  = return ([],y,z)
run s (0:x,y,z) = return (0:x,y,z)
run s x         = do res <- (bf s x); run s res

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
clean ('@':a:b:xs) = '@':a:b:clean xs
clean (x:xs)
 | elem x "()[]<>{}" = x:(clean xs)
 | otherwise         = clean xs

debugger :: [Char] -> [Integer] -> IO ()
debugger s x
 | balance = do bf (clean s) (x,[],[]); return ()
 | otherwise = error "Unbalanced braces"
 where balance = balanced s
