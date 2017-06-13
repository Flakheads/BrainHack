module Helpers (pop,rest,topadd,interior,exterior) where

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
