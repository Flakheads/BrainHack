module Expression (Expression(..), (+++), neg) where

type Expression = [Integer]

-- Modified from https://stackoverflow.com/a/21349554/ --

zipWD :: a -> [a] -> [a] -> [(a,a)]
zipWD da la lb = let len = max (length la) (length lb)
                     la' = la ++ (repeat da)
                     lb' = lb ++ (repeat da)
                 in take len $ zip la' lb'  


(+++):: Expression -> Expression -> Expression
e1 +++ e2 = [ a+b | (a,b) <- (zipWD 0 e1  e2)]

neg :: Expression -> Expression
neg e = [-x | x <- e]
