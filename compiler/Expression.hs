module Expression (Expression(..),(+++),neg) where

data Expression = Expression {
 rawvalue :: Integer,
 lstack :: [Integer],
 rstack :: [Integer],
 sstack :: [Integer]
}

-- Modified from https://stackoverflow.com/a/21349554/ --

zipWD :: a -> [a] -> [a] -> [(a,a)]
zipWD da la lb = let len = max (length la) (length lb)
                     la' = la ++ (repeat da)
                     lb' = lb ++ (repeat da)
                 in take len $ zip la' lb'  

(+++)::Expression -> Expression -> Expression
e1 +++ e2 = Expression (rawvalue e1 + rawvalue e2) [a+b|(a,b)<- zipWD 0 (lstack e1) (lstack e2)] [a+b|(a,b)<- zipWD 0 (rstack e1) (rstack e2)] [a+b|(a,b)<- zipWD 0 (sstack e1) (sstack e2)]

neg::Expression -> Expression
neg e = Expression (-(rawvalue e)) [-x|x<-lstack e] [-x|x<-rstack e] [-x|x<-sstack e]

instance Show Expression where
 show e = '<':((show$ rawvalue e)++", "++(show$ lstack e)++", "++(show$ rstack e)++", "++(show$ sstack e)++">")
