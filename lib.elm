module Lib where

cartProd : [a] -> [b] -> [(a,b)]
cartProd xs ys = if ((length xs) == 0 || (length ys) == 0)
                then []
                else map (\y -> (head xs, y)) ys ++ cartProd (tail xs) ys

range : Int -> [Int]
range n = if (n < 0)
          then [0]
          else n :: range (n-1)
