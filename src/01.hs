
module Problem01 where

import Data.List


test f what = do
    putStrLn $ (show what) ++ " = " ++ (show $ f what)


last1 = last
last2 = head . reverse
last3 = foldl1 (flip const)
last4 = foldr1 (flip const)

last5 :: [a] -> a
last5 [] = error "empty list"
last5 [x] = x
last5 (x:xs) = last5 xs

last6 xs = xs !! (length xs - 1)

main = do
    putStrLn "------ Problem 01 ------"
    putStrLn "Find last list element"
    test last1 "hello"
    test last2 "hello"
    test last3 [1,2,3]
    test last4 [1,2,3,4]
    test last5 [1,2,3,4,5]
    test last6 [1,2,3,4,5,6]
    test last5 ([] :: [Int])
    putStrLn "------------------------"
