

import Control.Applicative


test f xs = putStrLn $ show xs ++ " = " ++ show (f xs)


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome . init) xs

isPalindrome1 = and . (map ((==) <$> fst <*> snd)) . (zip <*> reverse)

isPalindrome2 xs = (half xs) == (half $ reverse xs)
    where half ys = take (length ys `div` 2) ys

isPalindrome3 = (==) <$> half <*> (half . reverse)
    where half = (flip take) <*> (flip div 2 . length)

main = do
    putStrLn "------ Problem 06 ------"
    putStrLn "Check is list is a palindrome"
    test isPalindrome "madam"
    test isPalindrome "mada"
    test isPalindrome1 [1,2,3]
    test isPalindrome1 [1,2,3,2,1]
    test isPalindrome2 [1,2,3]
    test isPalindrome2 [1,2,3,2,1]
    test isPalindrome3 [1,2,3]
    test isPalindrome3 [1,2,3,2,1]
    putStrLn "------------------------"
