{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude ( IO
               , Integer
               , Bool
               , Eq
               , putStrLn
               , reverse
               , head
               , foldl
               , show
               , map
               , filter
               , zip
               , return
               , tail
               , (.)
               , ($)
               , (<>)
               , (+)
               , (==))
import Data.List (genericLength, group)
import Data.Tuple (fst, snd)

myLast :: [a] -> a
myLast = head . reverse

myButLast :: [a] -> a
myButLast = head . tail . reverse

elementAt :: [a] -> Integer -> a
elementAt xs a = head $
  map snd $ 
    filter (((==) a). fst) $ 
      zip [1 .. genericLength xs] xs

myLength :: [a] -> Integer
myLength = myLength' 0
  where myLength' a [] = a
        myLength' a xs = myLength' (a + 1) $ tail xs

myReverse :: [a] -> [a]
myReverse = myReverse' []
  where myReverse' a [] = a
        myReverse' a xs = myReverse' ([head xs] <> a) $ tail xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = (==) xs $ myReverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten = flatten' []
  where flatten' _ (Elem x) = [x]
        flatten' a (List []) = a
        flatten' a (List xs) = flatten'' $ head xs
          where tailXs = tail xs
                flatten'' (Elem x) = flatten' (a <> [x]) $ List tailXs
                flatten'' (List []) = flatten' a $ List tailXs
                flatten'' (List ys) = flatten' a $ List $ [head ys] <> [List (tail ys)] <> tailXs

compress :: Eq a => [a] -> [a]
compress = map head . group

main :: IO ()
main = do
  foldl (\ a b -> a <> b) (return ()) $ 
    map putStrLn $
      [ (<>) "Problem 1 - myLast [1, 2, 3, 4]: " $ 
          show $ myLast [1, 2, 3, 4]
      , (<>) "Problem 1 - myLast ['x', 'y', 'z']: " $ 
          show $ myLast ['x', 'y', 'z']
      , (<>) "Problem 2 - myButLast [1, 2, 3, 4]: " $
          show $ myButLast [1, 2, 3, 4]
      , (<>) "Problem 2 - myButLast ['a' .. 'z']: " $
          show $ myButLast ['a' .. 'z']
      , (<>) "Problem 3 - elementAt [1, 2, 3] 2: " $
          show $ elementAt [1, 2, 3] 2
      , (<>) "Problem 3 - elementAt \"haskell\" 5: " $
          show $ elementAt "haskell" 5
      , (<>) "Problem 4 - myLength [123, 456, 789]: " $
          show $ myLength [123, 456, 789]
      , (<>) "Problem 4 - myLength \"Hello, world!\": " $
          show $ myLength "Hello, world!"
      , (<>) "Problem 5 - myReverse \"A man, a plan, a canal, panama!\": " $
          show $ myReverse "A man, a plan, a canal, panama!"
      , (<>) "Problem 5 - myReverse [1, 2, 3, 4]: " $
          show $ myReverse [1, 2, 3, 4]
      , (<>) "Problem 6 - isPalindrome [1, 2, 3]: " $
          show $ isPalindrome [1, 2, 3]
      , (<>) "Problem 6 - isPalindrome \"madamimadam\": " $
          show $ isPalindrome "madamimadam"
      , (<>) "Problem 6 - isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]: " $
          show $ isPalindrome [1, 2, 4, 8, 16, 8, 4, 2, 1]
      , (<>) "Problem 7 - flatten (Elem 5): " $
          show $ flatten (Elem 5)
      , (<>) "Problem 7 - flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]): " $
          show $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
      , (<>) "Problem 7 - flatten ((List []) :: NestedList Bool): " $
          show $ flatten ((List []) :: NestedList Bool)
      , (<>) "Problem 8 - compress \"aaaabccaadeeee\": " $
          show $ compress "aaaabccaadeeee"]
