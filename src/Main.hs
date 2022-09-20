{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude ( IO
               , Integer
               , Show
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
import Data.List (genericLength, group, repeat, genericTake, concatMap)
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

pack :: Eq a => [a] -> [[a]]
pack = pack' []
  where pack' a [] = reverse a
        pack' [] xs = pack' [[head xs]] $ tail xs
        pack' a xs = if (head (head a)) == (head xs) then
                       pack' ([head a <> [head xs]] <> (tail a)) $ tail xs
                     else
                       pack' ([[head xs]] <> a) $ tail xs

encode :: Eq a => [a] -> [(Integer, a)]
encode = (map (\ a -> (genericLength a, head a))) . group

data ListItem a = Multiple Integer a | Single a deriving Show

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = (map (\ (a, b) -> if a == 1 then
                                     Single b
                                   else 
                                     Multiple a b)) . encode 

decodeModified :: [ListItem a] -> [a]
decodeModified = (foldl (<>) []) . (map decodeModified')
  where decodeModified' (Multiple a b) = genericTake a $ repeat b
        decodeModified' (Single a) = genericTake 1 $ repeat a

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = encodeDirect' []
  where encodeDirect' a [] = reverse $ map (\ (b, c) -> if b == 1 then
                                                          Single c
                                                        else
                                                          Multiple b c) a
        encodeDirect' [] xs = encodeDirect' [(1, head xs)] $ tail xs
        encodeDirect' a xs = if snd (head a) == head xs then
                               encodeDirect' ([(fst (head a) + 1, head xs)] <> (tail a)) $ tail xs
                             else
                               encodeDirect' ([(1, head xs)] <> a) $ tail xs

dupli :: [a] -> [a]
dupli = concatMap $ \ x -> [x, x]

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
          show $ compress "aaaabccaadeeee"
      , (<>) "Problem 9 - pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']: " $
          show $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
      , (<>) "Problem 10 - encode \"aaaabccaadeeee\": " $
          show $ encode "aaaabccaadeeee"
      , (<>) "Problem 11 - encodeModified \"aaaabccaadeeee\": " $
          show $ encodeModified "aaaabccaadeeee"
      , (<>) "Problem 12 - decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']: " $
          show $ decodeModified [ Multiple 4 'a', Single 'b', Multiple 2 'c'
                                , Multiple 2 'a', Single 'd', Multiple 4 'e']
      , (<>) "Problem 13 - encodeDirect \"aaaabccaadeeee\": " $
          show $ encodeDirect "aaaabccaadeeee"
      , (<>) "Problem 14 - dupli [1, 2, 3]: " $
          show $ dupli [1, 2, 3]]
