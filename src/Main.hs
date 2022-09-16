{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude ( IO
               , Integer
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
import Data.List (genericLength)
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
          show $ myLength "Hello, world!"]
