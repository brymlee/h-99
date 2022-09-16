{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude ( IO
               , putStrLn
               , reverse
               , head
               , foldl
               , show
               , map
               , return
               , id
               , (.)
               , ($)
               , (<>))

myLast :: [a] -> a
myLast = head . reverse

main :: IO ()
main = do
  foldl (\ a b -> a <> b) (return ()) $ 
    map putStrLn $
      [ (<>) "Problem 1 - myLast [1, 2, 3, 4]: " $ 
          show $ myLast [1, 2, 3, 4]
      , (<>) "Problem 1 - myLast ['x', 'y', 'z']: " $ 
          show $ myLast ['x', 'y', 'z']]
