{-# OPTIONS_GHC -Wno-type-defaults #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  let foo = suffixes [1,2,3,4]
  print foo

suffixes :: [a] -> [[a]]
suffixes = foldr f [] where
  f y [] = [[y]]
  f y acc@(ys:_) = (y:ys) : acc
