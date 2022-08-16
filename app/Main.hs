module Main where

import Lib (someFunc)
import ClassyPrelude

main :: IO ()
main = do
  let a = mapFromList [('a','b')] :: Map Char Char
  print a
  someFunc
  putStrLn "Hello, Haskell!"
  
