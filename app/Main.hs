module Main where

import qualified Domain.Auth 
import ClassyPrelude

main :: IO ()
main = do
  let a = mapFromList [('a','b')] :: Map Char Char
  print a
  putStrLn "Hello, Haskell!"
  
