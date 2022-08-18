module Main where

import ClassyPrelude
import qualified Lib as L

main :: IO ()
main = do
  let a = mapFromList [('a', 'b')] :: Map Char Char
  print a
  L.main
  putStrLn "Hello, Haskell!"
