module Main where

import State

main :: IO ()
main = do
  putStrLn "Welcome to Haskell Exercises"
  putStrLn "FizzBuzz:"
  State.example
