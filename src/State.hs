{-# LANGUAGE InstanceSigs #-}

module State where

import Control.Monad (mapM_)
import qualified Data.DList as DL

-- Exercise: Chapter 23. State

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f m = State $ \s -> case runState m s of
    (a, s') -> (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ (,) a

  (<*>) :: State s (a -> b) -> State s a -> State s b
  mf <*> ma = State $ \s -> case runState mf s of
    (f, s') -> runState (fmap f ma) s'

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  ma >>= f = State $ \s -> case runState ma s of
    (x, s') -> runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

-- EXAMPLE ---

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

solveFizzBuzz :: [Integer] -> DL.DList String
solveFizzBuzz list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  put $ DL.snoc xs $ fizzBuzz n

example :: IO ()
example =
  mapM_ putStrLn $ solveFizzBuzz [1 .. 100]
