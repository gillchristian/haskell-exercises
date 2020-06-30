{-# LANGUAGE InstanceSigs #-}

module Lib
  ( State (..),
  )
where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> (f $ fst $ g s, s)

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State f) <*> (State g) = State $ \s -> (fst (f s) $ fst $ g s, s)

instance Monad (State s) where
  return :: a -> State s a
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State g) >>= f = State $ \s -> (fst $ runState (f $ fst $ g s) $ s, s)
