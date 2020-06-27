{-# LANGUAGE RankNTypes #-}
module LambdaCalculus.Recursion where

import Prelude (Int(..), (+), (-), (==), ($), Maybe(..), pure, return)
import Data.Map
import Control.Monad.State.Lazy (State(..), get, put, evalState)

fix :: ( a -> a ) -> a
fix f = f (fix f)

succ :: Int -> Int
succ = (+ 1)
pred :: Int -> Int
pred x = x - 1

add' :: (Int -> Int -> Int) -> Int -> Int -> Int
add' f x y = if y == 0
                then x
                else f (succ x) (pred y)

add :: Int -> Int -> Int
add = fix add'

mult' :: (Int -> Int -> Int) -> Int -> Int -> Int
mult' f x y = if x == 0
                then 0
                else add x (f x (pred y))

mult :: Int -> Int -> Int
mult = fix mult'

fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' f n = f (n - 1) + f (n - 2)

fib'' :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int
fib'' f acc accPred 0 = acc
fib'' f acc accPred c = f (acc + accPred) acc (c - 1)

type IntMemo = Map Int Int

fibMemo' :: (Int -> State IntMemo Int) -> Int -> State IntMemo Int
fibMemo' _ 0 = pure 1
fibMemo' _ 1 = pure 1
fibMemo' f n = do
  state <- get
  case lookup n state of
    Nothing -> do
      res1 <- fibWithState 1
      res2 <- fibWithState 2
      return $ res1 + res2
    Just res -> pure res
  where
    fibWithState num = do
      res <- f (n - num)
      s <- get
      put $ insert (n - num) res s
      return res


fib = fix fib'
fibFast = fibFast' 1 0
  where fibFast' = fix fib''
fibMemo = fix fibMemo'

-- $> evalState (fibMemo 34) empty
