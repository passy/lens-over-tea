{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.Traversable as T

type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

view :: Getting s a -> s -> a
view l = getConst . l Const

over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set  :: Setting s t a b -> b -> s -> t
set l f = over l $ const f

_all' :: Eq a => a -> AppLens' [a] a
_all' ref f = T.traverse update
  where update old = if old == ref then f old else pure old

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf l = getConst . l (\x -> Const [x])

main :: IO ()
main = do
  print $ set (_all' 0) 42 [ 0, 10, 0, 20, 0, 30 ]
  print $ toListOf (_all' 0) [0, 3, 0, 1]
  print $ toListOf (_all' 0) mempty
