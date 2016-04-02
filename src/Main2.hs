{-# LANGUAGE RankNTypes #-}

module Main where

import           Control.Applicative
import           Data.Functor.Identity
import           Data.Monoid           (Any (Any), First (First), getAny,
                                        getFirst, (<>), Endo (Endo), appEndo)
import qualified Data.Traversable      as T

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

view :: Getting a s a -> s -> a
view l = getConst . l Const

over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set  :: Setting s t a b -> b -> s -> t
set l f = over l $ const f

_all' :: Eq a => a -> Traversal' [a] a
_all' ref f = T.traverse update
  where update old = if old == ref then f old else pure old

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))

preview
  :: Getting (First a) s a
  -> s
  -> Maybe a
preview l = getFirst . getConst . l (Const . First . Just)

has
  :: Getting Any s a
  -> s
  -> Bool
has l = getAny . getConst . l (const . Const $ Any True)

_head :: Traversal' [a] a
_head f [] = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs

main :: IO ()
main = do
  print $ set (_all' 0) 42 [ 0, 10, 0, 20, 0, 30 ]
  print $ toListOf (_all' 0) [0, 3, 0, 1]
  print $ toListOf (_all' 0) mempty
  print $ preview (_all' 0) [0, 3, 0, 1]
  print $ preview (_all' 0) mempty
  print $ has (_all' 0) [0, 3, 0, 1]
  print $ has (_all' 0) mempty
  print $ preview _head [1, 2, 3]
  print $ preview _head ([] :: [Int])
