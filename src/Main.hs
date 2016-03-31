{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative
import           Control.Arrow         ((&&&))
import           Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 setter (a, b) = (\x -> (x, b)) <$> setter a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 setter (a, b) = (\x -> (a, x)) <$> setter b

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set setter val =
  let old = setter $ get val
  in set val <$> old

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f = \case
  Left s1 -> Left <$> l1 f s1
  Right s2 -> Right <$> l2 f s2

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (\a -> (a, f a))

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united = lens (const ()) const

view :: Lens s t a b -> s -> a
view lens s = x
  where
    Const x = lens Const s

over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

set :: Lens s t a b -> b -> s -> t
set l v = over l (const v)

_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0     = error "_abs: absolute value can't be negative"
      | otherwise = signum n * x

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
  where
    get s = ref
    set s new = map (\old -> if old == ref then new else old) s

main :: IO ()
main = do
  print $ view _1 (1, 2)
  print $ view _2 (1, 2)
  print $ view (_1 . _2) ((1, 4), 2)
  print $ over _1 (* 44) (1, 2)
  print $ over _1 (* 44) (1, 2)
  print $ over _abs (^ 2) (-10)
  print $ set (_all 0) 1337 [100, 200, 0, 300, 0, 400]
