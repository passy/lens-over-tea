{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

data Present = Wrapped Present | Toy
  deriving (Show, Eq)

unwrapped :: Traversal' Present Present
unwrapped f (Wrapped x) = Wrapped <$> f x
unwrapped f Toy = f Toy

set' :: Present -> Identity Present
set' t = Identity (Wrapped t)

get' :: forall b. Present -> Const [Present] b
get' t = Const [t]

main :: IO ()
main = do
  print . fmap (unwrapped get') . unwrapped set' $ Toy
  print . getCompose . unwrapped (Compose . fmap get' . set') $ Toy
