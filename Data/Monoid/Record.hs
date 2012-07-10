{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Data.Monoid.Record where

import Data.Monoid                        ( Any(..), Last(..), Monoid(..) )

add :: Monoid m => (a -> m) -> (m -> a -> a) -> m -> a -> a
add getter setter x y = setter (getter y `mappend` x) y

class Monoid m => Wrapper m a | m -> a where
    wrap :: a -> m
    addP :: (b -> m) -> (m -> b -> b) -> a -> b -> b
    addP getter setter = add getter setter . wrap

instance Wrapper Any Bool where
    wrap = Any

instance Wrapper (Last a) a where
    wrap = Last . return

instance Wrapper [a] [a] where
    wrap = id

